% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


-module(cassim_security).


-export([
    get_security/1,
    get_security/2,
    get_security_doc/1
]).

-export([
    set_security/2,
    set_security/3
]).

-export([
    migrate_security_props/2,
    validate_security_doc/1
]).


-include_lib("couch/include/couch_db.hrl").


get_security(DbName) ->
    get_security(DbName, [?ADMIN_CTX]).


get_security(DbName, Options) when is_binary(DbName) ->
    case cassim:is_active() of
        true ->
            UserCtx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
            Doc = get_security_doc(DbName),
            {SecProps} = couch_doc:to_json_obj(Doc, []),
            ok = check_is_member(UserCtx, SecProps),
            {proplists:delete(<<"_id">>, SecProps)};
        false ->
            fabric:get_security(DbName, Options)
    end;
get_security(Db, Options) ->
    get_security(couch_db:name(Db), Options).


get_security_doc(DbName) when is_binary(DbName) ->
    RetryCnt = config:get_integer("cassim", "get_security_retries", 3),
    get_security_doc(DbName, RetryCnt).


get_security_doc(DbName, RetryCnt) when RetryCnt =< 0 ->
    couch_log:error(
        "Exhausted retry limit loading security doc for db ~s", [DbName]
    ),
    throw({retries_limit_exhaused, "Exhaused security doc retry limit"});
get_security_doc(DbName0, RetryCnt) ->
    DbName = mem3:dbname(DbName0),
    MetaId = cassim_metadata_cache:security_meta_id(DbName),
    case cassim_metadata_cache:load_meta(MetaId) of
        undefined ->
            SecProps = fabric:get_security(DbName),
            try migrate_security_props(DbName, SecProps) of
                {ok, SecDoc} ->
                    couch_stats:increment_counter(
                        [cassim, security_migration, success]),
                    SecDoc
            catch conflict ->
                couch_stats:increment_counter(
                    [cassim, security_migration, conflict]),
                get_security_doc(DbName0, RetryCnt-1)
            end;
        {error, Error} ->
            throw(Error);
        SecProps ->
            couch_doc:from_json_obj(SecProps)
    end.


set_security(DbName, SecProps) ->
    set_security(DbName, SecProps, [?ADMIN_CTX]).


set_security(DbName0, #doc{}=SecDoc0, Options) when is_binary(DbName0) ->
    DbName = mem3:dbname(DbName0),
    MetaId = cassim_metadata_cache:security_meta_id(DbName),
    SecDoc = SecDoc0#doc{id=MetaId},
    UserCtx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
    MetaDbName = cassim_metadata_cache:metadata_db(),
    {ok, MetaDb} = couch_db:clustered_db(MetaDbName, ?ADMIN_USER),
    cassim:verify_admin_role(UserCtx),
    ok = validate_security_doc(SecDoc),
    {Status, Etag, {Body0}} =
        chttpd_db:update_doc(MetaDb, MetaId, SecDoc, Options),
    Body = {proplists:delete(<<"_id">>, Body0)},
    ok = cassim_metadata_cache:cleanup_old_docs(MetaId),
    {Status, Etag, Body};
set_security(Db, SecDoc, Options) ->
    set_security(couch_db:name(Db), SecDoc, Options).


migrate_security_props(DbName0, {SecProps}) ->
    DbName = mem3:dbname(DbName0),
    MetaId = cassim_metadata_cache:security_meta_id(DbName),
    SecDoc = #doc{id=MetaId, body={SecProps}},
    MetaDbName = cassim_metadata_cache:metadata_db(),
    {ok, MetaDb} = couch_db:clustered_db(MetaDbName, ?ADMIN_USER),
    %% Better way to construct a new #doc{} with the rev?
    {_, _, {Body}} = chttpd_db:update_doc(MetaDb, MetaId, SecDoc, [?ADMIN_CTX]),
    Rev = proplists:get_value(rev, Body),
    SecProps1 = lists:keystore(<<"_rev">>, 1, SecProps, {<<"_rev">>, Rev}),
    SecDoc1 = couch_doc:from_json_obj({SecProps1}),
    {ok, SecDoc1}.


validate_security_doc(#doc{body={SecProps}}) ->
    Admins = couch_util:get_value(<<"admins">>, SecProps, {[]}),
    % we fallback to readers here for backwards compatibility
    Members = couch_util:get_value(<<"members">>, SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})),
    ok = validate_names_and_roles(Admins),
    ok = validate_names_and_roles(Members),
    ok.


validate_names_and_roles({Props}) when is_list(Props) ->
    lists:foreach(
        fun(Name) ->
            validate_roles_list(Name, couch_util:get_value(Name, Props, []))
        end,
        [<<"names">>, <<"roles">>]
    ).


validate_roles_list(Field, Roles) when is_list(Roles) ->
    case lists:all(fun(X) -> is_binary(X) end, Roles) of
        true -> ok;
        false -> throw(binary_to_list(Field) ++ " must be a JSON list of strings")
    end;
validate_roles_list(Field, _Roles) ->
    throw(binary_to_list(Field) ++ " must be a JSON list of strings").


check_is_member(UserCtx, SecProps) ->
    {ok, FakeDb} = couch_db:clustered_db(<<"foo">>, UserCtx, SecProps),
    couch_db:check_is_member(FakeDb).

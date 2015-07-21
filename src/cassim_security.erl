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


get_security(#db{name=DbName}, Options) ->
    get_security(DbName, Options);
get_security(DbName, Options) ->
    case cassim:is_active() of
        true ->
            UserCtx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
            Doc = get_security_doc(DbName),
            {SecProps} = couch_doc:to_json_obj(Doc, []),
            check_is_member(UserCtx, SecProps),
            {proplists:delete(<<"_id">>, SecProps)};
        false ->
            fabric:get_security(DbName, Options)
    end.


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


set_security(#db{name=DbName0}, #doc{}=SecDoc0, Options) ->
    DbName = mem3:dbname(DbName0),
    MetaId = cassim_metadata_cache:security_meta_id(DbName),
    SecDoc = SecDoc0#doc{id=MetaId},
    UserCtx = couch_util:get_value(user_ctx, Options, #user_ctx{}),
    MetaDbName = cassim_metadata_cache:metadata_db(),
    MetaDb = #db{name=MetaDbName, user_ctx=?ADMIN_USER},
    cassim:verify_admin_role(UserCtx),
    ok = validate_security_doc(SecDoc),
    {Status, Etag, {Body0}} =
        chttpd_db:update_doc(MetaDb, MetaId, SecDoc, Options),
    Body = {proplists:delete(<<"_id">>, Body0)},
    ok = cassim_metadata_cache:cleanup_old_docs(MetaId),
    {Status, Etag, Body}.


migrate_security_props(DbName0, {SecProps}) ->
    DbName = mem3:dbname(DbName0),
    MetaId = cassim_metadata_cache:security_meta_id(DbName),
    SecDoc = #doc{id=MetaId, body={SecProps}},
    MetaDbName = cassim_metadata_cache:metadata_db(),
    MetaDb = #db{name=MetaDbName, user_ctx=?ADMIN_USER},
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


check_is_admin(#user_ctx{name=Name,roles=Roles}, SecProps) ->
    {Admins} = get_admins(SecProps),
    AdminRoles = [<<"_admin">> | couch_util:get_value(<<"roles">>, Admins, [])],
    AdminNames = couch_util:get_value(<<"names">>, Admins,[]),
    case AdminRoles -- Roles of
    AdminRoles -> % same list, not an admin role
        case AdminNames -- [Name] of
        AdminNames -> % same names, not an admin
            throw({unauthorized, <<"You are not a db or server admin.">>});
        _ ->
            ok
        end;
    _ ->
        ok
    end.


check_is_member(#user_ctx{name=Name,roles=Roles}=UserCtx, SecProps) ->
    case (catch check_is_admin(UserCtx, SecProps)) of
    ok -> ok;
    _ ->
        {Members} = get_members(SecProps),
        ReaderRoles = couch_util:get_value(<<"roles">>, Members,[]),
        WithAdminRoles = [<<"_admin">> | ReaderRoles],
        ReaderNames = couch_util:get_value(<<"names">>, Members,[]),
        case ReaderRoles ++ ReaderNames of
        [] -> ok; % no readers == public access
        _Else ->
            case WithAdminRoles -- Roles of
            WithAdminRoles -> % same list, not an reader role
                case ReaderNames -- [Name] of
                ReaderNames -> % same names, not a reader
                    couch_log:debug("Not a reader: UserCtx ~p "
                                    " vs Names ~p Roles ~p",
                                    [UserCtx, ReaderNames, WithAdminRoles]),
                    throw({unauthorized, <<"You are not authorized to access this db.">>});
                _ ->
                    ok
                end;
            _ ->
                ok
            end
        end
    end.


get_admins(SecProps) ->
    couch_util:get_value(<<"admins">>, SecProps, {[]}).


get_members(SecProps) ->
    % we fallback to readers here for backwards compatibility
    couch_util:get_value(<<"members">>, SecProps,
        couch_util:get_value(<<"readers">>, SecProps, {[]})).

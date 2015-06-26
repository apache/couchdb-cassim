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

-module(cassim).


-export([
    is_enabled/0,
    is_active/0,
    metadata_db_exists/0
]).

-export([
    is_server_admin/1,
    verify_is_admin/1,
    verify_is_server_admin/1
]).

-export([
    has_admin_role/1,
    verify_admin_role/1
]).

-export([
    get_security/1,
    get_security/2,
    set_security/2,
    set_security/3
]).


-include_lib("couch/include/couch_db.hrl").


is_active() ->
    is_enabled() andalso metadata_db_exists().


is_enabled() ->
    config:get_boolean("cassim", "enable", false).


metadata_db_exists() ->
    cassim_metadata_cache:metadata_db_exists().


verify_is_admin(#httpd{}=Req) ->
    chttpd:verify_is_server_admin(Req).


is_server_admin(#httpd{user_ctx=#user_ctx{roles=Roles}}) ->
    lists:member(server_admin, Roles).


verify_is_server_admin(#httpd{}=Req) ->
    case is_server_admin(Req) of
        true -> ok;
        false -> throw({unauthorized, <<"You are not a server admin.">>})
    end.


has_admin_role(#user_ctx{roles=Roles}) ->
    has_admin_role(Roles);
has_admin_role(Roles) when is_list(Roles) ->
    lists:member(<<"_admin">>, Roles) orelse lists:member(server_admin, Roles).


verify_admin_role(Obj) ->
    case has_admin_role(Obj) of
        true -> ok;
        false -> throw({unauthorized, <<"You are not a db or server admin.">>})
    end.


get_security(DbName) ->
    cassim_security:get_security(DbName).


get_security(DbName, Options) ->
    cassim_security:get_security(DbName, Options).


set_security(DbName, SecObj) ->
    cassim_security:set_security(DbName, SecObj).


set_security(DbName, SecObj, Options) ->
    cassim_security:set_security(DbName, SecObj, Options).

-module(cerberus).

%% Include files

%% Exported functions

-export([
    start/0,
    add_realm/2,
    authenticate/3,
    title/1,
    groups/1,
    users/1,
    restart/0
]).

-export_type([
    realm/0,
    realm_option/0,
    authentication_result/0,
    group_result/0,
    user_result/0
]).

-type realm() :: atom().
-type realm_option() :: {'directory', directory_spec()} | {'title', string()}.
-type directory_spec() :: {module(), directory_options()}.
-type directory_options() :: term().
-type authentication_result() :: 'ok' | {'reject', term()} | {'error', term()}.
-type group_result() :: {'ok', maps:map(Group :: string(), Uids :: [string()])} | {'error', term()}.
-type user_result() :: {'ok', maps:map(Uid :: string(), Name :: string())} | {'error', term()}.

%% API

%% @doc Start cerberus application.

start() ->
    application:start(cerberus).

-spec add_realm(realm(), [realm_option()]) -> {'ok', pid()}.

%% @doc Add new realm with specified name and spec.

add_realm(Realm, Options) ->
    cerberus_realms_sup:add(Realm, Options).

-spec authenticate(realm(), string() | binary(), string() | binary()) -> 'ok' | {'reject', term()} | {'error', term()}.

%% @doc Authenticate a user.
%%
%% Realm is a realm name either defined in cerberus application config or added in runtime
%% via {@link add_realm/2}.

authenticate(Realm, User, Password) ->
    case cerberus_realm:authenticate(Realm, to_string(User), to_string(Password)) of
        {ok, _Group} -> ok;
        Other -> Other
    end.

-spec title(realm()) -> string().

%% @doc Realm title
%%
%% Realm is a realm name either defined in cerberus application config or added in runtime
%% via {@link add_realm/2}.

title(Realm) ->
    cerberus_realm:title(Realm).

-spec groups(realm()) -> group_result().

%% @doc Get LDAP groups and their member UIDs.
%%
%% Realm is a realm name either defined in cerberus application config or added in runtime
%% via {@link add_realm/2}.

groups(Realm) ->
    cerberus_realm:groups(Realm).

%% @doc Get LDAP users and their member UIDs.
%%
%% Realm is a realm name either defined in cerberus application config or added in runtime
%% via {@link add_realm/2}.

users(Realm) ->
    cerberus_realm:users(Realm).

%% @doc Restart cerberus with new configs
%%
%% bin/server rpc cerberus restart

restart() ->
    aplib:reload_config(),
    application:stop(cerberus),
    application:start(cerberus).

%% Local functions

to_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_string(List) when is_list(List) ->
    List.

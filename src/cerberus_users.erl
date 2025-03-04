-module(cerberus_users).

-behaviour(cerberus_directory).

%% Include files

%% Exported functions

-export([
    authenticate/3,
    groups/1,
    users/1
]).

-export_type([config_data/0]).

%% Configuration data: list of {User, Password} tuples

-type config_data() :: [{string(), string()}].

%% API

-spec authenticate(config_data(), string(), string()) -> cerberus:authentication_result().

authenticate(Data, User, Password) ->
    case lists:keyfind(User, 1, Data) of
        {User, Password} -> ok;
        {User, _} -> {reject, invalid_password};
        false -> {reject, unknown_user}
    end.

-spec groups(config_data()) -> cerberus:group_result().

groups(_) ->
    {error, not_implemented}.

-spec users(config_data()) -> cerberus:user_result().

users(UserList) ->
    UserMap = maps:map(fun(U, _P) -> U end, maps:from_list(UserList)),
    {ok, UserMap}.

%% Local functions

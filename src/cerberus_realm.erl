-module(cerberus_realm).

-behaviour(gen_server).

%% Include files

%% Exported functions

-export([
    start_link/1,
    authenticate/3,
    title/1,
    groups/1,
    users/1
]).

%% gen_server callbacks

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% API

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

authenticate(Ref, User, Password) ->
    request(Ref, {authenticate, User, Password}).

request(Ref, Request) ->
    gen_server:call(realm_pid(Ref), {request, Request}).

title(Ref) ->
    gen_server:call(realm_pid(Ref), title).

groups(Ref) ->
    request(Ref, groups).

users(Ref) ->
    request(Ref, users).

realm_pid(Pid) when is_pid(Pid) -> Pid;
realm_pid(Realm) when is_atom(Realm) -> cerberus_realms_sup:find(Realm).

-record(state, {
    title :: string(),
    directory :: module(),
    directory_options :: term()
}).

%% gen_server callbacks

init(Options) ->
    Title = proplists:get_value(title, Options),
    {Directory, DirectoryOptions} = proplists:get_value(directory, Options),
    State = #state{title = Title, directory = Directory, directory_options = DirectoryOptions},
    {ok, State}.

handle_call({request, Request}, From, State) ->
    #state{directory = Directory, directory_options = DirectoryOptions} = State,
    erlang:spawn_link(fun() -> process_request(Request, From, Directory, DirectoryOptions) end),
    {noreply, State};
handle_call(title, _From, State) ->
    #state{title = Title} = State,
    {reply, Title, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Local functions

process_request(Request, From, Directory, Options) ->
    Reply = execute_request(Request, Directory, Options),
    gen_server:reply(From, Reply).

execute_request({authenticate, User, Password}, Directory, Options) ->
    try
        Directory:authenticate(Options, User, Password)
    catch
        error:Error:Stacktrace ->
            error_logger:error_msg("Cerberus: error reported by authentication handler ~p: ~p~n~p~n", [Directory, Error, Stacktrace]),
            {error, Error}
    end;

execute_request(groups, Directory, Options) ->
    try
        Directory:groups(Options)
    catch
        error:Error:Stacktrace ->
            error_logger:error_msg("Cerberus: error reported by groups handler ~p: ~p~n~p~n", [Directory, Error, Stacktrace]),
            {error, Error}
    end;

execute_request(users, Directory, Options) ->
    try
        Directory:users(Options)
    catch
        error:Error:Stacktrace ->
            error_logger:error_msg("Cerberus: error reported by users handler ~p: ~p~n~p~n", [Directory, Error, Stacktrace]),
            {error, Error}
    end.

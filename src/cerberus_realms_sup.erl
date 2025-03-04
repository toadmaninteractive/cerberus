-module(cerberus_realms_sup).

-behaviour(supervisor).

%% Include files

%% Exported functions

-export([
    start_link/0,
    find/1,
    add/2,
    which_realms/0
]).

%% Supervisor callbacks

-export([
    init/1
]).

%% API

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec find(cerberus:realm()) -> pid().

find(Realm) ->
    Children = supervisor:which_children(?MODULE),
    case lists:keyfind(Realm, 1, Children) of
        {_,Pid,_,_} when is_pid(Pid) -> Pid;
        {_,restarting,_,_} -> erlang:error({cerberus, realm_restarting, Realm});
        {_,undefined,_,_} -> erlang:error({cerberus, realm_down, Realm});
        false -> erlang:error({cerberus, realm_unknown, Realm})
    end.

-spec add(cerberus:realm(), [cerberus:realm_option()]) -> {'ok', pid()}.

add(Realm, Spec) ->
    supervisor:start_child(?MODULE, child_spec(Realm, Spec)).

-spec which_realms() -> [cerberus:realm()].

which_realms() ->
    Children = supervisor:which_children(?MODULE),
    [Realm || {Realm,_,_,_} <- Children].

%% Supervisor callbacks

init([]) ->
    Realms = application:get_env(cerberus, realms, []),
    ChildSpecs = [ child_spec(N, S) || {N, S} <- Realms ],
    {ok, {{one_for_one, 3, 10}, ChildSpecs}}.

%% Local functions

child_spec(Realm, Options) ->
    {Realm, {cerberus_realm, start_link, [Options]}, permanent, 2000, worker, [cerberus_realm]}.

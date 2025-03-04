-module(cerberus_app).

-behaviour(application).

%% Include files

%% Exported functions

-export([
    start/2,
    stop/1
]).

%% API

start(_StartType, _StartArgs) ->
    cerberus_realms_sup:start_link().

stop(_State) ->
    ok.

%% Local functions

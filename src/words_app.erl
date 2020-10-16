%%%-------------------------------------------------------------------
%% @doc words public API
%% @end
%%%-------------------------------------------------------------------

-module(words_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:start(emongo),
    words_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

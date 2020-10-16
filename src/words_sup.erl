%%%-------------------------------------------------------------------
%% @doc words top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(words_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Reader = {reader_id, {reader_server, start_link, []},
        Restart, Shutdown, Type, [reader_server]},

    Translator = {translator_id, {translator_server, start_link, []},
        Restart, Shutdown, Type, [translator_server]},

    WordsKeeper = {words_keeper_id, {words_keeper, start_link, []},
        Restart, Shutdown, Type, [words_keeper]},

    MongoDBChild = #{id => dbserverid,
        start => {db, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [db]},

    {ok, {SupFlags, [Reader,Translator,WordsKeeper,MongoDBChild]}}.
%% internal functions

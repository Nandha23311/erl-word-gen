%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2020 2:22 PM
%%%-------------------------------------------------------------------
-module(words_keeper).
-author("nandakumar").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 3000). % One minute

-record(words_keeper_state, {words=[],count=0}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #words_keeper_state{}} | {ok, State :: #words_keeper_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    io:format("~n Words Keeper : I started save words !! "),
    {ok, #words_keeper_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #words_keeper_state{}) ->
    {reply, Reply :: term(), NewState :: #words_keeper_state{}} |
    {reply, Reply :: term(), NewState :: #words_keeper_state{}, timeout() | hibernate} |
    {noreply, NewState :: #words_keeper_state{}} |
    {noreply, NewState :: #words_keeper_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #words_keeper_state{}} |
    {stop, Reason :: term(), NewState :: #words_keeper_state{}}).
handle_call(_Request, _From, State = #words_keeper_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #words_keeper_state{}) ->
    {noreply, NewState :: #words_keeper_state{}} |
    {noreply, NewState :: #words_keeper_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #words_keeper_state{}}).

handle_cast({save_to_db, WordList}, State) ->
    save_to_db(WordList),
    {noreply, State};

handle_cast(_Request, State = #words_keeper_state{}) ->
    io:format("~n not found"),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #words_keeper_state{}) ->
    {noreply, NewState :: #words_keeper_state{}} |
    {noreply, NewState :: #words_keeper_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #words_keeper_state{}}).

handle_info(_Info, State = #words_keeper_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #words_keeper_state{}) -> term()).
terminate(_Reason, _State = #words_keeper_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #words_keeper_state{},
    Extra :: term()) ->
    {ok, NewState :: #words_keeper_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #words_keeper_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_to_db(Words) when Words=:= [] ->
    ok;

save_to_db(Words)->
    Words1 = lists:sort(Words),
    AllWorkers = create_workers(100),

    lists:foldl(
        fun(Each, Counter) ->
            WorkerPid = next_worker_pid(AllWorkers, Counter),

            % sending the line to the next worker
            WorkerPid ! {self(), Each},

            Counter + 1
        end, 0, Words1),

    lists:foreach(
        fun(Pid) ->
            Pid ! {self(), eof}
        end, AllWorkers),

    % lets wait fo the worker responses
    SavedWordsCount = util:waiting_response(0, length(AllWorkers)),
    io:format("~n Word Keeper : ~p Words Saved to DB ", [SavedWordsCount]).


% this function will create N number of workers. Each worker will start with the function
% `init/0` defined in the `module multi_process_worker`.
create_workers(NumberOfWorkers) ->
    [spawn(words_keeper_worker, init, []) || _X <- lists:seq(1, NumberOfWorkers)].


next_worker_pid(AllWorkerPIDs, Counter) ->
    Index = Counter rem length(AllWorkerPIDs),
    lists:nth(Index + 1, AllWorkerPIDs).

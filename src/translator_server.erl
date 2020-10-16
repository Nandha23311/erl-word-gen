%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2020 5:39 PM
%%%-------------------------------------------------------------------
-module(translator_server).
-author("nandakumar").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 3000). % One minute
-define(HTTP_TIMEOUT_MSEC, 10000).
-define(HTTP_CONNECT_TIMEOUT_MSEC, 100000).
-define(TYPE, "application/json").
-define(WORDS, [<<"the">>,<<"is">>,<<"you">>,<<"your">>,
    <<"in">>,<<"of">>,<<"on">>,<<"as">>,<<"are">>,<<"it">>,<<"that">>,<<"by">>,
    <<"be">>,<<"i">>,<<"but">>,<<"for">>,<<"and">>,<<"all">>,<<"at">>,<<"an">>,<<"is">>,
    <<"am">>, <<"ah">>]).

-record(translater_server_state, {words = []}).

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
    {ok, State :: #translater_server_state{}} | {ok, State :: #translater_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    io:format("~n Translator : I started translating words !! "),
    {ok, #translater_server_state{words = []}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #translater_server_state{}) ->
    {reply, Reply :: term(), NewState :: #translater_server_state{}} |
    {reply, Reply :: term(), NewState :: #translater_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #translater_server_state{}} |
    {noreply, NewState :: #translater_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #translater_server_state{}} |
    {stop, Reason :: term(), NewState :: #translater_server_state{}}).


handle_call(_Request, _From, State = #translater_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #translater_server_state{}) ->
    {noreply, NewState :: #translater_server_state{}} |
    {noreply, NewState :: #translater_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #translater_server_state{}}).

handle_cast({new_words, WordList}, State) ->
    do_translate(WordList),
    {noreply, State}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #translater_server_state{}) ->
    {noreply, NewState :: #translater_server_state{}} |
    {noreply, NewState :: #translater_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #translater_server_state{}}).

handle_info(clear, State = #translater_server_state{}) ->
    {noreply, State#translater_server_state{words = []}};

handle_info(_Info, State = #translater_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #translater_server_state{}) -> term    ()).
terminate(_Reason, _State = #translater_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #translater_server_state{},
    Extra :: term()) ->
    {ok, NewState :: #translater_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #translater_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_translate(Words) when Words =:= [] ->
    ok;

do_translate(Words) ->
    AllWorkers = create_workers(100),
    lists:foldl(
        fun(Each, Counter) ->
            WorkerPid = next_worker_pid(AllWorkers, Counter),
            % sending the line to the next worker
            WorkerPid ! {self(), Each},
            Counter + 1
        end, 0, Words),

    lists:foreach(
        fun(Pid) ->
            Pid ! {self(), eof}
        end, AllWorkers),

    % lets wait fo the worker responses
    TranslatedWords = util:waiting_response([],length(AllWorkers)),

    gen_server:cast(words_keeper,{save_to_db,TranslatedWords}),
    io:format("~n Translator : ~p Words Translated ",[length(TranslatedWords)])
.

% this function will create N number of workers. Each worker will start with the function
% `init/0` defined in the `module multi_process_worker`.
create_workers(NumberOfWorkers) ->
    [spawn(translator_worker, init, []) || _X <- lists:seq(1, NumberOfWorkers)].


next_worker_pid(AllWorkerPIDs, Counter) ->
    Index = Counter rem length(AllWorkerPIDs),
    lists:nth(Index + 1, AllWorkerPIDs).
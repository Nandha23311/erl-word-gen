%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2020 5:31 PM
%%%-------------------------------------------------------------------
-module(reader_server).
-author("nandakumar").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,start/0, start_reading/1]).

-define(SERVER, ?MODULE).

-record(reader_server_state, {details=[]}).
-define(DEFAULT_PATH,[
    "books/a_doll's_house.txt",
    "books/anthem.txt",
    "books/beowulf.txt",
    "books/frankenstein.txt",
    "books/pride_and_prejudice.txt",
    "books/the_scarlet_letter.txt",

    "books/book.txt",
    "books/book1.txt",
    "books/book2.txt",
    "books/book3.txt"

    ]).

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
    {ok, State :: #reader_server_state{}} | {ok, State :: #reader_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    io:format("~n Reader : I started reading new words !! "),
    {ok, #reader_server_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #reader_server_state{}) ->
    {reply, Reply :: term(), NewState :: #reader_server_state{}} |
    {reply, Reply :: term(), NewState :: #reader_server_state{}, timeout() | hibernate} |
    {noreply, NewState :: #reader_server_state{}} |
    {noreply, NewState :: #reader_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #reader_server_state{}} |
    {stop, Reason :: term(), NewState :: #reader_server_state{}}).
handle_call(_A, _From, State = #reader_server_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #reader_server_state{}) ->
    {noreply, NewState :: #reader_server_state{}} |
    {noreply, NewState :: #reader_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #reader_server_state{}}).

handle_cast(start_read, State = #reader_server_state{}) ->
    gen_server:cast(?SERVER, {start_read, ?DEFAULT_PATH}),
    {noreply, State};

handle_cast({start_read,PATH}, State = #reader_server_state{}) ->
    start_reading(PATH),
    {noreply, State};

handle_cast({update_state,NewSuccessDetails}, State = #reader_server_state{}) ->
    #reader_server_state{
        details = SuccessDetails
    } = State,
    io:format("~n SuccessDetails ~p ",[SuccessDetails]),
    {noreply, State#reader_server_state{details = SuccessDetails ++ [NewSuccessDetails] }}.


%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #reader_server_state{}) ->
    {noreply, NewState :: #reader_server_state{}} |
    {noreply, NewState :: #reader_server_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #reader_server_state{}}).
handle_info(_Info, State = #reader_server_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #reader_server_state{}) -> term()).
terminate(_Reason, _State = #reader_server_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #reader_server_state{},
    Extra :: term()) ->
    {ok, NewState :: #reader_server_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #reader_server_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start()->
    gen_server:cast(?SERVER ,start_read),
    ok.

start_reading(PathList) ->
    Result = lists:foldl(
        fun(Path, Acc) ->
            case open_file(Path) of
                not_found -> [];
                Device ->
                    AllWorkers = create_workers(50),
                    read_lines(Device, AllWorkers, 0),

                    % lets wait fo the worker responses
                    ResultWords = util:waiting_response(#{}, length(AllWorkers)),
                    ResultWords1 = maps:keys(ResultWords),
                    gen_server:cast(translator_server, {new_words, ResultWords1}),
                    ResultWordsCount = length(ResultWords1),
                    io:format("~n Reader : ~p Words collected ", [ResultWordsCount]),
                    Acc ++ [{Path, ResultWordsCount}]
            end
        end, [], PathList),
    gen_server:cast(?SERVER, {update_state, Result}).


% this function will create N number of workers. Each worker will start with the function
% `init/0` defined in the `module multi_process_worker`.
create_workers(NumberOfWorkers) ->
    [spawn(reader_worker, init, []) || _X <- lists:seq(1, NumberOfWorkers)].

% This function will open the "big_text.txt" file and return the related Device. In Erlang this
% Device is like a process in charge of reading our file
open_file(Path) ->
    case (file:open(Path, read)) of
        {ok, Device} -> Device ;
        _ -> not_found
    end.


% this function reads the file line by line and send the line to the next worker process. It needs 3
% parameters, the Device in order to read the file, the list with all the worker PIDs and a counter,
% this counter is needed for dispatching the line to the next worker.
read_lines(Device, AllWorkers, Counter) ->
    % we are going to read the file line by line
    case file:read_line(Device) of
        {ok, Line} ->
            % getting the next worker process
            WorkerPid = next_worker_pid(AllWorkers, Counter),

            % sending the line to the next worker
            WorkerPid ! {self(), Line},

            % lets read a new line and update counter
            read_lines(Device, AllWorkers, Counter + 1);
        eof ->
            % eof is returned when the text is finished, if that is the case we let know all the workers
            % that the file is finished so we are going to wait for the responses
            lists:foreach(
                fun(Pid) ->
                    Pid ! {self(), eof}
                end, AllWorkers)
    end.

next_worker_pid(AllWorkerPIDs, Counter) ->
    Index = Counter rem length(AllWorkerPIDs),
    lists:nth(Index + 1, AllWorkerPIDs).

% helper in order to get the current time in milliseconds
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec)*1000 + round(Micro / 1000).


% This function will wait for the worker responses. It needs 2 parameters, an accumulated map where we
% are going to store the result and the number of pending responses.
waiting_response(Acc, 0) ->
    % if we arrive to 0 pending responses that means all the workers already answered so we return the
    % accumulated Map
    Acc;
waiting_response(Acc, PendingResponses) ->
    % waiting for messages
    receive
        {_PidWorker, ProcessCount } ->
            waiting_response(Acc + ProcessCount, PendingResponses - 1)
    end.

%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2020 10:33 PM
%%%-------------------------------------------------------------------
-module(db).
-author("nandakumar").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3,insert_one/1]).

-define(SERVER, ?MODULE).

-record(db_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #db_state{}} | {ok, State :: #db_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    emongo:add_pool(test, "localhost", 27017, "emongo", 1),
    io:format("~n ~n Mongo DB started... ~n ~n "),
    {ok, #db_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #db_state{}) ->
    {reply, Reply :: term(), NewState :: #db_state{}} |
    {reply, Reply :: term(), NewState :: #db_state{}, timeout() | hibernate} |
    {noreply, NewState :: #db_state{}} |
    {noreply, NewState :: #db_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #db_state{}} |
    {stop, Reason :: term(), NewState :: #db_state{}}).

handle_call({insert, Data }, _From, State = #db_state{}) ->
    emongo:insert(test, "words", [maps:to_list(Data)]),
    {reply, ok, State};

handle_call(_Request, _From, State = #db_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #db_state{}) ->
    {noreply, NewState :: #db_state{}} |
    {noreply, NewState :: #db_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #db_state{}}).
handle_cast(_Request, State = #db_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #db_state{}) ->
    {noreply, NewState :: #db_state{}} |
    {noreply, NewState :: #db_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #db_state{}}).
handle_info(_Info, State = #db_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #db_state{}) -> term()).
terminate(_Reason, _State = #db_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #db_state{},
    Extra :: term()) ->
    {ok, NewState :: #db_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #db_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
insert_one(Data)->
    gen_server:call({global,?MODULE},{insert, Data}).
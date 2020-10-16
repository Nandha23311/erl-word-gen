%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2020 6:55 PM
%%%-------------------------------------------------------------------
-module(words_keeper_worker).
-author("nandakumar").

%% API
-export([init/0]).

%----------------------------------------------
% Public API
%----------------------------------------------

init() ->
    % starting the loop with an empty accumulated map
    loop(0).

%----------------------------------------------
% Internal functions (not exported)
%----------------------------------------------

% This function is waiting for lines to process
loop(Acc) ->
    receive
        {CallerPID, eof} ->
            % if we received and eof message that means we have to return the result map to the caller process
            CallerPID ! {self(), word_keeper, Acc},
            ok;
        {_CallerPID, {Word, Trans}} ->
            db:insert_one(#{word=>Word,trans => Trans}),
            loop(Acc+1)
    end.
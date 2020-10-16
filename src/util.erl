%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2020 7:20 PM
%%%-------------------------------------------------------------------
-module(util).
-author("nandakumar").

%% API
-export([waiting_response/2]).


% This function will wait for the worker responses. It needs 2 parameters, an accumulated map where we
% are going to store the result and the number of pending responses.
waiting_response(Acc, 0) ->
    % if we arrive to 0 pending responses that means all the workers already answered so we return the
    % accumulated Map
    Acc;
waiting_response(Acc, PendingResponses) ->
    % waiting for messages
    receive
        {_PidWorker, reader, ProcessWords } ->
            waiting_response(maps:merge(Acc,ProcessWords), PendingResponses - 1);
        {_PidWorker, translator, ProcessWords } ->
            waiting_response(Acc ++ ProcessWords, PendingResponses - 1);
        {_PidWorker, word_keeper, ProcessCount } ->
            waiting_response(Acc + ProcessCount, PendingResponses - 1)
    end.

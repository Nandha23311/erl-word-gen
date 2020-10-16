%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2020 5:49 PM
%%%-------------------------------------------------------------------
-module(reader_worker).
-author("nandakumar").

%% API
-export([init/0]).

-define(WORDS, [<<"the">>,<<"is">>,<<"you">>,<<"your">>,
    <<"in">>,<<"of">>,<<"on">>,<<"as">>,<<"are">>,<<"it">>,<<"that">>,<<"by">>,
    <<"be">>,<<"i">>,<<"but">>,<<"for">>,<<"and">>,<<"all">>,<<"at">>,<<"an">>,<<"is">>,
    <<"am">>, <<"ah">>]).

%----------------------------------------------
% Public API
%----------------------------------------------

init() ->
    % starting the loop with an empty accumulated map
    loop(#{}).

%----------------------------------------------
% Internal functions (not exported)
%----------------------------------------------

% This function is waiting for lines to process
loop(Acc) ->
    receive
        {CallerPID, eof} ->
            % if we received and eof message that means we have to return the result map to the caller process
            CallerPID ! {self(), reader, Acc},
            ok;
        {_CallerPID, Line} ->
            ListOfWords = re:split(Line," "),
            NewMapAcc = lists:foldl(fun add_to_map/2, Acc, ListOfWords),
            loop(NewMapAcc)
    end.

% function applied to each word, it converts the words to uppercase in order to deal `Sofia` and `sofia`
% as the same word ('SOFIA')
add_to_map(Word, Map) ->
    % convert the word to uppercase
    Lowercase = string:lowercase(remove_non_letters(Word)),
    Word2 = list_to_binary(Lowercase),

    case {lists:member(Word2, ?WORDS), length(Lowercase) > 1} of
        {false, true} ->
            % update the value and return a new map
            maps:put(Word2, 1, Map);
        _ -> Map
    end.

remove_non_letters(Word)->
    re:replace(Word, "[^A-Za-z ]", "", [global, {return, list}]).
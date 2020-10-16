%%%-------------------------------------------------------------------
%%% @author nandakumar
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Oct 2020 10:16 AM
%%%-------------------------------------------------------------------
-module(translator_worker).
-author("nandakumar").

%% API
-export([init/0]).

-define(HTTP_TIMEOUT_MSEC, 10000).
-define(HTTP_CONNECT_TIMEOUT_MSEC, 100000).
-define(TYPE, "application/json").


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
            TranslatedWords = maps:values(Acc),
            CallerPID ! {self(), translator, TranslatedWords},
            ok;
        {_CallerPID, Word} ->
            MapAcc = get_meaning(Word,Acc),
            loop(MapAcc)
    end.


get_meaning(Word1,Acc)->
    try
%%        HTTPOptions = [{timeout, ?HTTP_TIMEOUT_MSEC}, {connect_timeout, ?HTTP_CONNECT_TIMEOUT_MSEC}],
%%        Url = "http://translate.googleapis.com/translate_a/single?client=gtx&sl=gu&tl=ta&dt=t&dj=1&q="++Word1,
%%        Body = jiffy:encode(#{}),
%%        {ok, {{_, _, _}, _, RespBody}} = httpc:request(post, {Url, [], ?TYPE, iolist_to_binary(Body)}, HTTPOptions, []),
%%
%%        RespMap = jiffy:decode(RespBody, [return_maps]),
%%        case maps:get(<<"sentences">>,RespMap,undefined)of
%%            undefined -> ok;
%%            Data ->
%%                Data1 = lists:nth(1,Data),
%%                Trans = maps:get(<<"trans">>,Data1),
%%                io:format("~n Trans ~p ",[Trans])
%%                gen_server:cast(words_keeper,{new_word,{}}),
%%        end
        maps:put(Word1, {Word1,Word1}, Acc)
    catch
        _S:E  -> io:format("Error")
    end.


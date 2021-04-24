%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(dgiot_amazedtu_decoder).
-include("dgiot_amazedtu.hrl").
-author("johnliu").
-protocol([?AMAZIOT]).

%% API
-export([parse_frame/2, to_frame/1, test/0]).


parse_frame(Buff, Opts) ->
    put(last, erlang:system_time(second)),
    Len = byte_size(Buff),
    NewBuff =
        case re:run(Buff, <<"[0-9a-fA-F]+">>, [global]) of
            {match, [[{0, Len}]]} -> shuwa_utils:hex_to_binary(Buff);
            _ -> Buff
        end,
    NewBuff1 = re:replace(NewBuff, <<"OK\r\n">>, <<",OK,">>, [global, {return, binary}]),
    List = binary:split(NewBuff1, <<",">>, [global]),
%%    lager:info("List ~p",[List]),
    parse_frame(List, [], Opts).


parse_frame([], Acc, _Opts) ->
    {<<>>, Acc};

%% amaziot
%% AMAZ,AP4000E,12345678,login
parse_frame([<<"AMAZ">>, DtuType, DtuAddr, <<"login">> | _Other], _Acc, _Opts) ->
    Frame = #{
        <<"devtype">> => <<"AMAZ">>,
        <<"name">> => DtuType,
        <<"hardware_number">> => DtuAddr
    },
    {<<>>, Frame};

parse_frame([_ | Other], Acc, Opts) ->
    parse_frame(Other, Acc, Opts).



to_frame(_) ->
    {error, not_encode}.

test() ->
    lists:foreach(
        fun(Bin) ->
            {_, Frame} = parse_frame(Bin, no),
            io:format("~p -> ~p~n", [Bin, Frame])
        end, [
            <<"SKZN,ÃûÅ·¶÷»ú05090502'ÃûÅ·µç±í12345678,55388987,q,null,yOK\r\n">>,
            <<"OK\r\nSKZN,ÃûÅ·¶÷»ú01010BDB'ÃûÅ·µç±í19222222,31984488,l,12346,ON+60+6+60+0">>,
            <<"SKZN,ÃûÅ·¶÷»ú07090502,07918088,f,null,null">>,
            <<"SKZN,ÃûÅ·¶÷»ú050205DBÃûÅ·µç±í12345678,03224712,q,null,n">>,
            <<"534B5A4E2CC3FBC5B7B6F7BBFA42424242424242422C33313938343438382C002C6E756C6C2C6E756C6C">>,
            <<"SKZN,ÃûÅ·¶÷»úC2ACD22CÃûÅ·µç±í12345678,28007407,q,null,N">>
        ]).
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
-module(dgiot_amazedtu_channel).
-behavior(shuwa_channelx).
-author("johnliu").
-include_lib("shuwa_framework/include/shuwa_socket.hrl").
-include("dgiot_amazedtu.hrl").
-define(TYPE, <<"AMAZIOTDTU">>).
-define(MAX_BUFF_SIZE, 1024).
-define(SECS, [5, 5 * 60]).
-define(JIAOSHI, 60 * 10 * 1000).
-record(state, {
    id,
    devaddr = <<>>,
    productid = <<>>
}).
%% API
-export([start/2]).

%% Channel callback
-export([init/3, handle_init/1, handle_event/3, handle_message/2, stop/3]).
%% TCP callback
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3]).


%% 注册通道类型
-channel(?TYPE).
-channel_type(#{
    type => 1,
    title => #{
        zh => <<"奇迹物联dtu采集通道"/utf8>>
    },
    description => #{
        zh => <<"奇迹物联dtu采集通道"/utf8>>
    }
}).
%% 注册通道参数
-params(#{
    <<"port">> => #{
        order => 1,
        type => integer,
        required => true,
        default => 51888,
        title => #{
            zh => <<"端口"/utf8>>
        },
        description => #{
            zh => <<"侦听端口"/utf8>>
        }
    },
    <<"heartbeat">> => #{
        order => 2,
        type => integer,
        required => true,
        default => 5,
        title => #{
            zh => <<"心跳周期"/utf8>>
        },
        description => #{
            zh => <<"心跳周期"/utf8>>
        }
    }
}).

start(ChannelId, ChannelArgs) ->
    shuwa_channelx:add(?TYPE, ChannelId, ?MODULE, ChannelArgs).

%% 通道初始化
init(?TYPE, ChannelId, #{
    <<"port">> := Port,
    <<"heartbeat">> := Heartbeat,
    <<"product">> := Products
} = _Args) ->
    lists:map(fun({ProductId, _}) ->
        case shuwa_parse:get_object(<<"Product">>, ProductId) of
            {ok, #{<<"name">> := Name, <<"devType">> := DevType, <<"ACL">> := Acl}} ->
                NewRole = lists:foldl(fun(X, Acc) ->
                    case X of
                        <<"*">> -> Acc;
                        _ ->
                            <<"role:", Role/binary>> = X,
                            Role
                    end
                                      end, <<>>, maps:keys(Acl)),
                shuwa_data:insert({amaziot, Name}, {ProductId, DevType, NewRole}),
                shuwa_data:insert({ChannelId, Name}, ProductId);
            _ -> pass
        end
              end, Products),
    shuwa_data:insert({ChannelId, heartbeat}, Heartbeat),
    State = #state{
        id = ChannelId
    },
    {ok, State, shuwa_tcp_server:child_spec(?MODULE, shuwa_utils:to_int(Port), State)};

init(?TYPE, _ChannelId, _Args) ->
    {ok, #{}, #{}}.

handle_init(State) ->
    {ok, State}.

%% 通道消息处理,注意：进程池调用
handle_event(_EventId, _Event, State) ->
    {ok, State}.

handle_message(_Message, State) ->
    {ok, State}.

stop(_ChannelType, _ChannelId, _State) ->
    ok.

%% =======================
%% tcp server start
%% {ok, State} | {stop, Reason}
init(#tcp{state = #state{id = ChannelId}} = TCPState) ->
    case shuwa_bridge:get_products(ChannelId) of
        {ok, ?TYPE, _ProductIds} ->
            erlang:send_after(5 * 1000, self(), login),
            {ok, TCPState};
        {error, not_find} ->
            {stop, not_find_channel}
    end.

%% 设备登录报文
handle_info({tcp, Buff}, #tcp{socket = Socket, state = #state{id = ChannelId, devaddr = <<>>, productid = <<>>} = State} = TCPState) ->
    shuwa_bridge:send_log(ChannelId, "DTU revice from  ~p", [shuwa_utils:binary_to_hex(Buff)]),
    DTUIP = shuwa_evidence:get_ip(Socket),
    {DtuAddr, ProductId1} =
        case dgiot_amazedtu_decoder:parse_frame(Buff, no) of
            {_Buff1, [#{<<"name">> := Name, <<"hardware_number">> := DevAddr1} | _]} when DevAddr1 =/= <<>> ->

                {ProductId, DeviceId} = dgiot_amazedtu:get_objectid(Name, ChannelId, DevAddr1),

                dgiot_amazedtu:create_device(DeviceId, Name, DevAddr1, DTUIP),
                {DevAddr1, ProductId};
            _ ->
                {<<>>, <<>>}
        end,
    {noreply, TCPState#tcp{state = State#state{productid = ProductId1, devaddr = DtuAddr}}};

%% 指令返回报文
handle_info({tcp, Buff}, #tcp{state = #state{id = ChannelId, devaddr = DevAddr, productid = ProductId}} = TCPState) ->
    shuwa_bridge:send_log(ChannelId, "Amaziot revice from  ~p", [shuwa_utils:binary_to_hex(Buff)]),
    <<SlaveId:8, _FunCode:8, Len:8, ResponseData/binary>> = Buff,
    case SlaveId of
        17 ->
            case Len of
                2 ->
                    <<Data:16, _Crc/binary>> = ResponseData,
                    Temperature = (Data - 10000) / 100,
                    Topic = <<"thing/", ProductId/binary, "/", DevAddr/binary, "/post">>,
                    shuwa_mqtt:publish(self(), Topic, shuwa_utils:binary_to_hex(Buff)),
                    Map = #{<<"temperature">> => Temperature},
                    shuwa_tdengine_adapter:save(ProductId, DevAddr, Map),
                    {noreply, TCPState};
                _ ->
                    {noreply, TCPState}
            end;
        _ ->
            {noreply, TCPState}
    end;

handle_info(login, #tcp{state = #state{devaddr = DevAddr, productid = ProductId, id = ChannelId}} = TCPState) ->
    shuwa_bridge:send_log(ChannelId, "ChannelId ~p DevAddr ~p", [ChannelId, DevAddr]),
    shuwa_mqtt:subscribe(<<"thing/", ProductId/binary, "/", DevAddr/binary>>),
    erlang:send_after(1000, self(), temperature),
    {noreply, TCPState#tcp{buff = <<>>, state = #state{id = ChannelId}}};


%%温度
%%110400500001334B
handle_info(temperature, #tcp{state = #state{id = ChannelId}} = TCPState) ->
    Payload = <<"110400500001334B">>,
    shuwa_tcp_server:send(TCPState, shuwa_utils:hex_to_binary(Payload)),

    Heartbeat =
        case shuwa_data:get({ChannelId, heartbeat}) of
            not_find ->
                <<"5">>;
            Heartbeat1 ->
                Heartbeat1
        end,

    erlang:send_after(Heartbeat * 1000, self(), temperature),
    {noreply, TCPState};

%% {stop, TCPState} | {stop, Reason} | {ok, TCPState} | ok | stop
handle_info(_Info, TCPState) ->
    {noreply, TCPState}.

handle_call(_Msg, _From, TCPState) ->
    {reply, ok, TCPState}.

handle_cast(_Msg, TCPState) ->
    {noreply, TCPState}.


code_change(_OldVsn, TCPState, _Extra) ->
    {ok, TCPState}.


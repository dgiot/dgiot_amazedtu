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

-module(dgiot_amazedtu).
-include("dgiot_amazedtu.hrl").
-export([
    get_Product/0,
    get_objectid/3,
    create_device/4,
    get_Dict/0,
    key_to_value/1,
    value_to_key/1]).

-define(APP, ?MODULE).

get_objectid(Name, ChannelId, DevAddr) ->
    case shuwa_data:get({ChannelId, Name}) of
        not_find -> not_find;
        ProductId ->
            #{<<"objectId">> := DeviceId} =
                shuwa_parse:get_objectid(<<"Device">>, #{<<"product">> => ProductId, <<"devaddr">> => DevAddr}),
            {ProductId, DeviceId}
    end.

json_to_binary(Param) ->
    Map =
        case binary:match(Param, [<<":">>]) of
            nomatch ->
                Param;
            {_, _} ->
                List = binary:split(Param, <<"+">>, [global]),
                lists:foldl(fun(X, Acc) ->
                    [_K, V] = binary:split(X, <<":">>, [global]),
                    case Acc of
                        <<>> -> <<V/binary>>;
                        _ -> <<Acc/binary, "+", V/binary>>
                    end
                            end,
                    <<>>, List)

        end,
    Map.

key_to_value(Key) ->
    case shuwa_data:get({tovalue, Key}) of
        not_find ->
            error;
        Value ->
            Value
    end.

value_to_key(Value) ->
    case shuwa_data:get({tokey, Value}) of
        not_find ->
            error;
        Key ->
            Key
    end.


get_Dict() ->
    Ids = case shuwa_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"type">> => <<"dict_template">>}}) of
              {ok, #{<<"results">> := Results}} ->
                  lists:foldl(fun(#{<<"objectId">> := ObjectId}, Acc) ->
                      case Acc of
                          <<>> -> ObjectId;
                          _ -> <<Acc/binary, ",", ObjectId/binary>>
                      end
                              end, <<>>, Results);
              _ -> <<>>
          end,
    List = binary:split(Ids, <<",">>, [global]),
    case shuwa_parse:query_object(<<"Dict">>, #{<<"where">> => #{<<"type">> => #{<<"$in">> => List}}}) of
        {ok, #{<<"results">> := Results1}} ->
            lists:foldl(fun(D, _Acc) ->
                case D of
                    #{<<"data">> := #{<<"key">> := Key, <<"value">> := Value, <<"para">> := Params}} ->
                        shuwa_data:insert({tovalue, Key}, Value),
                        shuwa_data:insert({tokey, Value}, Key),
                        shuwa_data:insert({params, Key}, Params);
                    #{<<"data">> := #{<<"key">> := Key, <<"value">> := Value}} ->
                        shuwa_data:insert({tovalue, Key}, Value),
                        shuwa_data:insert({tokey, Value}, Key);
                    _ -> pass
                end
                        end, [], Results1);
        _ -> pass
    end.

get_Product() ->
    Names = case shuwa_parse:query_object(<<"Product">>, #{<<"skip">> => 0}) of
                {ok, #{<<"results">> := Results}} ->
                    lists:foldl(fun(#{<<"name">> := Name}, Acc) ->
                        Acc ++ [Name]
                                end, [], Results);
                _ -> []
            end,
    shuwa_data:insert({amaziot, names}, Names),
    ok.

create_device(DeviceId, Name, DevAddr, DTUIP) ->
    case shuwa_data:get({amaziot, Name}) of
        not_find ->
            pass;
        {ProductId, DevType, Role}
            ->
            case shuwa_parse:get_object(<<"Device">>, DeviceId) of
                {ok, #{<<"results">> := [#{<<"devaddr">> := _GWAddr} | _] = _Result}} ->
                    shuwa_parse:update_object(<<"Device">>, DeviceId, #{<<"ip">> => DTUIP, <<"status">> => <<"ONLINE">>});
                _ ->
                    shuwa_shadow:create_device(#{
                        <<"devaddr">> => DevAddr,
                        <<"name">> => <<Name/binary, DevAddr/binary>>,
                        <<"ip">> => DTUIP,
                        <<"isEnable">> => true,
                        <<"product">> => ProductId,
                        <<"ACL">> => #{
                            <<"role:", Role/binary>> => #{
                                <<"read">> => true,
                                <<"write">> => true
                            }
                        },
                        <<"status">> => <<"ONLINE">>,
                        <<"location">> => #{<<"__type">> => <<"GeoPoint">>, <<"longitude">> => 120.161324, <<"latitude">> => 30.262441},
                        <<"brand">> => <<"Name"/utf8>>,
                        <<"devModel">> => DevType
                    })
            end
    end.





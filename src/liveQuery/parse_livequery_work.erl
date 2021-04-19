-module(parse_livequery_work).
-define(SHUWA_PARSE_ETS, shuwa_parse_ets).
%% API
-export([start_link/1, start_link/2, start_link/3,  init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {subscriber = [], sessionToken = undefined, is_ready = false, send = false, serial, id, appid, jskey}).

%% Parse LiveQuery Protocol Specification
%%https://github.com/parse-community/parse-server/wiki/Parse-LiveQuery-Protocol-Specification
%% Parse LiveQuery Protocol Specification

start_link(SessionToken) ->
    case shuwa_parse:get_config() of
        {ok, Cfg} ->
            lager:info("Cfg ~p ",[Cfg]),
            start_link([], Cfg#{ <<"sessionToken">> => SessionToken  });
        {error, Reason} ->
            {error, Reason}
    end.

start_link(WSOpts, #{<<"host">> := Host, <<"path">> := Path} = Args) ->
    Url = shuwa_httpc:url_join([Host, Path]),
    shuwa_wsocket:start_link(?MODULE, Url, WSOpts, [Args]).

start_link(Name, WSOpts, #{<<"host">> := Host, <<"path">> := Path} = Args) ->
    Url = shuwa_httpc:url_join([Host, Path]),
    shuwa_wsocket:start_link({local, Name}, ?MODULE, Url, WSOpts, [Args]).


init([#{<<"appid">> := AppId, <<"jskey">> := JSKey} = Args]) ->
    SessionToken = maps:get(<<"sessionToken">>, Args, undefined),
    shuwa_data:insert(?SHUWA_PARSE_ETS,SessionToken,self()),
    {ok, #state{
        sessionToken = SessionToken,
        appid = list_to_binary(AppId),
        jskey = list_to_binary(JSKey),
        serial = 0,
        subscriber = []
    }}.

handle_call({sub, Table, Where}, {Pid, _}, #state{subscriber = Subs} = State) ->
    case State#state.is_ready of
        false ->
            {reply, ok, State#state{subscriber = [{Pid, Table, Where} | Subs]}};
        _ ->
            case do_sub(Pid, Table, Where, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;


handle_call({unsub, RequestId}, {Pid, _}, #state{send = Send, subscriber = Subs} = State) ->
    case lists:keyfind(RequestId, 1, Subs) of
        false ->
            {reply, {error, not_find}, State};
        {RequestId, Pid} ->
            Frame = #{
                <<"op">> => <<"unsubscribe">>,
                <<"requestId">> => RequestId
            },
            case Send(jsx:encode(Frame)) of
                ok ->
                    {reply, ok, State#state{subscriber = lists:delete(RequestId, Subs)}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

%% 透传通道,只记录一个最近的用户,一般情况下只用于网页
handle_call({transparent, Payload}, _From, State = #state{send = Send}) ->
    case Send(Payload) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({text, Payload}, _From, State) ->
    case jsx:decode(Payload, [{labels, binary}, return_maps]) of
        #{<<"op">> := <<"error">>, <<"error">> := Err} ->
            {reply, {error, Err}, State};
        #{<<"op">> := <<"connected">>, <<"clientId">> := ClientId} ->
            logger:info("~p connected!", [ClientId]),
            NewState = re_sub(State#state{ is_ready = true }),
            {reply, ok, NewState#state{ id = ClientId}};
        #{<<"op">> := <<"subscribed">>,
            <<"requestId">> := ReportId, <<"clientId">> := _ClientId} ->
            Fun1 = fun(Pid) -> Pid ! {subscribed, ReportId}, ok end,
            case do_message(ReportId, Fun1, State) of
                ok ->
                    {reply, ok, State};
                {error, not_alive} ->
                    {reply, {error, not_find_sub}, State}
            end;
        Msg = #{<<"requestId">> := ReportId} ->
            Fun2 = fun(Pid) -> Pid ! {livequery, Msg}, ok end,
            case do_message(ReportId, Fun2, State) of
                ok ->
                    {reply, ok, State};
                {error, not_alive} ->
                    {reply, {error, not_find_sub}, State}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({connection_error, _Reason}, State) ->
    logger:info("connect_error ~p", [_Reason]),
    {noreply, State};

handle_info({connection_ready, Send}, #state{appid = AppId, jskey = JsKey, sessionToken = SessionToken} = State) ->
    Frame =
        case SessionToken of
            undefined ->
                #{<<"javascriptKey">> => JsKey};
            _ ->
                #{<<"sessionToken">> => SessionToken}
        end,
    logger:info("Frame ~p ~n",[Frame]),
    case Send(jsx:encode(Frame#{
        <<"op">> => <<"connect">>,
        <<"applicationId">> => AppId}
    )) of
        ok ->
            {noreply, State#state{send = Send}};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{sessionToken = SessionToken} = _State) ->
    shuwa_data:delete(?SHUWA_PARSE_ETS,SessionToken),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

do_message(RequestId, Fun, #state{subscriber = Subs}) ->
    case lists:keyfind(RequestId, 1, Subs) of
        false ->
            {error, not_find};
        {RequestId, Pid, _, _} ->
            case erlang:is_process_alive(Pid) of
                true -> Fun(Pid);
                false -> {error, not_alive}
            end
    end.

do_sub(Pid, Table, Where, #state{serial = Serial} = State) ->
    RequestId = Serial + 1,
    do_sub(Pid, RequestId, Table, Where, State).

do_sub(Pid, RequestId, Table, Where, #state{send = Send, subscriber = Subs,
    sessionToken = SessionToken} = State) ->
    Query =
        case SessionToken of
            undefine ->
                #{
                    <<"op">> => <<"subscribe">>,
                    <<"requestId">> => RequestId,
                    <<"query">> =>#{
                        <<"className">> => Table,
                        <<"where">> => Where
                    }
                };
            _ ->
                #{
                    <<"op">> => <<"subscribe">>,
                    <<"requestId">> => RequestId,
                    <<"query">> =>#{
                        <<"className">> => Table,
                        <<"where">> => Where
                    },
                    <<"sessionToken">> => SessionToken
                }
        end,
    case Send(jsx:encode(Query)) of
        ok ->
            {ok, State#state{
                serial = RequestId,
                subscriber = [{RequestId, Pid, Table, Where} | Subs]
            }};
        {error, Reason} ->
            logger:error("sub error, ~p", [Reason]),
            {error, Reason}
    end.


re_sub(#state{subscriber = Subs} = State) ->
    logger:info("Subs ~p ",[Subs]),
    lists:foldl(
        fun
            ({Pid, Table, Where}, State1) ->
                case do_sub(Pid, Table, Where, State1) of
                    {ok, State2} ->
                        State2;
                    {error, _Reason} ->
                        State1
                end;
            ({RequestId, Pid, Table, Where}, State1) ->
                case do_sub(Pid, RequestId, Table, Where, State1) of
                    {ok, State2} ->
                        State2;
                    {error, _Reason} ->
                        State1
                end
        end, State#state{subscriber = [], serial = 0}, Subs).
-module(parse_livequery).
-define(SHUWA_PARSE_ETS, shuwa_parse_ets).
%% API
-export([start_link/1, start_link/2, start_link/3]).
-export([subscribe/3, unsubscribe/1, unsubscribe/2, stop/1]).

-record(state, {subscriber = [], sessionToken = undefined, send = false, serial, id, appid, jskey}).

%% Parse LiveQuery Protocol Specification
%%https://github.com/parse-community/parse-server/wiki/Parse-LiveQuery-Protocol-Specification
%% Parse LiveQuery Protocol Specification

start_link(SessionToken) ->
    parse_livequery_work:start_link(SessionToken).

start_link(WSOpts, #{host := _Host, path := _Path} = Args) ->
    parse_livequery_work:start_link(WSOpts, Args).

start_link(Name, WSOpts, #{host := _Host, path := _Path} = Args) ->
    parse_livequery_work:start_link(Name, WSOpts, Args).

subscribe(SessionToken, Table, Where) ->
    lager:info("lookup ~p", [shuwa_data:lookup(?SHUWA_PARSE_ETS, SessionToken)]),
    {ok, NewPid} =
        case shuwa_data:lookup(?SHUWA_PARSE_ETS, SessionToken) of
            {ok, Pid} when is_pid(Pid) ->
                lager:info("Pid ~p", [Pid]),
                case is_process_alive(Pid) of
                    true -> {ok, Pid};
                    false -> parse_livequery_sup:start_livequery(SessionToken)
                end;
            _Reason ->
                lager:info("_Reason ~p", [_Reason]),
                R = parse_livequery_sup:start_livequery(SessionToken),
                lager:info("pid2 ~p", [R])
        end,
    lager:info("NewPid ~p", [NewPid]),
    gen_server:call(NewPid, {sub, Table, Where}).

unsubscribe(RequestId) ->
    gen_server:call(?MODULE, {unsub, RequestId}).
unsubscribe(SessionToken, RequestId) ->
    case shuwa_data:lookup(?SHUWA_PARSE_ETS, SessionToken) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, {unsub, RequestId});
        _Reason ->
            parse_livequery_sup:start_livequery(SessionToken)
    end.

stop(SessionToken) ->
    case shuwa_data:lookup(?SHUWA_PARSE_ETS, SessionToken) of
        {ok, Pid} when is_pid(Pid) ->
            is_process_alive(Pid) andalso gen_server:call(Pid, stop, 5000);
        _Reason ->
            pass
    end.

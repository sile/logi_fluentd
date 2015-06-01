%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Fluentd backend for logi
%%
%% TODO: socket書き込み時などにblockしないようにする
%%
%% @private
-module(logi_fluentd_backend).

-behaviour(logi_backend).
-behaviour(gen_server).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start/3, stop/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Application Internal API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([write/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%------------------------------------------------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------------------------------------------------
-record(state,
        {
          socket :: inet:socket()
        }).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Fluentd出力用のバックエンドプロセスを起動する
-spec start(logi_fluentd:backend_id(), logi_fluentd:host(), logi_fluentd:backend_options()) -> ok | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start(BackendId, FluentdHost, Options) when is_atom(BackendId), is_list(Options) ->
    case logi_fluentd_backend_sup:start_child(BackendId, FluentdHost, Options) of
        {ok, _Pid}                      -> ok;
        {error, Reason}                 -> {error, Reason}
    end;
start(BackendId, FluentdHost, Options) ->
    error(badarg, [BackendId, FluentdHost, Options]). % まだ引数チェックは簡易的

%% @doc バックエンドプロセスを停止する
-spec stop(logi_fluentd:backend_id()) -> ok.
stop(BackendId) ->
    logi_fluentd_backend_sup:stop_child(BackendId).

%%------------------------------------------------------------------------------------------------------------------------
%% Application Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc logi_fluentd_backend_supから呼び出される実際のプロセス起動関数
-spec start_link(logi_fluentd:backend_id(), logi_fluentd:host(), logi_fluentd:backend_options()) ->
                        {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_link(BackendId, Rotator, NameGenerator) ->
    gen_server:start_link({local, BackendId}, ?MODULE, [Rotator, NameGenerator], []).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_backend' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec write(logi_backend:backend(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> any().
write(Backend, Location, MsgInfo, Format, Args) ->
    Formatter = logi_backend:get_data(Backend),
    Msg = logi_fluentd_formatter:format(Formatter, Location, MsgInfo, Format, Args),
    gen_server:cast(logi_backend:get_process(Backend), {write, Msg}).

%%------------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
init([FluentdHost, Options]) ->
    Port = proplists:get_value(port, Options, 8888),
    Timeout = proplists:get_value(timeout, Options, 5000),
    case gen_tcp:connect(FluentdHost, Port, [], Timeout) of
        {error, Reason} ->
            _ = timer:sleep(1000), % XXX: fluentdが死んでいる場合にリトライが続くようにするための暫定処置
            {stop, {cannot_connect_to_fluentd, Reason}};
        {ok, Socket} ->
            State =
                #state{
                   socket = Socket
                  },
            {ok, State}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({write, Msg}, State) ->
    %% io:format("# write: ~s\n", [Msg]),
    %% TODO: overload対策 (TODO: この辺りは共通化したい)
    ok = do_write(Msg, State),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({tcp_closed, Socket}, State) ->
    {stop, {tcp_closed, Socket}, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    {stop, {tcp_error, Socket, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec do_write(binary(), #state{}) -> ok.
do_write(Msg, State) ->
    %% NOTE: エラー系はhandle_infoで受け取る想定
    _ = gen_tcp:send(State#state.socket, Msg),
    ok.

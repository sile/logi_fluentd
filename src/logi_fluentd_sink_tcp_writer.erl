%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A sink process and writer for logi_fluentd_sink_tcp module
%% @private
%% @end
-module(logi_fluentd_sink_tcp_writer).

-behaviour(logi_sink_writer).
-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/1]).

-export_type([start_arg/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/4, get_writee/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%----------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%----------------------------------------------------------------------------------------------------------------------
-define(STATE, ?MODULE).
-record(?STATE,
        {
          socket :: inet:socket()
        }).

-type start_arg() :: {logi:logger(), logi_layout:layout(), logi_fluentd_sink_tcp:address(),
                      inet:port_number(), timeout(), [gen_tcp:connect_option()]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new fluentd writer
-spec start_link(start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink_writer' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Format, Data, {Pid, Layout}) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    ok = gen_server:cast(Pid, {write, FormattedData}),
    FormattedData.

%% @private
get_writee({Pid, _}) ->
    Pid.

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init({Logger, Layout, Address, Port, ConnectTimeout, ConnectOptions}) ->
    _ = logi:save_as_default(Logger),
    case gen_tcp:connect(Address, Port, ConnectOptions, ConnectTimeout) of
        {error, Reason} ->
            _ = logi:critical("Can't connect to the fluentd: address=~p, port=~p, reason=~p",
                              [Address, Port, Reason]),
            {stop, {Reason, {gen_tcp, connect, [Address, Port, ConnectOptions, ConnectTimeout]}}};
        {ok, Socket} ->
            State =
                #?STATE{
                    socket = Socket
                   },
            _ = logi:info("Started: address=~p, port=~p, socket=~p", [Address, Port, Socket]),
            ok = logi_sink_proc:send_writer_to_parent(logi_sink_writer:new(?MODULE, {self(), Layout})),
            {ok, State}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({write, Arg}, State) ->
    handle_write(Arg, State);
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({tcp_closed, Socket}, State) ->
    _ = logi:critical("The tcp socket is closed: socket=~p", [Socket]),
    {stop, {tcp_closed, Socket}, State};
handle_info({tcp_error, Socket, Reason}, State) ->
    _ = logi:critical("A tcp socket error occurred: socket=~p, reason=~p", [Socket, Reason]),
    {stop, {tcp_error, Socket, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(Reason, _State) ->
    _ = logi:info("Terminating: reason=~p", [Reason]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec handle_write(iodata(), #?STATE{}) -> {noreply, #?STATE{}} | {stop, Reason::term(), #?STATE{}}.
handle_write(Data, State = #?STATE{socket = Socket}) ->
    case gen_tcp:send(Socket, Data) of
        {error, Reason} ->
            _ = logi:critical("Can't send data to the fluentd: socket=~p, reason=~p, data=~p", [Socket, Reason, Data]),
            {stop, {error, {Reason, {gen_tcp, send, [Socket, Data]}}}, State};
        ok ->
            {noreply, State}
    end.

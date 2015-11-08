%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc TODO
%% @private
-module(logi_fluentd_writer_tcp).

-behaviour(gen_server).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([start_link/2]).
-export([write/2]).

-export_type([start_arg/0]).

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

-type start_arg() :: {logi_fluentd_sink_tcp:address(), inet:port_number(), logi:logger(), timeout(), [gen_tcp:connect_option()]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Starts a new fluentd writer
-spec start_link(logi_fluentd_sink_tcp:writer_id(), start_arg()) -> {ok, pid()} | {error, Reason::term()}.
start_link(WriterId, Arg) ->
    gen_server:start_link({local, WriterId}, ?MODULE, [WriterId, Arg], []).

%% @doc Writes a message
-spec write(logi_fluentd_sink_tcp:writer_id(), iodata()) -> ok.
write(WriterId, Message) ->
    gen_server:cast(WriterId, {write, Message}).

%%----------------------------------------------------------------------------------------------------------------------
%% 'gen_server' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
init([WriterId, {Address, Port, Logger, ConnectTimeout, ConnectOptions}]) ->
    _ = logi:save_as_default(Logger),
    _ = logi:set_headers(#{id => WriterId}),
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

%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Fluentd Sink for the "in_tcp" Input Plugin
-module(logi_fluentd_sink_tcp).

-behaviour(logi_sink).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/1]).
-export([start_writer/2, start_writer/3]).
-export([stop_writer/1]).
-export([which_writers/0]).

-export_type([writer_id/0]).
-export_type([writer_options/0, writer_option/0]).
-export_type([address/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([write/5, default_layout/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type writer_id() :: atom().
%% The identifier of a fluentd writer

-type address() :: inet:ip_address() | inet:hostname().

-type writer_options() :: [writer_option()].
-type writer_option() :: {logger, logi:logger()}
                       | {port, inet:port_number()}
                       | {connect_timeout, timeout()}
                       | {connect_opt, [gen_tcp:connect_option()]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @doc Creates a new sink instance
-spec new(writer_id()) -> logi_sink:sink().
new(WriterId) ->
    _ = is_atom(WriterId) orelse error(badarg, [WriterId]),
    logi_sink:new(?MODULE, WriterId).

%% @equiv start_writer(WriterId, Address, [])
-spec start_writer(writer_id(), address()) -> {ok, pid()} | {error, Reason::term()}.
start_writer(WriterId, Address) ->
    start_writer(WriterId, Address, []).

%% @doc Starts a new fluentd writer
%%
%% The default layout is `TODO'.
-spec start_writer(writer_id(), address(), writer_options()) -> {ok, pid()} | {error, Reason::term()}.
start_writer(WriterId, Address, Options) ->
    Args = [WriterId, Address, Options],
    _ = is_atom(WriterId) orelse error(badarg, Args),
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Port = proplists:get_value(port, Options, 24224),
    ConnectTimeout = proplists:get_value(connect_timeout, Options, 1000),
    ConnectOptions = proplists:get_value(connect_opt, Options, []),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = is_integer(Port) orelse error(badarg, Args),
    _ = is_timeout(ConnectTimeout) orelse error(badarg, Args),
    _ = is_list(ConnectOptions) orelse error(badarg, Args),

    logi_fluentd_writer_tcp_sup:start_child(WriterId, {Address, Port, Logger, ConnectTimeout, ConnectOptions}).

%% @doc Stops the fluentd writer
%%
%% If the writer does not exists, it is silently ignored.
-spec stop_writer(writer_id()) -> ok.
stop_writer(WriterId) ->
    _ = is_atom(WriterId) orelse error(badarg, [WriterId]),
    logi_fluentd_writer_tcp_sup:stop_child(WriterId).

%% @doc Returns a list of the running fluentd writers
-spec which_writers() -> [writer_id()].
which_writers() ->
    [Id || {Id, _} <- logi_fluentd_writer_tcp_sup:which_children()].

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_sink' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
write(Context, Layout, Format, Data, Writer) ->
    FormattedData = logi_layout:format(Context, Format, Data, Layout),
    logi_fluentd_writer_tcp:write(Writer, FormattedData).

%% @private
default_layout(_Writer) ->
    logi_fluentd_layout_json_default:new(
      logi_layout_limit:new(
        logi_layout_io_lib_format:new())).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_timeout(timeout() | term()) -> boolean().
is_timeout(infinity) -> true;
is_timeout(X)        -> is_integer(X) andalso X >= 0.

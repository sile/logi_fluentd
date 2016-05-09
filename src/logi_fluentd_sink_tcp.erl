%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Fluentd Sink for the "in_tcp" Input Plugin
%%
%% == NOTE ==
%%
%% The sink has no overload protections,
%% so it is recommended to use it together with (for example) `logi_slink_flow_limiter'
%% of [logi_stdlib](https://github.com/sile/logi_stdlib) in a production environment.
%%
%% @end
-module(logi_fluentd_sink_tcp).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/2, new/3]).

-export_type([options/0, option/0]).
-export_type([address/0]).

%%----------------------------------------------------------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------------------------------------------------------
-type address() :: inet:ip_address() | inet:hostname().

-type options() :: [option()].
-type option() :: {logger, logi:logger()}
                | {layout, logi_layout:layout()}
                | {port, inet:port_number()}
                | {connect_timeout, timeout()}
                | {connect_opt, [gen_tcp:connect_option()]}.
%% `logger':
%% -  The logger instance which is used to report internal events of the sink process
%% - Default: `logi:default_logger()'
%%
%% `layout':
%% - The layout instance used by the sink
%% - Default: `logi_fluentd_layout_json:new()'
%%
%% `port':
%% - The port number of the destination fluentd
%% - Default: `24224'
%%
%% `connect_timeout':
%% - Timeout of `gen_tcp:connect/4'
%% - Default: `1000'
%%
%% `connect_opt':
%% - Connect Options (i.e., the third argment of `gen_tcp:connect/4')
%% - Default: `[]'

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(SinkId, Address, [])
-spec new(logi_sink:id(), address()) -> logi_sink:sink().
new(SinkId, Address) ->
    new(SinkId, Address, []).

%% @doc Creates a new sink
-spec new(logi_sink:id(), address(), options()) -> logi_sink:sink().
new(SinkId, Address, Options) ->
    Args = [SinkId, Address, Options],
    _ = is_list(Options) orelse error(badarg, Args),

    Logger = proplists:get_value(logger, Options, logi:default_logger()),
    Layout = proplists:get_value(layout, Options, logi_fluentd_layout_json:new()),
    Port = proplists:get_value(port, Options, 24224),
    ConnectTimeout = proplists:get_value(connect_timeout, Options, 1000),
    ConnectOptions = proplists:get_value(connect_opt, Options, []),
    _ = logi:is_logger(Logger) orelse error(badarg, Args),
    _ = logi_layout:is_layout(Layout) orelse error(badarg, Args),
    _ = is_integer(Port) orelse error(badarg, Args),
    _ = is_timeout(ConnectTimeout) orelse error(badarg, Args),
    _ = is_list(ConnectOptions) orelse error(badarg, Args),

    StartArg = {Logger, Layout, Address, Port, ConnectTimeout, ConnectOptions},
    logi_sink:new(#{id => SinkId, start => {logi_fluentd_sink_tcp_writer, start_link, [StartArg]}}).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec is_timeout(timeout() | term()) -> boolean().
is_timeout(infinity) -> true;
is_timeout(X)        -> is_integer(X) andalso X >= 0.

%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc A logi_layout implementation which formats log messages as JSON string
%% @end
-module(logi_fluentd_layout_json).

-behaviour(logi_layout).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([new/0, new/1]).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback API
%%----------------------------------------------------------------------------------------------------------------------
-export([format/4]).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @equiv new(logi_layout_io_lib_format:new())
-spec new() -> logi_layout:layout().
new() ->
    new(logi_layout_io_lib_format:new()).

%% @doc Creates a new layout instance
-spec new(logi_layout:layout()) -> logi_layout:layout().
new(BaseLayout) ->
    _ = logi_layout:is_layout(BaseLayout) orelse error(badarg, BaseLayout),
    logi_layout:new(?MODULE, BaseLayout).

%%----------------------------------------------------------------------------------------------------------------------
%% 'logi_layout' Callback Functions
%%----------------------------------------------------------------------------------------------------------------------
%% @private
format(Context, Format, Data, BaseLayout) ->
    Location = logi_context:get_location(Context),
    Params =
        [
         {severity, logi_context:get_severity(Context)},
         {node, node(logi_location:get_process(Location))},
         {process, to_binary(logi_location:get_process(Location))},
         {application, logi_location:get_application(Location)},
         {module, logi_location:get_module(Location)},
         {line, logi_location:get_line(Location)},
         {timestamp, format_timestamp(logi_context:get_timestamp(Context))},
         {message, iolist_to_binary(logi_layout:format(Context, Format, Data, BaseLayout))}
         | [{to_binary(K), to_json_scalar(V)} || {K, V} <- maps:to_list(logi_context:get_headers(Context))]
        ],
    [jsone:encode(Params), $\n].

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%----------------------------------------------------------------------------------------------------------------------
-spec format_timestamp(erlang:timestamp()) -> non_neg_integer().
format_timestamp({Mega, Sec, _}) ->
    Mega * 1000 * 1000 + Sec.

-spec to_binary(term()) -> binary().
to_binary(X) ->
    iolist_to_binary(logi_lib_layout:term_to_iodata(X)).

-spec to_json_scalar(term()) -> jsone:json_scalar().
to_json_scalar(X) when is_number(X) -> X;
to_json_scalar(X) when is_atom(X)   -> X;
to_json_scalar(X)                   -> to_binary(X).

%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% TODO: 他フォーマットに対応する (msgpack, json, etc)
-module(logi_fluentd_formatter_default).

-behaviour(logi_fluentd_formatter).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([make/0]).
-export_type([formatter/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_fluentd_formatter' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([format/5]).

%%------------------------------------------------------------------------------------------------------------------------
%% Macros & Records & Types
%%------------------------------------------------------------------------------------------------------------------------
-define(MAX_LOG_SIZE, 1024 * 4). % TODO: オプションで変更可能にする

-define(STATE, ?MODULE).

-record(?STATE,
        {
        }).

-opaque formatter() :: #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
-spec make() -> formatter().
make() ->
    #?STATE{}.

%%------------------------------------------------------------------------------------------------------------------------
%% 'logi_fluentd_formatter' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @private
-spec format(formatter(), logi_location:location(), logi_msg_info:info(), io:format(), [term()]) -> iodata().
format(_Formatter, Location, MsgInfo, Format, Args) ->
    Params =
        [
         {severity, logi_msg_info:get_severity(MsgInfo)},
         {node, logi_location:get_node(Location)},
         {process, to_binary(logi_location:get_process(Location))},
         {application, logi_location:get_application(Location)},
         {module, logi_location:get_module(Location)},
         {line, logi_location:get_line(Location)},
         {timestamp, format_timestamp(logi_msg_info:get_timestamp(MsgInfo))},
         {message, abbrev(iolist_to_binary(re:replace(io_lib:format(Format, Args), "\\s+", " ", [global])),
                          ?MAX_LOG_SIZE, <<"...">>)} |
         [{K, to_binary(V)} || {K, V} <- logi_msg_info:get_headers(MsgInfo)]
        ],
    <<(jsone:encode(Params))/binary, $\n>>.

%%------------------------------------------------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------------------------------------------------
%% TODO: formatter系は共通化する
-spec format_timestamp(erlang:timestamp()) -> non_neg_integer().
format_timestamp(Timestamp) ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Timestamp)).

-spec to_binary(term()) -> binary().
to_binary(X) ->
    list_to_binary(to_string(X)). % XXX: 非効率

-spec to_string(term()) -> string().
to_string(V) when is_binary(V)   -> binary_to_list(V);
to_string(V) when is_atom(V)     -> atom_to_list(V);
to_string(V) when is_integer(V)  -> integer_to_list(V);
to_string(V) when is_float(V)    -> float_to_list(V);
to_string(V) when is_function(V) -> erlang:fun_to_list(V);
to_string(V) when is_pid(V)      -> erlang:pid_to_list(V);
to_string(V) when is_port(V)     -> erlang:port_to_list(V);
to_string(V) when is_reference(V)-> erlang:ref_to_list(V);
to_string(V) when is_list(V)     ->
    IsNonNegInteger = fun (C) -> is_integer(C) andalso C >= 0 end,
    case lists:all(IsNonNegInteger, V) of
        true  -> V;
        false -> lists:flatten(io_lib:format("~w", [V]))
    end;
to_string(V) ->
    lists:flatten(io_lib:format("~w", [V])).

-spec abbrev(Input::binary(), MaxLength::non_neg_integer(), Ellipsis::binary()) -> binary().
abbrev(<<Bin/binary>>, MaxLength, <<Ellipsis/binary>>) when is_integer(MaxLength), MaxLength >= 0 ->
    case byte_size(Bin) =< MaxLength of
        true  -> Bin;
        false ->
            EllipsisSize = byte_size(Ellipsis),
            TruncateSize = max(0, MaxLength - EllipsisSize),
            <<(binary:part(Bin, 0, TruncateSize))/binary, Ellipsis/binary>>
    end.

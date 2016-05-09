%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_fluentd_layout_json_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a layout instance",
      fun () ->
              Layout = logi_fluentd_layout_json:new(),
              ?assert(logi_layout:is_layout(Layout))
      end}
    ].

format_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi_fluentd), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Formats log messages",
       fun () ->
               Caller = self(),
               Layout = logi_fluentd_layout_json:new(),
               Sink =
                   logi_builtin_sink_fun:new(
                     test_sink,
                     fun (Context, Format, Data) ->
                             FormattedData = iolist_to_binary(logi_layout:format(Context, Format, Data, Layout)),
                             Caller ! {formatted_data, FormattedData},
                             FormattedData
                     end),
               {ok, _} = logi_channel:install_sink(Sink, info),

               logi:info("Hello ~s", ["World!"], [{headers, #{id => 123}}]),
               receive
                   {formatted_data, Data} ->
                       Node = atom_to_binary(node(), utf8),
                       Module = atom_to_binary(?MODULE, utf8),
                       Process = list_to_binary(pid_to_list(Caller)),
                       ?assertMatch(
                          #{
                             <<"node">> := Node,
                             <<"application">> := <<"undefined">>,
                             <<"module">> := Module,
                             <<"function">> := <<"format_test_">>,
                             <<"line">> := 38,
                             <<"message">> := <<"Hello World!">>,
                             <<"process">> := Process,
                             <<"severity">> := <<"info">>,
                             <<"timestamp">> := _,
                             <<"id">> := 123
                           },
                          jsone:decode(Data))
               after 10 -> ?assert(timeout)
               end
       end}
     ]}.

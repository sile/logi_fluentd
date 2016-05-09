%% @copyright 2015-2016 Takeru Ohta <phjgt308@gmail.com>
%% @end
-module(logi_fluentd_sink_tcp_tests).

-include_lib("eunit/include/eunit.hrl").

%%----------------------------------------------------------------------------------------------------------------------
%% Unit Tests
%%----------------------------------------------------------------------------------------------------------------------
new_test_() ->
    [
     {"Creates a sink",
      fun () ->
              Sink = logi_fluentd_sink_tcp:new(test_sink, "localhost"),
              ?assert(logi_sink:is_sink(Sink))
      end}
    ].

write_test_() ->
    {foreach,
     fun () -> {ok, Apps} = application:ensure_all_started(logi_fluentd), Apps end,
     fun (Apps) -> lists:foreach(fun application:stop/1, Apps) end,
     [
      {"Writes log messages",
       fun () ->
               Port = find_free_port(),
               ok = start_echo_tcp_server(Port),
               Sink = logi_fluentd_sink_tcp:new(test_sink, "localhost", [{port, Port}]),
               {ok, _} = logi_channel:install_sink(Sink, info),

               logi:info("Hello ~s", ["World!"], [{headers, #{id => 123}}]),
               receive
                   {echo, Data} ->
                       Node = atom_to_binary(node(), utf8),
                       Module = atom_to_binary(?MODULE, utf8),
                       Process = list_to_binary(pid_to_list(self())),
                       ?assertMatch(
                          #{
                             <<"node">> := Node,
                             <<"application">> := <<"undefined">>,
                             <<"module">> := Module,
                             <<"function">> := <<"write_test_">>,
                             <<"line">> := 31,
                             <<"message">> := <<"Hello World!">>,
                             <<"process">> := Process,
                             <<"severity">> := <<"info">>,
                             <<"timestamp">> := _,
                             <<"id">> := 123
                           },
                          jsone:decode(Data))
               after 10 -> ?assert(timeout)
               end
       end},
      {"Connect failure",
       fun () ->
               UnusedPort = find_free_port(),
               Sink = logi_fluentd_sink_tcp:new(test_sink, "localhost", [{port, UnusedPort}]),
               ?assertMatch({error, _}, logi_channel:install_sink(Sink, info))
       end}
     ]}.

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Server
%%----------------------------------------------------------------------------------------------------------------------
-spec start_echo_tcp_server(inet:port_number()) -> ok.
start_echo_tcp_server(Port) ->
    Parent = self(),
    _ = spawn_link(
          fun () ->
                  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
                  Parent ! ready,
                  (fun Loop () ->
                           {ok, Socket} = gen_tcp:accept(ListenSocket),
                           {ok, Data} = gen_tcp:recv(Socket, 0),
                           Parent ! {echo, Data},
                           Loop()
                   end)()
          end),
    receive ready -> ok end,
    timer:sleep(10),
    ok.

-spec find_free_port() -> inet:port_number().
find_free_port() ->
    {ok, Socket} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.

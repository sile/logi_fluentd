%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Supervisor for Backend Processes
%% @private
-module(logi_fluentd_backend_sup).

-behaviour(supervisor).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_link/0, start_child/3, stop_child/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback API
%%------------------------------------------------------------------------------------------------------------------------
-export([init/1]).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Starts root supervisor
-spec start_link() -> {ok, pid()} | {error, Reason::term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start Child Process
-spec start_child(logi_fluentd:backend_id(), logi_fluentd:host(), logi_fluentd:backend_options()) ->
                         {ok, pid()} | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_child(BackendId, FluentdHost, Options) ->
    Backend = logi_fluentd_backend,
    Child = {BackendId, {Backend, start_link, [BackendId, FluentdHost, Options]}, permanent, 5000, worker, [Backend]},
    supervisor:start_child(?MODULE, Child).

%% @doc Stop Child Process
-spec stop_child(logi_fluentd:backend_id()) -> ok.
stop_child(BackendId) ->
    _ = supervisor:terminate_child(?MODULE, BackendId),
    _ = supervisor:delete_child(?MODULE, BackendId),
    ok.

%%------------------------------------------------------------------------------------------------------------------------
%% 'supervisor' Callback Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @hidden
init([]) ->
    {ok, { {one_for_one, 5, 10}, []}}.

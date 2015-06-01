%% @copyright 2015 Takeru Ohta <phjgt308@gmail.com>
%%
%% @doc Fluentd Backend Interface
%%
%% TODO: 他の入力プラグインに対応
-module(logi_fluentd).

%%------------------------------------------------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------------------------------------------------
-export([start_backend/3, stop_backend/1]).
-export([install/2, install/3, install_opt/3, install_opt/4, uninstall/1, uninstall/2]).

-export_type([backend_id/0]).
-export_type([backend_option/0, backend_options/0]).
-export_type([install_option/0, install_options/0]).
-export_type([host/0]).

%%------------------------------------------------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------------------------------------------------
-type backend_id() :: atom().
-type host() :: inet:ip_address() | inet:hostname().

-type backend_options() :: [backend_option()].
-type backend_option() :: {port, inet:port_number()}
                        | {connect_timeout, timeout()}.

-type install_options() :: [install_option()].
-type install_option() :: {formatter, logi_fluentd_formatter:formatter()}. % default: `logi_fluentd_formatter_default:make()'

%%------------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%------------------------------------------------------------------------------------------------------------------------
%% @doc Fluentd出力用のバックエンドを起動する
-spec start_backend(backend_id(), host(), backend_options()) -> ok | {error, Reason} when
      Reason :: {already_started, pid()} | term().
start_backend(BackendId, FluentdHost, Options) ->
    logi_fluentd_backend:start(BackendId, FluentdHost, Options).

%% @doc バックエンドを停止する
%%
%% 対象バックエンドが未起動の場合は、エラーとならずに単に無視される
-spec stop_backend(backend_id()) -> ok.
stop_backend(BackendId) ->
    logi_fluentd_backend:stop(BackendId).

%% @equiv uninstall(logi:default_logger(), BackendId)
-spec uninstall(backend_id()) -> ok.
uninstall(BackendId) ->
    uninstall(logi:default_logger(), BackendId).

%% @equiv install(logi:default_logger(), ConditionSpec, BackendId)
-spec install(logi_condition:spec(), backend_id()) -> ok.
install(ConditionSpec, BackendId) ->
    install(logi:default_logger(), ConditionSpec, BackendId).

%% @equiv install_opt(Logger, ConditionSpec, BackendId, [])
-spec install(logi:logger(), logi_condition:spec(), backend_id()) -> ok.
install(Logger, ConditionSpec, BackendId) ->
    install_opt(Logger, ConditionSpec, BackendId, []).

%% @equiv install_opt(logi:default_logger(), ConditionSpec, BackendId, [])
-spec install_opt(logi_condition:spec(), backend_id(), install_options()) -> ok.
install_opt(ConditionSpec, BackendId, Options) ->
    install_opt(logi:default_logger(), ConditionSpec, BackendId, Options).

%% @doc Fluentd出力用のログバックエンドをLoggerに登録する
%%
%% 既に登録済みの場合は、内容が更新される.
%%
%% なおバックエンドを登録しても、そのバックエンドが{@link start_backend/3}を使って起動されていない場合は、
%% ログが出力されないので注意が必要。
-spec install_opt(logi:logger(), logi_condition:spec(), backend_id(), install_options()) -> ok.
install_opt(Logger, ConditionSpec, BackendId, Options) when is_atom(BackendId) ->
    Formatter = proplists:get_value(formatter, Options, logi_fluentd_formatter_default:make()),
    logi:set_backend(Logger, {BackendId, logi_fluentd_backend, Formatter}, ConditionSpec);
install_opt(Logger, ConditionSpec, BackendId, Options) ->
    error(badarg, [Logger, ConditionSpec, BackendId, Options]).

%% @doc バックエンドの登録を解除する
%%
%% バックエンドが未登録の場合は、エラーとはならずに単に無視される.
%%
%% また、登録を解除してもバックエンドプロセスは自動では停止しないので、
%% 必要であれば明示的に{@link stop_backend/1}を呼び出す必要がある。
-spec uninstall(logi:logger(), backend_id()) -> ok.
uninstall(Logger, BackendId) ->
    logi:delete_backend(Logger, BackendId).

%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ Management Plugin.
%%
%%   The Initial Developer of the Original Code is VMware, Inc.
%%   Copyright (c) 2007-2010 VMware, Inc.  All rights reserved.
-module(rabbit_mgmt_app).

-behaviour(application).
-export([start/2, stop/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-define(PREFIX, "api").
-define(UI_PREFIX, "mgmt").
-define(CLI_PREFIX, "cli").

%% Make sure our database is hooked in *before* listening on the network or
%% recovering queues (i.e. so there can't be any events fired before it starts).
-rabbit_boot_step({rabbit_mgmt_database,
                   [{description, "management statistics database"},
                    {mfa,         {rabbit_sup, start_child,
                                   [rabbit_mgmt_global_sup]}},
                    {requires,    rabbit_event},
                    {enables,     queue_sup_queue_recovery}]}).

start(_Type, _StartArgs) ->
    log_startup(),
    setup_wm_logging(),
    rabbit_mgmt_sup:start_link().

stop(_State) ->
    ok.

setup_wm_logging() ->
    {ok, LogDir} = application:get_env(rabbit_management, http_log_dir),
    case LogDir of
        none ->
            ok;
        _ ->
            application:set_env(webmachine, webmachine_logger_module,
                                webmachine_logger),
            webmachine_sup:start_logger(LogDir)
    end.

log_startup() ->
    {ok, Hostname} = inet:gethostname(),
    URLPrefix = "http://" ++ Hostname ++ ":" ++ integer_to_list(get_port()),
    rabbit_log:info(
      "Management plugin started.~n"
      ++ "HTTP API:       ~s/~s/~n"
      ++ "Management UI:  ~s/~s/~n",
      [URLPrefix, ?PREFIX, URLPrefix, ?UI_PREFIX]).

get_port() ->
    {ok, P} = application:get_env(port),
    P.

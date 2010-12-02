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
%%   The Original Code is RabbitMQ Management Console.
%%
%%   The Initial Developers of the Original Code are Rabbit Technologies Ltd.
%%
%%   Copyright (C) 2010 Rabbit Technologies Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%
-module(rabbit_mgmt_mochiweb).

-export([start/0, stop/0]).

-define(PREFIX, "api").

start() ->
    application:set_env(
      webmachine, dispatch_list,
      [{[?PREFIX | Path], F, A} ||
          {Path, F, A} <- rabbit_mgmt_dispatcher:dispatcher()]),
    application:set_env(webmachine, error_handler, webmachine_error_handler),
    {ok, Port} = application:get_env(port),
    mochiweb_http:start([{name, ?MODULE}, {port, Port}, {loop, fun loop/1}]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req) ->
    Unauthorized = {401, [{"WWW-Authenticate",
                           "Basic realm=\"RabbitMQ Management\""}], ""},
    case rabbit_mgmt_util:login(Req:get_header_value("authorization"),
                                fun(_) -> true end) of
        {ok, _User} -> handle(Req);
        _           -> Req:respond(Unauthorized)
    end.

handle(Req) ->
    "/" ++ Path = Req:get(raw_path),
    case Path of
        "api/" ++ _ ->
            webmachine_mochiweb:loop(Req);
        "mgmt/" ++ Tail ->
            %% Redirect URLs from when we used rabbitmq-mochiweb.
            Req:respond({301, [{"Location", "/" ++ Tail}], ""});
        _    ->
            {file, Here} = code:is_loaded(?MODULE),
            ModuleRoot = filename:dirname(filename:dirname(Here)),
            LocalPath = filename:join(ModuleRoot, "priv/www"),
            Req:serve_file(Path, LocalPath)
    end.

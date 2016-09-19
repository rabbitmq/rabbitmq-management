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
%%   The Initial Developer of the Original Code is GoPivotal, Inc.
%%   Copyright (c) 2010-2015 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_wm_redirect).
-export([init/3, handle/2, terminate/2]).

init(_, Req, RedirectTo) ->
    {ok, Req, RedirectTo}.

handle(Req0, RedirectTo) ->
    {ok, Req} = cowboy_req:reply(301, [{<<"location">>, RedirectTo}], Req0),
    {ok, Req, RedirectTo}.

terminate(_, _) ->
    ok.

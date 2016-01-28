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
%%   Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_wm_definitions).

-export([init/3, rest_init/2, to_json/2, content_types_provided/2, is_authorized/2]).
-export([content_types_accepted/2, allowed_methods/2, accept_json/2]).
-export([accept_multipart/2]).

-export([apply_defs/3]).

-import(rabbit_misc, [pget/2, pget/3]).

-include("rabbit_mgmt.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Config) -> {ok, Req, #context{}}.

content_types_provided(ReqData, Context) ->
   {[{<<"application/json">>, to_json}], ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{<<"application/json">>, accept_json},
     {{<<"multipart">>, <<"form-data">>, '*'}, accept_multipart}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
    {[<<"HEAD">>, <<"GET">>, <<"POST">>], ReqData, Context}.

to_json(ReqData, Context) ->
    Xs = [X || X <- rabbit_mgmt_wm_exchanges:basic(ReqData),
               export_exchange(X)],
    Qs = [Q || Q <- rabbit_mgmt_wm_queues:basic(ReqData),
               export_queue(Q)],
    QNames = [{pget(name, Q), pget(vhost, Q)} || Q <- Qs],
    Bs = [B || B <- rabbit_mgmt_wm_bindings:basic(ReqData),
               export_binding(B, QNames)],
    {ok, Vsn} = application:get_key(rabbit, vsn),
    rabbit_mgmt_util:reply(
      [{rabbit_version, list_to_binary(Vsn)}] ++
      filter(
        [{users,       rabbit_mgmt_wm_users:users()},
         {vhosts,      rabbit_mgmt_wm_vhosts:basic()},
         {permissions, rabbit_mgmt_wm_permissions:permissions()},
         {parameters,  rabbit_mgmt_wm_parameters:basic(ReqData)},
         {policies,    rabbit_mgmt_wm_policies:basic(ReqData)},
         {queues,      Qs},
         {exchanges,   Xs},
         {bindings,    Bs}]),
      case cowboy_req:qs_val(<<"download">>, ReqData) of
          {undefined, _} -> ReqData;
          {Filename, _}  -> rabbit_mgmt_util:set_resp_header(
                         "Content-Disposition",
                         "attachment; filename=" ++
                             binary_to_list(Filename), ReqData)
      end,
      Context).

accept_json(ReqData0, Context) ->
    {ok, Body, ReqData} = cowboy_req:body(ReqData0),
    accept(Body, ReqData, Context).

accept_multipart(ReqData0, Context) ->
    {Parts, ReqData} = get_all_parts(ReqData0),
    Redirect = get_part(<<"redirect">>, Parts),
    Json = get_part(<<"file">>, Parts),
    Resp = {Res, _, _} = accept(Json, ReqData, Context),
    case Res of
        true ->
            ReqData1 =
                case Redirect of
                    unknown -> ReqData;
                    _       -> cowboy_req:set_resp_header(<<"location">>, Redirect, ReqData)
                end,
            {true, ReqData1, Context};
        _ ->
            Resp
    end.

is_authorized(ReqData, Context) ->
    case cowboy_req:qs_val(<<"auth">>, ReqData) of
        {undefined, _} -> rabbit_mgmt_util:is_authorized_admin(ReqData, Context);
        {Auth, _}      -> is_authorized_qs(ReqData, Context, Auth)
    end.

%% Support for the web UI - it can't add a normal "authorization"
%% header for a file download.
is_authorized_qs(ReqData, Context, Auth) ->
    case rabbit_web_dispatch_util:parse_auth_header("Basic " ++ Auth) of
        [Username, Password] -> rabbit_mgmt_util:is_authorized_admin(
                                  ReqData, Context, Username, Password);
        _                    -> {?AUTH_REALM, ReqData, Context}
    end.

%%--------------------------------------------------------------------

accept(Body, ReqData, Context) ->
    apply_defs(Body, fun() -> {true, ReqData, Context} end,
               fun(E) -> rabbit_mgmt_util:bad_request(E, ReqData, Context) end).

apply_defs(Body, SuccessFun, ErrorFun) ->
    case rabbit_mgmt_util:decode([], Body) of
        {error, E} ->
            ErrorFun(E);
        {ok, _, All} ->
            Version = pget(rabbit_version, All),
            try
                for_all(users,       All, fun(User) -> 
                                              rabbit_mgmt_wm_user:put_user(
                                                  User, 
                                                  Version) 
                                          end),
                for_all(vhosts,      All, fun add_vhost/1),
                for_all(permissions, All, fun add_permission/1),
                for_all(parameters,  All, fun add_parameter/1),
                for_all(policies,    All, fun add_policy/1),
                for_all(queues,      All, fun add_queue/1),
                for_all(exchanges,   All, fun add_exchange/1),
                for_all(bindings,    All, fun add_binding/1),
                SuccessFun()
            catch {error, E} -> ErrorFun(format(E));
                  exit:E     -> ErrorFun(format(E))
            end
    end.

format(#amqp_error{name = Name, explanation = Explanation}) ->
    list_to_binary(rabbit_misc:format("~s: ~s", [Name, Explanation]));
format(E) ->
    list_to_binary(rabbit_misc:format("~p", [E])).

get_all_parts(ReqData) ->
    get_all_parts(ReqData, []).

get_all_parts(ReqData0, Acc) ->
    case cowboy_req:part(ReqData0) of
        {done, ReqData} ->
            {Acc, ReqData};
        {ok, Headers, ReqData1} ->
            Name = case cow_multipart:form_data(Headers) of
                {data, N} -> N;
                {file, N, _, _, _} -> N
            end,
            {ok, Body, ReqData} = cowboy_req:part_body(ReqData1),
            get_all_parts(ReqData, [{Name, Body}|Acc])
    end.

get_part(Name, Parts) ->
    case lists:keyfind(Name, 1, Parts) of
        false -> unknown;
        {_, Value} -> Value
    end.

export_queue(Queue) ->
    pget(owner_pid, Queue) == none.

export_binding(Binding, Qs) ->
    Src      = pget(source,           Binding),
    Dest     = pget(destination,      Binding),
    DestType = pget(destination_type, Binding),
    VHost    = pget(vhost,            Binding),
    Src =/= <<"">>
        andalso
          ( (DestType =:= queue andalso lists:member({Dest, VHost}, Qs))
            orelse (DestType =:= exchange andalso Dest =/= <<"">>) ).

export_exchange(Exchange) ->
    export_name(pget(name, Exchange)).

export_name(<<>>)                 -> false;
export_name(<<"amq.", _/binary>>) -> false;
export_name(_Name)                -> true.

%%--------------------------------------------------------------------

rw_state() ->
    [{users,       [name, password_hash, hashing_algorithm, tags]},
     {vhosts,      [name]},
     {permissions, [user, vhost, configure, write, read]},
     {parameters,  [vhost, component, name, value]},
     {policies,    [vhost, name, pattern, definition, priority, 'apply-to']},
     {queues,      [name, vhost, durable, auto_delete, arguments]},
     {exchanges,   [name, vhost, type, durable, auto_delete, internal,
                    arguments]},
     {bindings,    [source, vhost, destination, destination_type, routing_key,
                    arguments]}].

filter(Items) ->
    [filter_items(N, V, proplists:get_value(N, rw_state())) || {N, V} <- Items].

filter_items(Name, List, Allowed) ->
    {Name, [filter_item(I, Allowed) || I <- List]}.

filter_item(Item, Allowed) ->
    [{K, Fact} || {K, Fact} <- Item, lists:member(K, Allowed)].

%%--------------------------------------------------------------------

for_all(Name, All, Fun) ->
    case pget(Name, All) of
        undefined -> ok;
        List      -> [Fun([{atomise_name(K), V} || {K, V} <- I]) ||
                         {struct, I} <- List]
    end.

atomise_name(N) -> list_to_atom(binary_to_list(N)).

%%--------------------------------------------------------------------

add_parameter(Param) ->
    VHost = pget(vhost,     Param),
    Comp  = pget(component, Param),
    Key   = pget(name,      Param),
    Term  = rabbit_misc:json_to_term(pget(value, Param)),
    case rabbit_runtime_parameters:set(VHost, Comp, Key, Term, none) of
        ok                -> ok;
        {error_string, E} -> S = rabbit_misc:format(" (~s/~s/~s)",
                                                    [VHost, Comp, Key]),
                             exit(list_to_binary(E ++ S))
    end.

add_policy(Param) ->
    VHost = pget(vhost, Param),
    Key   = pget(name,  Param),
    case rabbit_policy:set(
           VHost, Key, pget(pattern, Param),
           rabbit_misc:json_to_term(pget(definition, Param)),
           pget(priority, Param),
           pget('apply-to', Param, <<"all">>)) of
        ok                -> ok;
        {error_string, E} -> S = rabbit_misc:format(" (~s/~s)", [VHost, Key]),
                             exit(list_to_binary(E ++ S))
    end.

add_vhost(VHost) ->
    VHostName = pget(name, VHost),
    VHostTrace = pget(tracing, VHost),
    rabbit_mgmt_wm_vhost:put_vhost(VHostName, VHostTrace).

add_permission(Permission) ->
    rabbit_auth_backend_internal:set_permissions(pget(user,      Permission),
                                                 pget(vhost,     Permission),
                                                 pget(configure, Permission),
                                                 pget(write,     Permission),
                                                 pget(read,      Permission)).

add_queue(Queue) ->
    rabbit_amqqueue:declare(r(queue,                              Queue),
                            pget(durable,                         Queue),
                            pget(auto_delete,                     Queue),
                            rabbit_mgmt_util:args(pget(arguments, Queue)),
                            none).

add_exchange(Exchange) ->
    Internal = case pget(internal, Exchange) of
                   undefined -> false; %% =< 2.2.0
                   I         -> I
               end,
    rabbit_exchange:declare(r(exchange,                           Exchange),
                            rabbit_exchange:check_type(pget(type, Exchange)),
                            pget(durable,                         Exchange),
                            pget(auto_delete,                     Exchange),
                            Internal,
                            rabbit_mgmt_util:args(pget(arguments, Exchange))).

add_binding(Binding) ->
    DestType = list_to_atom(binary_to_list(pget(destination_type, Binding))),
    rabbit_binding:add(
      #binding{source       = r(exchange, source,                   Binding),
               destination  = r(DestType, destination,              Binding),
               key          = pget(routing_key,                     Binding),
               args         = rabbit_mgmt_util:args(pget(arguments, Binding))}).

r(Type, Props) -> r(Type, name, Props).

r(Type, Name, Props) ->
    rabbit_misc:r(pget(vhost, Props), Type, pget(Name, Props)).

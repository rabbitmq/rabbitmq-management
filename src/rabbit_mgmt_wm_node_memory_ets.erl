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
%%   The Initial Developer of the Original Code is GoPivotal, Inc.
%%   Copyright (c) 2007-2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_wm_node_memory_ets).

-export([init/3, rest_init/2, to_json/2, content_types_provided/2,
         is_authorized/2]).
-export([resource_exists/2]).
-export([variances/2]).

-include("rabbit_mgmt.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

%%--------------------------------------------------------------------

init(_, _, _) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [Mode]) ->
    {ok, rabbit_mgmt_cors:set_headers(Req, ?MODULE), {Mode, #context{}}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

content_types_provided(ReqData, Context) ->
   {[{<<"application/json">>, to_json}], ReqData, Context}.

resource_exists(ReqData, Context) ->
    {node_exists(ReqData, get_node(ReqData)), ReqData, Context}.

to_json(ReqData, {Mode, Context}) ->
    rabbit_mgmt_util:reply(augment(Mode, ReqData), ReqData, {Mode, Context}).

is_authorized(ReqData, {Mode, Context}) ->
    {Res, RD, C} = rabbit_mgmt_util:is_authorized_monitor(ReqData, Context),
    {Res, RD, {Mode, C}}.

%%--------------------------------------------------------------------
get_node(ReqData) ->
    list_to_atom(binary_to_list(rabbit_mgmt_util:id(node, ReqData))).

get_filter(ReqData) ->
    case rabbit_mgmt_util:id(filter, ReqData) of
        none                        -> all;
        <<"management">>            -> rabbit_mgmt_event_collector;
        Other when is_binary(Other) -> list_to_atom(binary_to_list(Other));
        _                           -> all
    end.

node_exists(ReqData, Node) ->
    case [N || N <- rabbit_mgmt_wm_nodes:all_nodes(ReqData),
               proplists:get_value(name, N) == Node] of
        [] -> false;
        [_] -> true
    end.

augment(Mode, ReqData) ->
    Node = get_node(ReqData),
    Filter = get_filter(ReqData),
    case node_exists(ReqData, Node) of
        false ->
            not_found;
        true ->
            case rpc:call(Node, rabbit_vm, ets_tables_memory,
                          [Filter], infinity) of
                {badrpc, _} -> [{ets_tables_memory, not_available}];
                []          -> [{ets_tables_memory, no_tables}];
                Result      -> [{ets_tables_memory, format(Mode, Result)}]
            end
    end.

format(absolute, Result) ->
    Total = lists:sum([V || {_K,V} <- Result]),
    [{total, Total} | Result];
format(relative, Result) ->
    Total = lists:sum([V || {_K,V} <- Result]),
    [{total, 100} | [{K, percentage(V, Total)} || {K, V} <- Result]].

percentage(Part, Total) ->
    case round((Part/Total) * 100) of
        0 when Part =/= 0 ->
            1;
        Int ->
            Int
    end.
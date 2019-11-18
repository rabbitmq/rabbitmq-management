%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is RabbitMQ Management Plugin.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2019 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_wm_auth).

-export([init/2, to_json/2, content_types_provided/2, is_authorized/2]).
-export([variances/2]).

-include_lib("rabbitmq_management_agent/include/rabbit_mgmt_records.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

%%--------------------------------------------------------------------

init(Req, _State) ->
    {cowboy_rest, rabbit_mgmt_headers:set_common_permission_headers(Req, ?MODULE), #context{}}.

variances(Req, Context) ->
    {[<<"accept-encoding">>, <<"origin">>], Req, Context}.

content_types_provided(ReqData, Context) ->
   {rabbit_mgmt_util:responder_map(to_json), ReqData, Context}.

to_json(ReqData, Context) ->
    EnableOAuth2 = application:get_env(rabbitmq_management, enable_oauth2, false),
    OAuth2Implementation = application:get_env(rabbitmq_management, oauth2_implementation, uaa),
    Data = case EnableOAuth2 of
               true ->
                   OAuth2ClientId = application:get_env(rabbitmq_management, oauth2_client_id, ""),
                   OAuth2Location = application:get_env(rabbitmq_management, oauth2_location, ""),
                   OAuth2Scopes = application:get_env(rabbitmq_management, oauth2_scopes, ""),
                   case is_invalid([OAuth2ClientId, OAuth2Location, OAuth2Scopes]) of
                       true ->
                           rabbit_log:warning("Disabling OAuth 2 authorization, relevant configuration settings are missing", []),
                           [{enable_oauth2, false}, {oauth2_client_id, <<>>}, {oauth2_location, <<>>}];
                       false ->
                           [{enable_oauth2, true},
                            {oauth2_client_id, rabbit_data_coercion:to_binary(OAuth2ClientId)},
                            {oauth2_location, rabbit_data_coercion:to_binary(OAuth2Location)},
                            {oauth2_scopes, rabbit_data_coercion:to_binary(OAuth2Scopes)},
                            {oauth2_implementation, rabbit_data_coercion:to_binary(OAuth2Implementation)}]
                   end;
               false ->
                   [{enable_oauth2, false}, {oauth2_client_id, <<>>}, {oauth2_location, <<>>}]
           end,
    rabbit_mgmt_util:reply(Data, ReqData, Context).

is_authorized(ReqData, Context) ->
    {true, ReqData, Context}.

is_invalid(List) ->
    lists:any(fun(V) -> V == "" end, List).

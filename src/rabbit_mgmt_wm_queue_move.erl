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
%%   Copyright (c) 2010-2014 GoPivotal, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_wm_queue_move).

-export([init/1, resource_exists/2, post_is_create/2, is_authorized/2,
         allowed_methods/2, process_post/2]).

-include("rabbit_mgmt.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%%--------------------------------------------------------------------

init(_Config) -> {ok, #context{}}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

resource_exists(ReqData, Context) ->
	VHost = rabbit_mgmt_util:id(vhost, ReqData),
	SourceQueueExists = case rabbit_mgmt_wm_queue:queue(VHost, rabbit_mgmt_util:id(queue, ReqData)) of
         not_found -> false;
         _         -> true
    end,
    DestinationQueueExists = case rabbit_mgmt_wm_queue:queue(VHost, rabbit_mgmt_util:id(destinationQueue, ReqData)) of
         not_found -> false;
         _         -> true
    end,

    if
    	SourceQueueExists, DestinationQueueExists ->
    		{true, ReqData, Context};
    	true ->
    		{false, ReqData, Context}
    end.

post_is_create(ReqData, Context) ->
    {false, ReqData, Context}.

process_post(ReqData, Context) ->
    rabbit_mgmt_util:post_respond(do_it(ReqData, Context)).

do_it(ReqData, Context) ->
	rabbit_mgmt_util:with_decode(
      	[vhost, queue, destinationQueue], ReqData, Context,
      	fun([VHost, SourceName, DestinationName], _Body) ->
      		rabbit_mgmt_util:with_channel(
                VHost, ReqData, Context,
                fun (Ch) ->
                    SourceQueue = rabbit_mgmt_wm_queue:queue(VHost, SourceName),

					[SourceQueueStats] = rabbit_mgmt_db:augment_queues([SourceQueue], rabbit_mgmt_util:range_ceil(ReqData), full),
					MessageCount = proplists:get_value(messages, SourceQueueStats),

					move_messages(MessageCount, Ch, SourceName, DestinationName),

		          	rabbit_mgmt_util:reply([{messages, MessageCount}], ReqData, Context)
                end)
      	end).

move_messages(0, Ch, SourceName, DestinationName) ->
	none;
move_messages(MessageCount, Ch, SourceName, DestinationName) ->
	Message = case amqp_channel:call(Ch, #'basic.get'{queue = SourceName, no_ack = false}) of
        {#'basic.get_ok'{delivery_tag = Tag}, Content} ->
        	{_, Props, Payload} = Content,
        	case rabbit_mgmt_wm_exchange_publish:publish_msg(Ch, <<"">>, DestinationName, Props, Payload) of
        		{good, true} ->
        			amqp_channel:call(Ch, #'basic.ack'{ delivery_tag = Tag });
        		{good, false} ->
        			amqp_channel:call(Ch, #'basic.ack'{ delivery_tag = Tag });
        		{down, Err} ->
        			Err
        	end;
        #'basic.get_empty'{} ->
            none
    end,

	move_messages(MessageCount - 1, Ch, SourceName, DestinationName).

is_authorized(ReqData, Context) ->
    rabbit_mgmt_util:is_authorized_vhost(ReqData, Context).

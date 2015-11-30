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

-module(rabbit_mgmt_db).

-include("rabbit_mgmt.hrl").
-include("rabbit_mgmt_metrics.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(gen_server2).

-export([start_link/0]).
-export([pget/2, id_name/1, id/2, lookup_element/2]).

-export([augment_exchanges/3, augment_queues/3,
         augment_nodes/2, augment_vhosts/2,
         get_channel/2, get_connection/2,
         get_all_channels/1, get_all_connections/1,
         get_all_consumers/0, get_all_consumers/1,
         get_overview/2, get_overview/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, handle_pre_hibernate/1,
         format_message_queue/2]).

-import(rabbit_misc, [pget/3]).

%% The management database listens to events broadcast via the
%% rabbit_event mechanism, and responds to queries from the various
%% rabbit_mgmt_wm_* modules. It handles several kinds of events, and
%% slices and dices them in various ways.
%%
%% There are three types of events coming in: created (when an object
%% is created, containing immutable facts about it), stats (emitted on
%% a timer, with mutable facts about the object), and deleted (just
%% containing the object's ID). In this context "objects" means
%% connections, channels, exchanges, queues, consumers, vhosts and
%% nodes. Note that we do not care about users, permissions, bindings,
%% parameters or policies.
%%
%% Connections and channels are identified by pids. Queues and
%% exchanges are identified by names (which are #resource{}s). VHosts
%% and nodes are identified by names which are binaries. And consumers
%% are identified by {ChPid, QName, CTag}.
%%
%% The management database records the "created" events for
%% connections, channels and consumers, and can thus be authoritative
%% about those objects. For queues, exchanges and nodes we go to
%% Mnesia to find out the immutable details of the objects.
%%
%% For everything other than consumers, the database can then augment
%% these immutable details with stats, as the object changes. (We
%% never emit anything very interesting about consumers).
%%
%% Stats on the inbound side are referred to as coarse- and
%% fine-grained. Fine grained statistics are the message rates
%% maintained by channels and associated with tuples: {publishing
%% channel, exchange}, {publishing channel, exchange, queue} and
%% {queue, consuming channel}. Coarse grained stats are everything
%% else and are associated with only one object, not a tuple.
%%
%% Within the management database though we rearrange things a bit: we
%% refer to basic stats, simple stats and detail stats.
%%
%% Basic stats are those coarse grained stats for which we do not
%% retain a history and do not perform any calculations -
%% e.g. connection.state or channel.prefetch_count.
%%
%% Simple stats are those for which we do history / calculations which
%% are associated with one object *after aggregation* - so these might
%% originate with coarse grained stats - e.g. connection.send_oct or
%% queue.messages_ready. But they might also originate from fine
%% grained stats which have been aggregated - e.g. the message rates
%% for a vhost or queue.
%%
%% Finally, detailed stats are those for which we do history /
%% calculations which are associated with two objects. These
%% have to have originated as fine grained stats, but can still have
%% been aggregated.
%%
%% Created events and basic stats are stored in ETS tables by object.
%% Simple and detailed stats (which only differ depending on how
%% they're keyed) are stored in aggregated_stats.
%%
%% For detailed stats we also store an index for each object referencing
%% all the other objects that form a detailed stats key with it. This is
%% so that we can always avoid table scanning while deleting stats and
%% thus make sure that handling deleted events is O(n)-ish.
%%
%% For each key for simple and detailed stats we maintain a #stats{}
%% record, essentially a base counter for everything that happened
%% before the samples we have kept, and a gb_tree of {timestamp,
%% sample} values.
%%
%% We also have old_stats to let us calculate instantaneous
%% rates, in order to apportion simple / detailed stats into time
%% slices as they come in. These instantaneous rates are not returned
%% in response to any query, the rates shown in the API are calculated
%% at query time. old_stats contains both coarse and fine
%% entries. Coarse entries are pruned when the corresponding object is
%% deleted, and fine entries are pruned when the emitting channel is
%% closed, and whenever we receive new fine stats from a channel. So
%% it's quite close to being a cache of "the previous stats we
%% received".
%%
%% We also keep a timer going, in order to prune old samples from
%% aggregated_stats.
%%
%% Overall the object is to do all the aggregation when events come
%% in, and make queries be simple lookups as much as possible. One
%% area where this does not happen is the global overview - which is
%% aggregated from vhost stats at query time since we do not want to
%% reveal anything about other vhosts to unprivileged users.

%%----------------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------------

start_link() ->
    case gen_server2:start_link({global, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> register(?MODULE, Pid), %% [1]
                     {ok, Pid};
        Else      -> Else
    end.
%% [1] For debugging it's helpful to locally register the name too
%% since that shows up in places global names don't.

%% R = Ranges, M = Mode
augment_exchanges(Xs, R, M) -> safe_call({augment_exchanges, Xs, R, M}, Xs).
augment_queues(Qs, R, M)    -> safe_call({augment_queues, Qs, R, M}, Qs).
augment_vhosts(VHosts, R)   -> safe_call({augment_vhosts, VHosts, R}, VHosts).
augment_nodes(Nodes, R)     -> safe_call({augment_nodes, Nodes, R}, Nodes).

get_channel(Name, R)        -> safe_call({get_channel, Name, R}, not_found).
get_connection(Name, R)     -> safe_call({get_connection, Name, R}, not_found).

get_all_channels(R)         -> safe_call({get_all_channels, R}).
get_all_connections(R)      -> safe_call({get_all_connections, R}).

get_all_consumers()         -> safe_call({get_all_consumers, all}).
get_all_consumers(V)        -> safe_call({get_all_consumers, V}).

get_overview(User, R)       -> safe_call({get_overview, User, R}).
get_overview(R)             -> safe_call({get_overview, all, R}).

safe_call(Term)          -> safe_call(Term, []).
safe_call(Term, Default) -> safe_call(Term, Default, 1).

%% See rabbit_mgmt_sup_sup for a discussion of the retry logic.
safe_call(Term, Default, Retries) ->
    rabbit_misc:with_exit_handler(
      fun () ->
              case Retries of
                  0 -> Default;
                  _ -> rabbit_mgmt_sup_sup:start_child(),
                       safe_call(Term, Default, Retries - 1)
              end
      end,
      fun () -> gen_server2:call({global, ?MODULE}, Term, infinity) end).

%%----------------------------------------------------------------------------
%% Internal, gen_server2 callbacks
%%----------------------------------------------------------------------------

-record(state, {interval}).

init([]) ->
    %% When Rabbit is overloaded, it's usually especially important
    %% that the management plugin work.
    process_flag(priority, high),
    {ok, Interval} = application:get_env(rabbit, collect_statistics_interval),
    rabbit_log:info("Statistics database started.~n"),
    {ok, #state{interval = Interval}, hibernate,
     {backoff, ?HIBERNATE_AFTER_MIN, ?HIBERNATE_AFTER_MIN, ?DESIRED_HIBERNATE}}.

handle_call({augment_exchanges, Xs, Ranges, basic}, _From,
            #state{interval = Interval} = State) ->
    reply(list_exchange_stats(Ranges, Xs, Interval), State);

handle_call({augment_exchanges, Xs, Ranges, full}, _From,
            #state{interval = Interval} = State) ->
    reply(detail_exchange_stats(Ranges, Xs, Interval), State);

handle_call({augment_queues, Qs, Ranges, basic}, _From,
            #state{interval = Interval} = State) ->
    reply(list_queue_stats(Ranges, Qs, Interval), State);

handle_call({augment_queues, Qs, Ranges, full}, _From,
            #state{interval = Interval} = State) ->
    reply(detail_queue_stats(Ranges, Qs, Interval), State);

handle_call({augment_vhosts, VHosts, Ranges}, _From,
            #state{interval = Interval} = State) ->
    reply(vhost_stats(Ranges, VHosts, Interval), State);

handle_call({augment_nodes, Nodes, Ranges}, _From,
            #state{interval = Interval} = State) ->
    {reply, node_stats(Ranges, Nodes, Interval), State};

handle_call({get_channel, Name, Ranges}, _From,
            #state{interval = Interval} = State) ->
    case created_event(Name, channel_stats) of
        not_found -> reply(not_found, State);
        Ch        -> [Result] = detail_channel_stats(Ranges, [Ch], Interval),
                     reply(Result, State)
    end;

handle_call({get_connection, Name, Ranges}, _From,
            #state{interval = Interval} = State) ->
    case created_event(Name, connection_stats) of
        not_found -> reply(not_found, State);
        Conn      -> [Result] = connection_stats(Ranges, [Conn], Interval),
                     reply(Result, State)
    end;

handle_call({get_all_channels, Ranges}, _From,
            #state{interval = Interval} = State) ->
    Chans = created_events(channel_stats),
    reply(list_channel_stats(Ranges, Chans, Interval), State);

handle_call({get_all_connections, Ranges}, _From,
            #state{interval = Interval} = State) ->
    Conns = created_events(connection_stats),
    reply(connection_stats(Ranges, Conns, Interval), State);

handle_call({get_all_consumers, VHost}, _From, State) ->
    {reply, [augment_msg_stats(augment_consumer(Obj)) ||
                Obj <- consumers_by_queue_and_vhost(VHost)], State};

handle_call({get_overview, User, Ranges}, _From,
            #state{interval = Interval} = State) ->
    VHosts = case User of
                 all -> rabbit_vhost:list();
                 _   -> rabbit_mgmt_util:list_visible_vhosts(User)
             end,
    %% TODO: there's no reason we can't do an overview of send_oct and
    %% recv_oct now!
    VStats = [read_simple_stats(vhost_stats, VHost) ||
                 VHost <- VHosts],
    MessageStats = [overview_sum(Type, VStats) || Type <- ?MSG_RATES],
    QueueStats = [overview_sum(Type, VStats) || Type <- ?QUEUE_MSG_COUNTS],
    F = case User of
            all -> fun (L) -> length(L) end;
            _   -> fun (L) -> length(rabbit_mgmt_util:filter_user(L, User)) end
        end,
    %% Filtering out the user's consumers would be rather expensive so let's
    %% just not show it
    Consumers = case User of
                    all -> [{consumers, ets:info(consumers_by_queue, size)}];
                    _   -> []
                end,
    ObjectTotals = Consumers ++
        [{queues,      length([Q || V <- VHosts,
                                    Q <- rabbit_amqqueue:list(V)])},
         {exchanges,   length([X || V <- VHosts,
                                    X <- rabbit_exchange:list(V)])},
         {connections, F(created_events(connection_stats))},
         {channels,    F(created_events(channel_stats))}],
    FormatMessage = format_samples(Ranges, MessageStats, Interval),
    FormatQueue = format_samples(Ranges, QueueStats, Interval),
    [rabbit_mgmt_stats:free(S) || {_, S} <- MessageStats],
    [rabbit_mgmt_stats:free(S) || {_, S} <- QueueStats],
    reply([{message_stats, FormatMessage},
           {queue_totals,  FormatQueue},
           {object_totals, ObjectTotals},
           {statistics_db_event_queue,
            rabbit_mgmt_event_collector:get_last_queue_length()}],
          State);

handle_call(_Request, _From, State) ->
    reply(not_understood, State).

handle_cast(_Request, State) ->
    noreply(State).

handle_info(_Info, State) ->
    noreply(State).

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

reply(Reply, NewState) -> {reply, Reply, NewState, hibernate}.
noreply(NewState) -> {noreply, NewState, hibernate}.

handle_pre_hibernate(State) ->
    %% rabbit_event can end up holding on to some memory after a busy
    %% workout, but it's not a gen_server so we can't make it
    %% hibernate. The best we can do is forcibly GC it here (if
    %% rabbit_mgmt_db is hibernating the odds are rabbit_event is
    %% quiescing in some way too).
    rpc:multicall(
      rabbit_mnesia:cluster_nodes(running), rabbit_mgmt_db_handler, gc, []),
    {hibernate, State}.

format_message_queue(Opt, MQ) -> rabbit_misc:format_message_queue(Opt, MQ).

%%----------------------------------------------------------------------------
%% Internal, utilities
%%----------------------------------------------------------------------------

pget(Key, List) -> pget(Key, List, unknown).

%% id_name() and id() are for use when handling events, id_lookup()
%% for when augmenting. The difference is that when handling events a
%% queue name will be a resource, but when augmenting we will be
%% passed a queue proplist that will already have been formatted -
%% i.e. it will have name and vhost keys.
id_name(node_stats)       -> name;
id_name(node_node_stats)  -> route;
id_name(vhost_stats)      -> name;
id_name(queue_stats)      -> name;
id_name(exchange_stats)   -> name;
id_name(channel_stats)    -> pid;
id_name(connection_stats) -> pid.

id(Type, List) -> pget(id_name(Type), List).

id_lookup(queue_stats, List) ->
    rabbit_misc:r(pget(vhost, List), queue, pget(name, List));
id_lookup(exchange_stats, List) ->
    rabbit_misc:r(pget(vhost, List), exchange, pget(name, List));
id_lookup(Type, List) ->
    id(Type, List).

lookup_element(Table, Key) -> lookup_element(Table, Key, 2).

lookup_element(Table, Key, Pos) ->
    try ets:lookup_element(Table, Key, Pos)
    catch error:badarg -> []
    end.

details_key(Key) -> list_to_atom(atom_to_list(Key) ++ "_details").

%%----------------------------------------------------------------------------
%% Internal, querying side
%%----------------------------------------------------------------------------

-define(QUEUE_DETAILS,
        {queue_stats, [{incoming,   queue_exchange_stats, fun first/1},
                       {deliveries, channel_queue_stats,  fun second/1}]}).

-define(EXCHANGE_DETAILS,
        {exchange_stats, [{incoming, channel_exchange_stats, fun second/1},
                          {outgoing, queue_exchange_stats,   fun second/1}]}).

-define(CHANNEL_DETAILS,
        {channel_stats, [{publishes,  channel_exchange_stats, fun first/1},
                         {deliveries, channel_queue_stats,    fun first/1}]}).

-define(NODE_DETAILS,
        {node_stats, [{cluster_links, node_node_stats, fun first/1}]}).

first(Id)  -> {Id, '$1'}.
second(Id) -> {'$1', Id}.

list_queue_stats(Ranges, Objs, Interval) ->
    adjust_hibernated_memory_use(
      merge_stats(Objs, queue_funs(Ranges, Interval))).

detail_queue_stats(Ranges, Objs, Interval) ->
    adjust_hibernated_memory_use(
      merge_stats(Objs, [consumer_details_fun(
                           fun (Props) -> id_lookup(queue_stats, Props) end,
                           consumers_by_queue),
                         detail_stats_fun(Ranges, ?QUEUE_DETAILS, Interval)
                         | queue_funs(Ranges, Interval)])).

queue_funs(Ranges, Interval) ->
    [basic_stats_fun(queue_stats),
     simple_stats_fun(Ranges, queue_stats, Interval),
     augment_msg_stats_fun()].

list_exchange_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [simple_stats_fun(Ranges, exchange_stats, Interval),
                       augment_msg_stats_fun()]).

detail_exchange_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [simple_stats_fun(Ranges, exchange_stats, Interval),
                       detail_stats_fun(Ranges, ?EXCHANGE_DETAILS, Interval),
                       augment_msg_stats_fun()]).

connection_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [basic_stats_fun(connection_stats),
                       simple_stats_fun(Ranges, connection_stats, Interval),
                       augment_msg_stats_fun()]).

list_channel_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [basic_stats_fun(channel_stats),
                       simple_stats_fun(Ranges, channel_stats, Interval),
                       augment_msg_stats_fun()]).

detail_channel_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [basic_stats_fun(channel_stats),
                       simple_stats_fun(Ranges, channel_stats, Interval),
                       consumer_details_fun(
                         fun (Props) -> pget(pid, Props) end,
                         consumers_by_channel),
                       detail_stats_fun(Ranges, ?CHANNEL_DETAILS, Interval),
                       augment_msg_stats_fun()]).

vhost_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [simple_stats_fun(Ranges, vhost_stats, Interval)]).

node_stats(Ranges, Objs, Interval) ->
    merge_stats(Objs, [basic_stats_fun(node_stats),
                       simple_stats_fun(Ranges, node_stats, Interval),
                       detail_and_basic_stats_fun(
                         node_node_stats, Ranges, ?NODE_DETAILS, Interval)]).

merge_stats(Objs, Funs) ->
    [lists:foldl(fun (Fun, Props) -> combine(Fun(Props), Props) end, Obj, Funs)
     || Obj <- Objs].

combine(New, Old) ->
    case pget(state, Old) of
        unknown -> New ++ Old;
        live    -> New ++ proplists:delete(state, Old);
        _       -> proplists:delete(state, New) ++ Old
    end.

%% i.e. the non-calculated stats
basic_stats_fun(Type) ->
    fun (Props) ->
            Id = id_lookup(Type, Props),
            lookup_element(Type, {Id, stats})
    end.

%% i.e. coarse stats, and fine stats aggregated up to a single number per thing
simple_stats_fun(Ranges, Type, Interval) ->
    fun (Props) ->
            Id = id_lookup(Type, Props),
            extract_msg_stats(
              format_samples(Ranges, read_simple_stats(Type, Id), Interval))
    end.

%% i.e. fine stats that are broken out per sub-thing
detail_stats_fun(Ranges, {IdType, FineSpecs}, Interval) ->
    fun (Props) ->
            Id = id_lookup(IdType, Props),
            [detail_stats(Ranges, Name, AggregatedStatsType, IdFun(Id), Interval)
             || {Name, AggregatedStatsType, IdFun} <- FineSpecs]
    end.

%% This does not quite do the same as detail_stats_fun +
%% basic_stats_fun; the basic part here assumes compound keys (like
%% detail stats) but non-calculated (like basic stats). Currently the
%% only user of that is node-node stats.
%%
%% We also assume that FineSpecs is single length here (at [1]).
detail_and_basic_stats_fun(Type, Ranges, {IdType, FineSpecs}, Interval) ->
    F = detail_stats_fun(Ranges, {IdType, FineSpecs}, Interval),
    fun (Props) ->
            Id = id_lookup(IdType, Props),
            BasicStatsRaw = ets:match(Type, {{{Id, '$1'}, stats}, '$2', '_'}),
            BasicStatsDict = dict:from_list([{K, V} || [K,V] <- BasicStatsRaw]),
            [{K, Items}] = F(Props), %% [1]
            Items2 = [case dict:find(id_lookup(IdType, Item), BasicStatsDict) of
                          {ok, BasicStats} -> BasicStats ++ Item;
                          error            -> Item
                      end || Item <- Items],
            [{K, Items2}]
    end.

read_simple_stats(Type, Id) ->
    FromETS = ets:match(aggregated_stats, {{{Type, Id}, '$1'}, '$2'}),
    [{K, V} || [K, V] <- FromETS].

read_detail_stats(Type, Id) ->
    %% Id must contain '$1'
    FromETS = ets:match(aggregated_stats, {{{Type, Id}, '$2'}, '$3'}),
    %% [[G, K, V]] -> [{G, [{K, V}]}] where G is Q/X/Ch, K is from
    %% ?FINE_STATS and V is a stats tree
    %% TODO does this need to be optimised?
    lists:foldl(
      fun ([G, K, V], L) ->
              case lists:keyfind(G, 1, L) of
                  false    -> [{G, [{K, V}]} | L];
                  {G, KVs} -> lists:keyreplace(G, 1, L, {G, [{K, V} | KVs]})
              end
      end, [], FromETS).

extract_msg_stats(Stats) ->
    {MsgStats, Other} =
        lists:partition(fun({K, _}) ->
                                lists:member(K, ?MSG_RATES_DETAILS)
                        end, Stats),
    case MsgStats of
        [] -> Other;
        _  -> [{message_stats, MsgStats} | Other]
    end.

detail_stats(Ranges, Name, AggregatedStatsType, Id, Interval) ->
    {Name,
     [[{stats, format_samples(Ranges, KVs, Interval)} | format_detail_id(G)]
      || {G, KVs} <- read_detail_stats(AggregatedStatsType, Id)]}.

format_detail_id(ChPid) when is_pid(ChPid) ->
    augment_msg_stats([{channel, ChPid}]);
format_detail_id(#resource{name = Name, virtual_host = Vhost, kind = Kind}) ->
    [{Kind, [{name, Name}, {vhost, Vhost}]}];
format_detail_id(Node) when is_atom(Node) ->
    [{name, Node}].

format_samples(Ranges, ManyStats, Interval) ->
    lists:append(
      [case rabbit_mgmt_stats:is_blank(Stats) andalso
           not lists:member(K, ?ALWAYS_REPORT_STATS) of
           true  -> [];
           false -> {Details, Counter} = rabbit_mgmt_stats:format(
                                           pick_range(K, Ranges),
                                           Stats, Interval),
                    [{K,              Counter},
                     {details_key(K), Details}]
       end || {K, Stats} <- ManyStats]).

pick_range(K, {RangeL, RangeM, RangeD, RangeN}) ->
    case {lists:member(K, ?QUEUE_MSG_COUNTS),
          lists:member(K, ?MSG_RATES),
          lists:member(K, ?COARSE_CONN_STATS),
          lists:member(K, ?COARSE_NODE_STATS)
          orelse lists:member(K, ?COARSE_NODE_NODE_STATS)} of
        {true, false, false, false} -> RangeL;
        {false, true, false, false} -> RangeM;
        {false, false, true, false} -> RangeD;
        {false, false, false, true} -> RangeN
    end.

%% We do this when retrieving the queue record rather than when
%% storing it since the memory use will drop *after* we find out about
%% hibernation, so to do it when we receive a queue stats event would
%% be fiddly and racy. This should be quite cheap though.
adjust_hibernated_memory_use(Qs) ->
    Pids = [pget(pid, Q) ||
               Q <- Qs, pget(idle_since, Q, not_idle) =/= not_idle],
    %% We use delegate here not for ordering reasons but because we
    %% want to get the right amount of parallelism and minimise
    %% cross-cluster communication.
    {Mem, _BadNodes} = delegate:invoke(Pids, {erlang, process_info, [memory]}),
    MemDict = dict:from_list([{P, M} || {P, M = {memory, _}} <- Mem]),
    [case dict:find(pget(pid, Q), MemDict) of
         error        -> Q;
         {ok, Memory} -> [Memory|proplists:delete(memory, Q)]
     end || Q <- Qs].

created_event(Name, Type) ->
    case ets:select(Type, [{{{'_', '$1'}, '$2', '$3'}, [{'==', 'create', '$1'},
                                                        {'==', Name, '$3'}],
                            ['$2']}]) of
        [] -> not_found;
        [Elem] -> Elem
    end.

created_events(Type) ->
    ets:select(Type, [{{{'_', '$1'}, '$2', '_'}, [{'==', 'create', '$1'}],
                       ['$2']}]).

consumers_by_queue_and_vhost(VHost) ->
    ets:select(consumers_by_queue,
               [{{{#resource{virtual_host = '$1', _ = '_'}, '_', '_'}, '$2'},
                 [{'orelse', {'==', 'all', VHost}, {'==', VHost, '$1'}}],
                 ['$2']}]).

consumer_details_fun(KeyFun, TableName) ->
    fun ([])    -> [];
        (Props) -> Pattern = {KeyFun(Props), '_', '_'},
                   [{consumer_details,
                     [augment_msg_stats(augment_consumer(Obj))
                      || Obj <- lists:append(
                                  ets:match(TableName, {Pattern, '$1'}))]}]
    end.

augment_consumer(Obj) ->
    [{queue, rabbit_mgmt_format:resource(pget(queue, Obj))} |
     proplists:delete(queue, Obj)].

%%----------------------------------------------------------------------------
%% Internal, query-time summing for overview
%%----------------------------------------------------------------------------

overview_sum(Type, VHostStats) ->
    Stats = lists:foldl(fun(VHost, Acc) ->
                                case pget(Type, VHost) of
                                    unknown -> Acc;
                                    V -> [V | Acc]
                                end
                        end, [], VHostStats),
    {Type, rabbit_mgmt_stats:sum(Stats)}.

%%----------------------------------------------------------------------------
%% Internal, query-time augmentation
%%----------------------------------------------------------------------------

augment_msg_stats(Props) ->
    rabbit_mgmt_format:strip_pids(
      (augment_msg_stats_fun())(Props) ++ Props).

augment_msg_stats_fun() ->
    fun(Props) ->
            lists:foldl(fun({_, none}, Acc) ->
                                Acc;
                           ({_, unknown}, Acc) ->
                                Acc;
                           ({connection, Value}, Acc) ->
                                [{connection_details, augment_connection_pid(Value)}
                                 | Acc];
                           ({channel, Value}, Acc) ->
                                [{channel_details, augment_channel_pid(Value)}
                                 | Acc];
                           ({owner_pid, Value}, Acc) ->
                                [{owner_pid_details, augment_connection_pid(Value)}
                                 | Acc];
                           (_, Acc) ->
                                Acc
                        end, [], Props)
    end.

augment_channel_pid(Pid) ->
    Ch = lookup_element(channel_stats, {Pid, create}),
    Conn = lookup_element(connection_stats,
                          {pget(connection, Ch), create}),
    [{name,            pget(name,   Ch)},
     {number,          pget(number, Ch)},
     {user,            pget(user,   Ch)},
     {connection_name, pget(name,         Conn)},
     {peer_port,       pget(peer_port,    Conn)},
     {peer_host,       pget(peer_host,    Conn)}].

augment_connection_pid(Pid) ->
    Conn = lookup_element(connection_stats, {Pid, create}),
    [{name,         pget(name,         Conn)},
     {peer_port,    pget(peer_port,    Conn)},
     {peer_host,    pget(peer_host,    Conn)}].


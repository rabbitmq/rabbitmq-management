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
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developer of the Original Code is Pivotal Software, Inc.
%%   Copyright (c) 2010-2015 Pivotal Software, Inc.  All rights reserved.
%%

-define(DELIVER_GET, [deliver, deliver_no_ack, get, get_no_ack]).
-define(FINE_STATS, [publish, publish_in, publish_out,
                     ack, deliver_get, confirm, return_unroutable, redeliver] ++
            ?DELIVER_GET).

%% Most come from channels as fine stats, but queues emit these directly.
-define(QUEUE_MSG_RATES, [disk_reads, disk_writes]).

-define(MSG_RATES, ?FINE_STATS ++ ?QUEUE_MSG_RATES).

-define(MSG_RATES_DETAILS, [publish_details, publish_in_details,
                            publish_out_details, ack_details,
                            deliver_get_details, confirm_details,
                            return_unroutable_details, redeliver_details,
                            deliver_details, deliver_no_ack_details,
                            get_details, get_no_ack_details,
                            disk_reads_details, disk_writes_details] ++ ?MSG_RATES).

-define(QUEUE_MSG_COUNTS, [messages, messages_ready, messages_unacknowledged]).

-define(COARSE_NODE_STATS,
        [mem_used, fd_used, sockets_used, proc_used, disk_free,
         io_read_count,  io_read_bytes,  io_read_avg_time,
         io_write_count, io_write_bytes, io_write_avg_time,
         io_sync_count,  io_sync_avg_time,
         io_seek_count,  io_seek_avg_time,
         io_reopen_count, mnesia_ram_tx_count,  mnesia_disk_tx_count,
         msg_store_read_count, msg_store_write_count,
         queue_index_journal_write_count,
         queue_index_write_count, queue_index_read_count]).

-define(COARSE_NODE_NODE_STATS, [send_bytes, recv_bytes]).

%% Normally 0 and no history means "has never happened, don't
%% report". But for these things we do want to report even at 0 with
%% no history.
-define(ALWAYS_REPORT_STATS,
        [io_read_avg_time, io_write_avg_time,
         io_sync_avg_time | ?QUEUE_MSG_COUNTS]).

-define(COARSE_CONN_STATS, [recv_oct, send_oct]).


%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License at
%% http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%% License for the specific language governing rights and limitations
%% under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2016 Pivotal Software, Inc.  All rights reserved.
%%

-module(rabbit_mgmt_rabbitmqadmin_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [ {group, list_to_atom(Py)} || Py <- find_pythons() ].

groups() ->
    Tests = [
             help,
             host,
             config,
             user,
             fmt_long,
             fmt_kvp,
             fmt_tsv,
             fmt_table,
             fmt_bash,
             vhosts,
             users,
             permissions,
             alt_vhost,
             exchanges,
             queues,
             bindings,
             policies,
             operator_policies,
             parameters,
             publish,
             ignore_vhost,
             sort
            ],
    [ {list_to_atom(Py), [], Tests} || Py <- find_pythons() ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    inets:start(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
                                                    {rmq_nodename_suffix, ?MODULE}
                                                   ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
                                      rabbit_ct_broker_helpers:setup_steps() ++
                                      rabbit_ct_client_helpers:setup_steps() ++
                                      [fun (C) ->
                                               rabbit_ct_helpers:set_config(C,
                                                                            {rabbitmqadmin_path,
                                                                             rabbitmqadmin(C)})
                                       end
                                      ]).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
                                         rabbit_ct_client_helpers:teardown_steps() ++
                                             rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(python2, Config) ->
    rabbit_ct_helpers:set_config(Config, {python, "python2"});
init_per_group(python3, Config) ->
    rabbit_ct_helpers:set_config(Config, {python, "python3"});
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(config, Config) ->
    rabbit_ct_helpers:set_config(Config, {env_home, os:getenv("HOME")});
init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(config, Config) ->
    Home = rabbit_ct_helpers:get_config(Config, env_home),
    os:putenv("HOME", Home);
end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).


%% -------------------------------------------------------------------
%% Testcases.
%% -------------------------------------------------------------------

help(Config) ->
    {ok, _} = run(Config, ["--help"]),
    {ok, _} = run(Config, ["help", "subcommands"]),
    {ok, _} = run(Config, ["help", "config"]),
    {error, _, _} = run(Config, ["help", "astronomy"]).

host(Config) ->
    {ok, _} = run(Config, ["show", "overview"]),
    {ok, _} = run(Config, ["--host", "localhost", "show", "overview"]),
    {error, _, _} = run(Config, ["--host", "some-host-that-does-not-exist",
                                 "show", "overview"]).

config(Config) ->
    PrivDir = ?config(priv_dir, Config),
    os:putenv("HOME", PrivDir),
    {_DefConf, TestConf} = write_test_config(Config),
    {error, _, _} = run(Config, ["--config", "/tmp/no-such-config-file",
                                 "show", "overview"]),
    {ok, _} = run(Config, ["--config", TestConf, "--node",
                           "host_normal", "show", "overview"]),

    % test 'default node in the config file' where "default" uses an invalid host
    {error, _, _} = run(Config, ["--config", TestConf, "show", "overview"]),
    {ok, _} = run(Config, ["show", "overview"]),
    {error, _, _} = run(Config, ["--node", "non_default", "show", "overview"]).

user(Config) ->
    {ok, _} = run(Config, ["--user", "guest", "--password", "guest", "show", "overview"]),
    {error, _, _} = run(Config, ["--user", "no", "--password", "guest", "show", "overview"]),
    {error, _, _} = run(Config, ["--user", "guest", "--password", "no", "show", "overview"]).

fmt_long(Config) ->
    Out = multi_line_string([
        "",
        "--------------------------------------------------------------------------------",
        "",
        "   name: /",
        "tracing: False",
        "",
        "--------------------------------------------------------------------------------",
        "" ]),
    {ok, Out} = run(Config, ["--format", "long", "list", "vhosts", "name", "tracing"]).

fmt_kvp(Config) ->
    Out = multi_line_string(["name=\"/\" tracing=\"False\""]),
    {ok, Out} = run(Config, ["--format", "kvp", "list", "vhosts", "name", "tracing"]).

fmt_tsv(Config) ->
    Out = multi_line_string([
                             "name\ttracing",
                             "/\tFalse"
                            ]),
    {ok, Out} = run(Config, ["--format", "tsv", "list", "vhosts", "name", "tracing"]).

fmt_table(Config) ->
    Out = multi_line_string([
                             "+------+---------+",
                             "| name | tracing |",
                             "+------+---------+",
                             "| /    | False   |",
                             "+------+---------+"
                            ]),
    {ok, Out} = run(Config, ["list", "vhosts", "name", "tracing"]),
    {ok, Out} = run(Config, ["--format", "table", "list",
                             "vhosts", "name", "tracing"]).

fmt_bash(Config) ->
    {ok, "/\n"} = run(Config, ["--format", "bash", "list",
                               "vhosts", "name", "tracing"]).

vhosts(Config) ->
    {ok, ["/"]} = run_list(Config, l("vhosts")),
    {ok, _} = run(Config, ["declare", "vhost", "name=foo"]),
    {ok, ["/", "foo"]} = run_list(Config, l("vhosts")),
    {ok, _} = run(Config, ["delete", "vhost", "name=foo"]),
    {ok, ["/"]} = run_list(Config, l("vhosts")).

users(Config) ->
    {ok, ["guest"]} = run_list(Config, l("users")),
    {error, _, _} = run(Config, ["declare", "user", "name=foo"]),
    {ok, _} = run(Config, ["declare", "user", "name=foo", "password=pass", "tags="]),
    {ok, ["foo", "guest"]} = run_list(Config, l("users")),
    {ok, _} = run(Config, ["delete", "user", "name=foo"]),
    {ok, ["guest"]} = run_list(Config, l("users")).

permissions(Config) ->
    {ok, _} = run(Config, ["declare", "vhost", "name=foo"]),
    {ok, _} = run(Config, ["declare", "user", "name=bar", "password=pass", "tags="]),
    {ok, [["guest", "/"]]} = run_table(Config, ["list", "permissions",
                                                "user", "vhost"]),
    {ok, _} = run(Config, ["declare", "permission", "user=bar", "vhost=foo",
                           "configure=.*", "write=.*", "read=.*"]),
    {ok, [["guest", "/"], ["bar", "foo"]]} =  run_table(Config, ["list",
                                                                 "permissions",
                                                                 "user",
                                                                 "vhost"]),
    {ok, _} = run(Config, ["delete", "user", "name=bar"]),
    {ok, _} = run(Config, ["delete", "vhost", "name=foo"]).

alt_vhost(Config) ->
    {ok, _} = run(Config, ["declare", "vhost", "name=foo"]),
    {ok, _} = run(Config, ["declare", "permission", "user=guest", "vhost=foo",
                           "configure=.*", "write=.*", "read=.*"]),
    {ok, _} = run(Config, ["declare", "queue", "name=in_/"]),
    {ok, _} = run(Config, ["--vhost", "foo", "declare", "queue", "name=in_foo"]),
    {ok, [["/", "in_/"], ["foo", "in_foo"]]} = run_table(Config, ["list", "queues",
                                                                  "vhost", "name"]),
    {ok, _} = run(Config, ["--vhost", "foo", "delete", "queue", "name=in_foo"]),
    {ok, _} = run(Config, ["delete", "queue", "name=in_/"]),
    {ok, _} = run(Config, ["delete", "vhost", "name=foo"]).

exchanges(Config) ->
    {ok, _} = run(Config, ["declare", "exchange", "name=foo", "type=direct"]),
    {ok, ["amq.direct",
          "amq.fanout",
          "amq.headers",
          "amq.match",
          "amq.rabbitmq.log",
          "amq.rabbitmq.trace",
          "amq.topic",
          "foo"]} = run_list(Config, l("exchanges")),
    {ok, _} = run(Config, ["delete", "exchange", "name=foo"]).

queues(Config) ->
    {ok, _} = run(Config, ["declare", "queue", "name=foo"]),
    {ok, ["foo"]} = run_list(Config, l("queues")),
    {ok, _} = run(Config, ["delete", "queue", "name=foo"]).

bindings(Config) ->
    {ok, _} = run(Config, ["declare", "queue", "name=foo"]),
    {ok, _} = run(Config, ["declare", "binding", "source=amq.direct",
                           "destination=foo", "destination_type=queue",
                           "routing_key=test"]),
    {ok, [["foo", "queue", "foo"],
          ["amq.direct", "foo", "queue", "test"]
         ]} = run_table(Config,
                              ["list", "bindings",
                               "source", "destination",
                               "destination_type", "routing_key"]),
    {ok, _} = run(Config, ["delete", "queue", "name=foo"]).

policies(Config) ->
    {ok, _} = run(Config, ["declare", "policy", "name=ha",
                           "pattern=.*", "definition={\"ha-mode\":\"all\"}"]),
    {ok, [["ha", "/", ".*", "{\"ha-mode\": \"all\"}"]]} =
        run_table(Config, ["list", "policies", "name",
                                 "vhost", "pattern", "definition"]),
    {ok, _} = run(Config, ["delete", "policy", "name=ha"]).

operator_policies(Config) ->
    {ok, _} = run(Config, ["declare", "operator_policy", "name=len",
                           "pattern=.*", "definition={\"max-length\":100}"]),
    {ok, [["len", "/", ".*", "{\"max-length\": 100}"]]} =
        run_table(Config, ["list", "operator_policies", "name",
                                 "vhost", "pattern", "definition"]),
    {ok, _} = run(Config, ["delete", "operator_policy", "name=len"]).

parameters(Config) ->
    ok = rpc(Config, rabbit_mgmt_runtime_parameters_util, register, []),
    {ok, _} = run(Config, ["declare", "parameter", "component=test",
                           "name=good", "value=123"]),
    {ok, [["test", "good", "/", "123"]]} = run_table(Config, ["list",
                                                              "parameters",
                                                              "component",
                                                              "name",
                                                              "vhost",
                                                              "value"]),
    {ok, _} = run(Config, ["delete", "parameter", "component=test", "name=good"]),
    ok = rpc(Config, rabbit_mgmt_runtime_parameters_util, unregister, []).

publish(Config) ->
    {ok, _} = run(Config, ["declare", "queue", "name=test"]),
    {ok, _} = run(Config, ["publish", "routing_key=test", "payload=test_1"]),
    {ok, _} = run(Config, ["publish", "routing_key=test", "payload=test_2"]),
    % publish with stdin
    {ok, _} = rabbit_ct_helpers:exec(["python", "-c",
                                      publish_with_stdin_python_program(Config, "test_3")],
                                     []),

    M = exp_msg("test", 2, "False", "test_1"),
    {ok, [M]} = run_table(Config, ["get", "queue=test", "ackmode=ack_requeue_false"]),
    M2 = exp_msg("test", 1, "False", "test_2"),
    {ok, [M2]} = run_table(Config, ["get", "queue=test", "ackmode=ack_requeue_true"]),
    M3 = exp_msg("test", 1, "True", "test_2"),
    {ok, [M3]} = run_table(Config, ["get",
                                    "queue=test",
                                    "ackmode=ack_requeue_false"]),
    M4 = exp_msg("test", 0, "False", "test_3"),
    {ok, [M4]} = run_table(Config, ["get",
                                    "queue=test",
                                    "ackmode=ack_requeue_false"]),
    {ok, _} = run(Config, ["publish", "routing_key=test", "payload=test_4"]),
    Fn = filename:join(?config(priv_dir, Config), "publish_test_4"),

    {ok, _} = run(Config, ["get", "queue=test", "ackmode=ack_requeue_false", "payload_file=" ++ Fn]),
    {ok, <<"test_4">>} = file:read_file(Fn),
    {ok, _} = run(Config, ["delete", "queue", "name=test"]).

ignore_vhost(Config) ->
    {ok, _} = run(Config, ["--vhost", "/", "show", "overview"]),
    {ok, _} = run(Config, ["--vhost", "/", "list", "users"]),
    {ok, _} = run(Config, ["--vhost", "/", "list", "vhosts"]),
    {ok, _} = run(Config, ["--vhost", "/", "list", "nodes"]),
    {ok, _} = run(Config, ["--vhost", "/", "list", "permissions"]),
    {ok, _} = run(Config, ["--vhost", "/", "declare", "user",
                           "name=foo", "password=pass", "tags="]),
    {ok, _} = run(Config, ["delete", "user", "name=foo"]).

sort(Config) ->
    {ok, _} = run(Config, ["declare", "queue", "name=foo"]),
    {ok, _} = run(Config, ["declare", "binding", "source=amq.direct",
                           "destination=foo", "destination_type=queue",
                           "routing_key=bbb"]),
    {ok, _} = run(Config, ["declare", "binding", "source=amq.topic",
                           "destination=foo", "destination_type=queue",
                           "routing_key=aaa"]),
    {ok, [["foo"],
          ["amq.direct", "bbb"],
          ["amq.topic", "aaa"]]} = run_table(Config, ["--sort", "source",
                                                      "list", "bindings",
                                                      "source", "routing_key"]),
    {ok, [["amq.topic", "aaa"],
          ["amq.direct", "bbb"],
          ["foo"]]} = run_table(Config, ["--sort", "routing_key",
                                         "list", "bindings", "source",
                                         "routing_key"]),
    {ok, [["amq.topic", "aaa"],
          ["amq.direct", "bbb"],
          ["foo"]]} = run_table(Config, ["--sort", "source",
                                         "--sort-reverse", "list",
                                         "bindings", "source",
                                         "routing_key"]),
    {ok, _} = run(Config, ["delete", "queue", "name=foo"]).

%% -------------------------------------------------------------------
%% Utilities
%% -------------------------------------------------------------------

exp_msg(Key, Count, Redelivered, Payload) ->
    % routing_key, message_count,
    % payload, payload_bytes,
    % payload_encoding, redelivered
    [Key, integer_to_list(Count),
     Payload, integer_to_list(length(Payload)),
     "string", Redelivered].

rpc(Config, M, F, A) ->
    rabbit_ct_broker_helpers:rpc(Config, 0, M, F, A).

l(Thing) ->
    ["list", Thing, "name"].

multi_line_string(Lines) ->
    lists:flatten([string:join(Lines, io_lib:nl()), io_lib:nl()]).

run_table(Config, Args) ->
    {ok, Lines} = run_list(Config, Args),
    Tokens = [string:tokens(L, "\t") || L <- Lines],
    {ok, Tokens}.

run_list(Config, Args) ->
    A = ["-f", "tsv", "-q"],
    case run(Config, A ++ Args) of
        {ok, Out} -> {ok, string:tokens(Out, io_lib:nl())};
        Err -> Err
    end.

run(Config, Args) ->
    Py = rabbit_ct_helpers:get_config(Config, python),
    MgmtPort = rabbit_ct_broker_helpers:get_node_config(Config, 0, tcp_port_mgmt),
    RmqAdmin = rabbit_ct_helpers:get_config(Config, rabbitmqadmin_path),
    rabbit_ct_helpers:exec([Py,
                                  RmqAdmin,
                                  "-P",
                                  integer_to_list(MgmtPort)] ++ Args,
                                 [drop_stdout]).

rabbitmqadmin(Config) ->
    filename:join([?config(current_srcdir, Config), "bin", "rabbitmqadmin"]).

find_pythons() ->
    Py2 = rabbit_ct_helpers:exec(["python2", "-V"]),
    Py3 = rabbit_ct_helpers:exec(["python3", "-V"]),
    case {Py2, Py3} of
         {{ok, _}, {ok, _}} -> ["python2", "python3"];
         {{ok, _}, _} -> ["python2"];
         {_, {ok, _}} -> ["python3"];
         _ -> erlang:error("python not found")
    end.

publish_with_stdin_python_program(Config, In) ->
    % This is a nasty workaround erlang ports not supporting EOF
    Py = rabbit_ct_helpers:get_config(Config, python),
    MgmtPort = rabbit_ct_broker_helpers:get_node_config(Config, 0, tcp_port_mgmt),
    RmqAdmin = rabbit_ct_helpers:get_config(Config, rabbitmqadmin_path),
    "import subprocess;" ++
    "proc = subprocess.Popen(['" ++ Py ++ "', '" ++ RmqAdmin ++ "', '-P', '" ++ integer_to_list(MgmtPort) ++
    "', 'publish', 'routing_key=test'], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE);" ++
    "(stdout, stderr) = proc.communicate('" ++ In ++ "');" ++
    "exit(proc.returncode)".

write_test_config(Config) ->
    MgmtPort = integer_to_list(rabbit_ct_broker_helpers:get_node_config(Config, 0, tcp_port_mgmt)),
    PrivDir = ?config(priv_dir, Config),
    DefaultConfig = [
        "[non_default]",
        "hostname = localhost",
        "port = 99999",
        "username = guest",
        "password = guest",
        "declare_vhost = /",
        "vhost = /",
        "",
        "[bad_host]",
        "hostname = rabbit.acme.com",
        "port = " ++ MgmtPort,
        "username = guest",
        "password = guest"
                    ],
    TestConfig = [
        "[host_normal]",
        "hostname = localhost",
        "port = " ++ MgmtPort,
        "username = guest",
        "password = guest",
        "declare_vhost = /",
        "vhost = /",
        "",
        "[default]",
        "hostname = localhost",
        "port = 99999",
        "username = guest",
        "password = guest"
           ],
    DefaultConfig1 = [string:join(DefaultConfig, io_lib:nl()), io_lib:nl()],
    TestConfig1 = [string:join(TestConfig, io_lib:nl()), io_lib:nl()],
    FnDefault = filename:join(PrivDir, ".rabbitmqadmin.conf"),
    FnTest = filename:join(PrivDir, "test-config"),
    file:write_file(FnDefault, DefaultConfig1),
    file:write_file(FnTest, TestConfig1),
    {FnDefault, FnTest}.

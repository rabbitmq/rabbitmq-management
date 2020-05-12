-module(honeycomb_cth).

-export([id/1]).
-export([init/2]).

-export([pre_init_per_testcase/4]).
-export([post_end_per_testcase/5]).

-record(state, {directory, github_workflow, github_run_id,
                github_repository, github_sha, github_ref,
                base_rmq_ref, erlang_version, elixir_version,
                otp_release, cpu_topology_json, schedulers,
                system_architecture, system_memory_data_json,
                start_times = #{}}).

id(Opts) ->
    proplists:get_value(directory, Opts, "/tmp/honeycomb").

init(Id, _Opts) ->
    application:ensure_all_started(os_mon),
    {ok, #state{directory = Id,
                github_workflow = os:getenv("GITHUB_WORKFLOW", "unknown"),
                github_run_id = os:getenv("GITHUB_RUN_ID", "unknown"),
                github_repository = os:getenv("GITHUB_REPOSITORY", "unknown"),
                github_sha = os:getenv("GITHUB_SHA", "unknown"),
                github_ref = os:getenv("GITHUB_REF", "unknown"),
                base_rmq_ref = os:getenv("BASE_RMQ_REF", "unknown"),
                erlang_version = os:getenv("ERLANG_VERSION", "unknown"),
                elixir_version = os:getenv("ELIXIR_VERSION", "unknown"),
                otp_release = erlang:system_info(otp_release),
                cpu_topology_json = cpu_topology_json(erlang:system_info(cpu_topology)),
                schedulers = erlang:system_info(schedulers),
                system_architecture = erlang:system_info(system_architecture),
                system_memory_data_json = json_string_memory(memsup:get_system_memory_data())}}.

pre_init_per_testcase(_Suite, TC, Config, #state{start_times = StartTimes} = State) ->
    {Config, State#state{start_times = StartTimes#{TC => erlang:timestamp()}}}.

post_end_per_testcase(Suite, TC, _Config, Return, #state{github_workflow = GithubWorkflow,
                                                         github_run_id = GithubRunId,
                                                         github_repository = GithubRepository,
                                                         github_sha = GithubSha,
                                                         github_ref = GithubRef,
                                                         base_rmq_ref = BaseRmqRef,
                                                         erlang_version = ErlangVersion,
                                                         elixir_version = ElixirVersion,
                                                         otp_release = OtpRelease,
                                                         cpu_topology_json = CpuTopologyJson,
                                                         schedulers = Schedulers,
                                                         system_architecture = SystemArchitecture,
                                                         system_memory_data_json = SystemMemoryDataJson,
                                                         start_times = StartTimes} = State) ->
    EndTime = erlang:timestamp(),
    {StartTime, StartTimes1} = maps:take(TC, StartTimes),
    DurationMicroseconds = timer:now_diff(EndTime, StartTime),
    File = filename(Suite, TC, State),
    ok = filelib:ensure_dir(File),
    {ok, F} = file:open(File, [write]),
    io:format(F, "{"
              "\"ci\":\"GitHub Actions\","
              "\"github_workflow\":\"~s\","
              "\"github_run_id\":\"~s\","
              "\"github_repository\":\"~s\","
              "\"github_sha\":\"~s\","
              "\"github_ref\":\"~s\","
              "\"base_rmq_ref\":\"~s\","
              "\"erlang_version\":\"~s\","
              "\"elixir_version\":\"~s\","
              "\"otp_release\":\"~s\","
              "\"cpu_topology\":~s,"
              "\"schedulers\":~p,"
              "\"system_architecture\":\"~s\","
              "\"system_memory_data\":~s,"
              "\"suite\":\"~p\","
              "\"testcase\":\"~p\","
              "\"duration_seconds\":~p,"
              "\"result\":\"~p\""
              "}~n", [GithubWorkflow, GithubRunId, GithubRepository,
                      GithubSha, GithubRef, BaseRmqRef, ErlangVersion,
                      ElixirVersion, OtpRelease,
                      CpuTopologyJson, Schedulers,
                      SystemArchitecture, SystemMemoryDataJson,
                      Suite, TC,
                      DurationMicroseconds / 1000000, Return]),
    file:close(F),
    {Return, State#state{start_times = StartTimes1}}.

filename(Suite, TC, #state{directory = Dir}) ->
    filename:join(Dir, atom_to_list(Suite) ++ "_" ++ atom_to_list(TC)
                  ++ ".json").

json_string_memory(SystemMemoryData) when is_list(SystemMemoryData) ->
    "{" ++ string:join([io_lib:format("\"~p\":~p", [K, V])
                        || {K, V} <- SystemMemoryData], ",") ++ "}".

cpu_topology_json([{processor, Cores}]) when is_list(Cores) ->
    "{\"processor\":["
        ++ string:join([io_lib:format("{\"core\":{\"~p\":~p}}", [Kind, Index])
                        || {core, {Kind, Index}} <- Cores], ",")
        ++ "]}".

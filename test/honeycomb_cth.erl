-module(honeycomb_cth).

-export([id/1]).
-export([init/2]).

-export([pre_init_per_testcase/4]).
-export([post_end_per_testcase/5]).

-record(state, {directory, github_workflow, github_run_id,
                github_repository, github_sha, github_ref,
                start_times = #{}}).

id(Opts) ->
    proplists:get_value(directory, Opts, "/tmp/honeycomb").

init(Id, _Opts) ->
    {ok, #state{directory = Id,
                github_workflow = os:getenv("GITHUB_WORKFLOW", "unknown"),
                github_run_id = os:getenv("GITHUB_RUN_ID", "unknown"),
                github_repository = os:getenv("GITHUB_REPOSITORY", "unknown"),
                github_sha = os:getenv("GITHUB_SHA", "unknown"),
                github_ref = os:getenv("GITHUB_REF", "unknown")}}.

pre_init_per_testcase(_Suite, TC, Config, #state{start_times = StartTimes} = State) ->
    {Config, State#state{start_times = StartTimes#{TC => erlang:timestamp()}}}.

post_end_per_testcase(Suite, TC, _Config, Return, #state{github_workflow = GithubWorkflow,
                                                         github_run_id = GithubRunId,
                                                         github_repository = GithubRepository,
                                                         github_sha = GithubSha,
                                                         github_ref = GithubRef,
                                                         start_times = StartTimes} = State) ->
    EndTime = erlang:timestamp(),
    {StartTime, StartTimes1} = maps:take(TC, StartTimes),
    DurationMicroseconds = timer:now_diff(EndTime, StartTime),
    File = filename(Suite, TC, State),
    ok = filelib:ensure_dir(File),
    {ok, F} = file:open(File, [write]),
    io:format(F, "{"
              "\"github_workflow\":\"~s\","
              "\"github_run_id\":\"~s\","
              "\"github_repository\":\"~s\","
              "\"github_sha\":\"~s\","
              "\"github_ref\":\"~s\","
              "\"suite\":\"~p\","
              "\"testcase\":\"~p\","
              "\"duration_seconds\":~p,"
              "\"result\":\"~p\""
              "}~n", [GithubWorkflow, GithubRunId, GithubRepository,
                      GithubSha, GithubRef, Suite, TC,
                      DurationMicroseconds / 1000000, Return]),
    file:close(F),
    {Return, State#state{start_times = StartTimes1}}.

filename(Suite, TC, #state{directory = Dir}) ->
    filename:join(Dir, atom_to_list(Suite) ++ "_" ++ atom_to_list(TC)
                  ++ ".json").

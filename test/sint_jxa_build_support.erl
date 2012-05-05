-module(sint_jxa_build_support).

-export([given/3, 'when'/3, then/3]).
-include_lib("eunit/include/eunit.hrl").

given([a,generated,project,that,contains,joxa,modules], _State, _) ->
    Result = {ok, {ProjectDir, _}} = sint_test_project_gen:a_generated_project(),
    io:format("~s~n", [ProjectDir]),
    BarFile = filename:join([ProjectDir, "src",
                          "foo", "bar.jxa"]),
    filelib:ensure_dir(BarFile),
    ok = file:write_file(BarFile, jxa_contents()),

    BazFile = filename:join([ProjectDir, "src",
                          "foo", "baz.jxa"]),
    filelib:ensure_dir(BazFile),
    ok = file:write_file(BazFile, jxa_dep()),
    Result.

'when'([a,build,step,is,run,on,this,project], {ProjectDir, ProjectName}, _) ->
    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    ?assertMatch({_, _}, Ret),
    {ok, {ProjectDir, ProjectName, Ret}}.

then([build,the,app,normally], State = {ProjectDir, ProjectName, _Ret}, _) ->
    code:add_patha(filename:join([ProjectDir, "_build", ProjectName, "lib",
                                  ProjectName ++ "-0.1.0", "ebin"])),
    code:ensure_loaded(foo.bar),
    code:ensure_loaded(foo.baz),
    ?assertMatch('test-return', foo.bar:'test-return'()),
    {ok, State}.

jxa_dep() ->
    "(module foo.baz)
(defn+ test-return ()
     :test-return)".

jxa_contents() ->
    "(module foo.bar
             (require foo.baz))

(defn+ test-return ()
       (foo.baz/test-return))".

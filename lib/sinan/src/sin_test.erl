%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006-2010 Erlware
%%%
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Runs the 'test' function on all modules in an application
%%%   if that function exits.
%%% @end
%%% @copyright (C) 2007-2010 Erlware
%%% Created : 16 Oct 2006 by Eric Merritt
%%%---------------------------------------------------------------------------
-module(sin_test).

-behaviour(eta_gen_task).

-include("etask.hrl").

%% API
-export([start/0, do_task/1, test/1]).

-define(TASK, test).
-define(DEPS, [build]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @spec () -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    Desc = "Runs all of the existing eunit unit tests in the project",
    TaskDesc = #task{name = ?TASK,
                     task_impl = ?MODULE,
                     deps = ?DEPS,
                     desc = Desc,
                     callable = true,
                     opts = []},
    eta_task:register_task(TaskDesc).


%%--------------------------------------------------------------------
%% @doc
%%  dO the task defined in this module.
%% @spec (BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
do_task(BuildRef) ->
    test(BuildRef).

%%--------------------------------------------------------------------
%% @doc
%%  Run the application tests.
%% @spec test(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
test(BuildRef) ->
    eta_event:task_start(BuildRef, ?TASK),
    case sin_build_config:get_value(BuildRef, "eunit") of
        "disabled" ->
            eta_event:task_fault(BuildRef, ?TASK,
                                 "Unit testing is disabled for this project. "
                                 "If you wish to change this change the eunit "
                                 "value of the build config from 'disabled' to "
                                 "'enabled' or remove it.");
        _ ->
            GL = sin_group_leader:capture_start(BuildRef, ?TASK),
            Apps = lists:map(fun({App, _Vsn, _Deps, _}) ->
                                     atom_to_list(App)
                             end, sin_build_config:get_value(BuildRef,
                                                  "project.apps")),
            test_apps(BuildRef, Apps),
            sin_group_leader:capture_stop(GL)

    end,
    eta_event:task_stop(BuildRef, ?TASK).


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%%   Run tests for all the applications specified.
%% @spec (BuildRef, AppList) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
test_apps(BuildRef, [AppName | T]) ->
    io:format("Testing ~s~n", [AppName]),
    Modules = sin_build_config:get_value(BuildRef,
                              "apps." ++ AppName ++ ".modules"),
    case Modules == undefined orelse length(Modules) =< 0 of
        true ->
            eta_event:task_fault(BuildRef, ?TASK,
                                 {"No modules defined for ~s.",
                                  [AppName]}),
            ok;
        false ->
            prepare_for_tests(BuildRef, AppName, Modules)
    end,
    test_apps(BuildRef, T);
test_apps(_, []) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Prepare for running the tests. This mostly means seting up the
%%  coverage tools.
%% @spec (BuildRef, AppName, Modules) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
prepare_for_tests(BuildRef, AppName, Modules) ->
    BuildDir = sin_build_config:get_value(BuildRef, "build.dir"),
    DocDir = filename:join([BuildDir, "docs", "coverage", AppName]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),
    Paths = sin_build_config:get_value(BuildRef,
                                       "apps." ++ AppName ++ ".code_paths"),
    code:add_pathsa(Paths),
    setup_code_coverage(BuildRef, Modules),
    run_module_tests(Modules),
    CoverageFiles = output_code_coverage(BuildRef, DocDir, Modules, []),
    output_coverage_index(DocDir, AppName, CoverageFiles),
    sin_utils:remove_code_paths(Paths).


%%--------------------------------------------------------------------
%% @doc
%%  Output coverage information to make accessing the files a
%%  bit easier.
%% @spec (DocDir, AppName, CoverageFiles) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
output_coverage_index(_DocDir, _AppName, []) ->
    % no coverage files created
    ok;
output_coverage_index(DocDir, AppName, CoverageFiles=[{Name, _Module} | _T]) ->
    Frame = ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \n"
             "   \"http://www.w3.org/TR/html4/frameset.dtd\">\n"
             "<HTML>\n"
             "<HEAD>\n"
             "<TITLE> Unit Test Coverage : ", AppName, " </TITLE>\n",
             "</HEAD>"
             "<FRAMESET cols=\"20%, 80%\">\n"
             "    <FRAME src=\"coverage_index.html\">\n"
             "    <FRAME name=\"bodyarea\" src=\"", Name, "\">\n"
             "    <NOFRAMES>\n"
             "      <P>This frameset document contains:\n"
             "        <A href=\"coverage_index.html\">Index of coverage reports</A>\n"
             "      </P>\n"
             "    </NOFRAMES>\n"
             "</FRAMESET>\n"
             "</HTML>\n"],
    CoverageIndex = ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n"
                     "   \"http://www.w3.org/TR/html4/loose.dtd\">\n"
                     "<HTML>\n"
                     "<HEAD>\n"
                     "  <TITLE>Coverage Index</TITLE>\n"
                     "</HEAD>\n"
                     "<BODY>\n"
                     " <P> \n"
                     "   <UL> \n",
                     make_index(CoverageFiles, []),
                     "   </UL>\n"
                     " </P> \n"
                     "</BODY> \n"
                     "</HTML> \n"],
    IndexFile = filename:join([DocDir, "index.html"]),
    CList = filename:join([DocDir, "coverage_index.html"]),
    file:write_file(IndexFile, list_to_binary(Frame), [write]),
    file:write_file(CList, list_to_binary(CoverageIndex), [write]).


%%--------------------------------------------------------------------
%% @doc
%%  Render the list of modules into a deep list of links.
%% @spec (FileList, Acc) -> DeepListOfLinks
%% @end
%% @private
%%--------------------------------------------------------------------
make_index([{File, Module} | T], Acc) ->
    Acc2 = ["<LI><A href=\"", File, "\" target=\"bodyarea\">", atom_to_list(Module),
            "</A></LI>" | Acc],
    make_index(T, Acc2);
make_index([], Acc) ->
    Acc.


%%--------------------------------------------------------------------
%% @doc
%%  Instrument all of the modules for code coverage checks.
%% @spec (BuildRef, Modules) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
setup_code_coverage(BuildRef, [Module | T]) ->
    case cover:compile_beam(Module) of
        {error, _} ->
            eta_event:task_event(BuildRef, ?TASK, info, {"Couldn't add code coverage to ~w", [Module]});
        _ ->
            ok
    end,
    setup_code_coverage(BuildRef, T);
setup_code_coverage(_, []) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%%  Take the analysis from test running and output it to an html file.
%% @spec (BuildRef, DocDir, Modules, Acc) -> ListOModules
%% @end
%% @private
%%--------------------------------------------------------------------
output_code_coverage(BuildRef, DocDir, [Module | T], Acc) ->
    File = lists:flatten([atom_to_list(Module), ".html"]),
    OutFile = filename:join([DocDir, File]),
    case cover:analyse_to_file(Module, OutFile, [html]) of
        {ok, _} ->
            output_code_coverage(BuildRef, DocDir, T, [{File, Module} | Acc]);
        {error, _} ->
            eta_event:task_event(BuildRef, ?TASK, info,
            {"Unable to write coverage information for ~w",
             [Module]}),
            output_code_coverage(BuildRef, DocDir, T, Acc)
    end;
output_code_coverage(_, _DocDir, [], Acc) ->
    Acc.
%%--------------------------------------------------------------------
%% @doc
%%   Run tests for each module that has a test/0 function
%% @spec (Modules) -> ok
%% @end
%% @private
%%--------------------------------------------------------------------
run_module_tests([Module | T]) ->
    io:format("~p:",[Module]),
    eunit:test(Module),
    run_module_tests(T);
run_module_tests([]) ->
    ok.

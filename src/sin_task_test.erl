%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%-------------------------------------------------------------------
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Runs the 'test' function on all modules in an application
%%%   if that function exits.
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_test).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-define(TASK, test).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc provides a description of this task
-spec description() ->  sin_task:task_description().
description() ->
    Desc = "Runs all eunit tests available in the project. Currently this \n"
	"task only supports eunit",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  example = "test",
	  desc = Desc,
	  short_desc = "Runs all of the existing eunit unit tests in the project",
	  opts = []}.

%% @doc run all tests for all modules in the system
do_task(BuildRef) ->
    case sin_config:get_value(BuildRef, "eunit") of
        "disabled" ->
	    ewl_talk:say("Unit testing is disabled for this project. "
			 "If you wish to change this change the eunit "
			 "value of the build config from 'disabled' to "
			 "'enabled' or remove it.");
        _ ->
            Apps = lists:map(fun({App, _Vsn, _Deps, _}) ->
                                     atom_to_list(App)
                             end, sin_config:get_value(BuildRef,
                                                  "project.apps")),
	    test_apps(build_all(BuildRef), BuildRef, Apps, []),
	    print_overall_percentage(BuildRef, Apps)
    end,
    BuildRef.


%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc if the command line has an 'all' prespective run all tests instead of
%% just those that changed
-spec build_all(sin_config:config()) ->
    all | changed.
build_all(BuildRef) ->
    case sin_config:get_value(BuildRef, "command_line.arg") of
	"all" ->
	    all;
	_ ->
	    changed
    end.

%% @doc Run tests for all the applications specified.
%% @private
-spec test_apps(all | changed,
		sin_config:config(), [string()], [[atom()]]) -> ok.
test_apps(BuildAll, BuildRef, [AppName | T], Acc) ->
    io:format("testing app ~s~n", [AppName]),
    Modules = sin_config:get_value(BuildRef,
                              "apps." ++ AppName ++ ".file_list"),
    case Modules == undefined orelse length(Modules) =< 0 of
        true ->
	    ewl_talk:say("No modules defined for ~s.",
			 [AppName]),
            ok;
        false ->
            prepare_for_tests(BuildAll, BuildRef, AppName, Modules)
    end,
    test_apps(BuildAll, BuildRef, T, [Modules | Acc]);
test_apps(_, _, [], Modules) ->
    Modules.

%% @doc Prepare for running the tests. This mostly means seting up the
%% coverage tools.
-spec prepare_for_tests(all | changed,
			sin_config:config(), string(), [atom()]) -> ok.
prepare_for_tests(BuildAll, BuildRef, AppName, AllModules) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    DocDir = filename:join([BuildDir, "docs", "coverage", AppName]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),
    Paths = sin_config:get_value(BuildRef,
                                       "apps." ++ AppName ++ ".code_paths"),
    code:add_pathsa(Paths),
    Modules = setup_code_coverage(BuildRef, AppName),
    run_module_tests(BuildAll, AllModules),
    CoverageFiles = output_code_coverage(BuildRef, DocDir, Modules, []),
    output_coverage_index(DocDir, AppName, CoverageFiles),
    sin_utils:remove_code_paths(Paths),
    Modules.


%% @doc Output coverage information to make accessing the coverage files a bit
%% easier.
-spec output_coverage_index(string(), string(),
			    [{Name::string(), Module::atom()}]) ->
    ok.
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
             "        <A href=\"coverage_index.html\">Index of coverage"
	     " reports</A>\n"
             "      </P>\n"
             "    </NOFRAMES>\n"
             "</FRAMESET>\n"
             "</HTML>\n"],
    CoverageIndex = ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 "
		     "Transitional//EN\"\n"
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

%% @doc Render the list of modules into a deep list of links.
-spec make_index([{string(), atom()}], list()) -> list().
make_index([{File, Module} | T], Acc) ->
    Acc2 = ["<LI><A href=\"", File, "\" target=\"bodyarea\">",
	    atom_to_list(Module),
            "</A></LI>" | Acc],
    make_index(T, Acc2);
make_index([], Acc) ->
    Acc.

%% @doc Instrument all of the modules for code coverage checks.
-spec setup_code_coverage(sin_config:config(), [atom()]) -> ok.
setup_code_coverage(BuildRef, AppName) ->
    Modules = sin_config:get_value(BuildRef,
                              "apps." ++ AppName ++ ".modules"),
    lists:foreach(
      fun({_, Module, _, _, _}) ->
	      case cover:compile_beam(Module) of
		  {error, Reason} ->
		      ewl_talk:say("Couldn't add code coverage to ~w "
				   "because ~p",
				   [Module, Reason]);
		  {ok, Module} ->
		      ok
	      end
      end,
      Modules),
    Modules.

%% @doc Take the analysis from test running and output it to an html file.
-spec output_code_coverage(sin_config:config(), string(), [atom()], list()) ->
    list().
output_code_coverage(BuildRef, DocDir, [{_, Module, _, _, _} | T], Acc) ->
    File = lists:flatten([atom_to_list(Module), ".html"]),
    OutFile = filename:join([DocDir, File]),
    case cover:analyse_to_file(Module, OutFile, [html]) of
        {ok, _} ->
            output_code_coverage(BuildRef, DocDir, T, [{File, Module} | Acc]);
        {error, Reason} ->
	    ewl_talk:say("Unable to write coverage information for "
			 "~w because ~p",
			 [Module, Reason]),
            output_code_coverage(BuildRef, DocDir, T, Acc)
    end;
output_code_coverage(_, _DocDir, [], Acc) ->
    Acc.

%% @doc Run tests for each module that has a test/0 function
-spec run_module_tests(all | changed, [atom()]) -> ok.
run_module_tests(BuildAll, AllModules) ->
    lists:foreach(
      fun({{_, Module, _, _, _},
	   {HasChanged, TestImplementations,
	    TestedModules, _}}) ->
	      case {BuildAll, lists:member(proper, TestImplementations),
		    tested_changed(TestedModules, AllModules)} of
		  {all, _, _} ->
		      proper:module(Module);
		  {_, true, true} ->
		      proper:module(Module);
		  _ ->
		      ok
	      end,
	      case {BuildAll,
		    lists:member(eunit, TestImplementations), HasChanged} of
		  {all, _, _} ->
		      ewl_talk:say("testing ~p", [Module]),
		      eunit:test(Module),
		      print_code_coverage(Module);
		  {_, true, changed} ->
		      ewl_talk:say("testing ~p", [Module]),
		      eunit:test(Module),
		      print_code_coverage(Module);
		  _ ->
		      ok
	      end
      end, AllModules).


%% @doc check to see if any of the modules listed in 'TestedModules' have
%% changed. If so return true, else return false.
-spec tested_changed([module()], [tuple]) -> boolean().
tested_changed([], _) ->
    true;
tested_changed(TestedModules, All) ->
    tested_changed1(TestedModules, All).

tested_changed1([TestModule | Rest], FileList) ->
    case ec_lists:find(fun({{_, TargetModule, _, _, _},
			    {changed, _, _, _}}) ->
			       TargetModule == TestModule;
			  (_) ->
			       false
		       end, FileList) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end,
    tested_changed(Rest, FileList);
tested_changed1([], _) ->
    false.



%% @doc inform the user what the code coverage percentage is for this module and
%% return the result to the caller.
-spec print_code_coverage(atom()) -> float().
print_code_coverage(Module) ->
    ewl_talk:say("~p coverage percentage ~.2f%",
		 [Module,
		  to_percentage(get_code_coverage(Module))]).


%% @doc get the code coverage amount for the specified module
-spec get_code_coverage(atom()) -> float().
get_code_coverage(Module) ->
    case cover:analyze(Module) of
	{ok, {Module, Calls}} when is_integer(Calls) ->
	    0.0;
	{ok, {Module, {Cov, NoCov}}} ->
	    Percentage = Cov / (Cov + NoCov),
	    Percentage;
	{ok, Terms} when is_list(Terms)  ->
	    Percentage = get_aggregate_percentage(Terms),
	    Percentage;
	{error, _} ->
	    0.0
    end.


%% @doc given a list of results from cover, print the code coverage percentage.
-spec get_aggregate_percentage([term()]) -> float().
get_aggregate_percentage(Terms) ->
    {Coverage, Count} =
	lists:foldl(fun({_, {Cov, _NoCov}}, {Coverage, Count})
		       when Cov == 0 ->
			    {Coverage, Count + 1};
		       ({_, {Cov, NoCov}}, {Coverage, Count}) ->
			    Percentage = (Cov / (Cov + NoCov)) + Coverage,
			    {Percentage, Count + 1};
		       (_, {Coverage, Count}) ->
			    {Coverage, Count + 1}
		    end,
		    {0, 0},
		    Terms),
    case Coverage of
	0 ->
	    0.0;
	_ ->
	    Coverage / Count
    end.


%% @doc print the total code coverage for the project
-spec print_overall_percentage(sin_config:config(), [string()]) -> ok.
print_overall_percentage(BuildRef, Apps) ->
    {Percentages, Count} =
	lists:foldl(fun(AppName, Acc) ->
			    Modules = sin_config:get_value(BuildRef,
							   "apps." ++ AppName
							   ++ ".all_modules"),
			    lists:foldl(fun({_, Module, _, _, _},
					    {Coverage, Count}) ->
						{Coverage +
						 get_code_coverage(Module),
						 Count + 1}
					end,
					Acc,
					lists:flatten(Modules))
		    end,
		    {0, 0},
		    Apps),
    ewl_talk:say("Overall run coverage ~.2f%",
		 [to_percentage(Percentages / Count)]).

%% @doc convert the value to a percentage
-spec to_percentage(float()) -> float().
to_percentage(Value) ->
    Value * 100.


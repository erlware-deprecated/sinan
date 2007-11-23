%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
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
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc 
%%%   Runs the 'test' function on all modules in an application
%%%   if that function exits.
%%% @end
%%% Created : 16 Oct 2006 by Eric Merritt 
%%%---------------------------------------------------------------------------
-module(sin_test).


%% API
-export([test/2]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Run the application tests.
%% @spec test() -> ok.
%% @end
%%--------------------------------------------------------------------
test(_BuildRef, "notest") ->
    ok;
test(BuildRef, _) ->
    case fconf:get_value(BuildRef, "eunit") of
        "disabled" ->
            ewl_talk:say("Unit testing is disabled for this project. "
                         "If you wish to change this change the eunit "
                         "value of the build config from 'disabled' to "
                         "'enabled' or remove it.");
        _ ->
            Apps = lists:map(fun({App, _Vsn, _Deps}) ->
                                     atom_to_list(App)
                             end, fconf:get_value(BuildRef, 
                                                  "project.apps")),
            test_apps(BuildRef, Apps)
    end.


%%====================================================================
%%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec test_apps(AppList) -> ok.
%% @doc
%%   Run tests for all the applications specified.
%% @end 
%% @private
%%--------------------------------------------------------------------
test_apps(BuildRef, [AppName | T]) ->
    Modules = fconf:get_value(BuildRef, 
                              {path, ["apps", AppName, "modules"]}),
    case Modules == undefined orelse length(Modules) =< 0 of
        true ->
            ewl_talk:say("No modules defined for ~s.",
                         [AppName]),
            ok;
        false ->
            prepare_for_tests(BuildRef, AppName, Modules)
    end,
    test_apps(BuildRef, T);
test_apps(_, []) ->
    ok.

%%--------------------------------------------------------------------
%% @spec prepare_for_tests(AppName, Modules) -> ok.
%% 
%% @doc  
%%  Prepare for running the tests. This mostly means seting up the 
%%  coverage tools.
%% @end
%% @private
%%--------------------------------------------------------------------
prepare_for_tests(BuildRef, AppName, Modules) ->
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    DocDir = filename:join([BuildDir, "docs", "coverage", AppName]),
    filelib:ensure_dir(filename:join([DocDir, "tmp"])),
    Paths = fconf:get_value(BuildRef, {path, ["apps", AppName, "code_paths"]}),
    code:add_pathsa(Paths),
    setup_code_coverage(Modules),
    run_module_tests(Modules),
    CoverageFiles = output_code_coverage(DocDir, Modules, []),
    output_coverage_index(DocDir, AppName, CoverageFiles),
    sin_utils:remove_code_paths(Paths).


%%--------------------------------------------------------------------
%% @spec output_coverage_index(DocDir, AppName, CoverageFiles) ->
%%   ok.
%% @doc 
%%  Output coverage information to make accessing the files a 
%%  bit easier.
%% @end
%% @private
%%--------------------------------------------------------------------
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
%% @spec make_index(FileList, Acc) -> DeepListOfLinks.
%% 
%% @doc 
%%  Render the list of modules into a deep list of links.
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
%% @spec setup_code_coverage(Modules) -> ok.
%% 
%% @doc 
%%  Instrument all of the modules for code coverage checks.
%% @end
%% @private
%%--------------------------------------------------------------------
setup_code_coverage([Module | T]) ->
    case cover:compile_beam(Module) of
        {error, _} ->
            ewl_talk:say("Couldn't add code coverage to ~w", [Module]);
        _ ->
            ok
    end,
    setup_code_coverage(T);
setup_code_coverage([]) ->
    ok.

%%--------------------------------------------------------------------
%% @spec output_code_coverage(DocDir, Modules, Acc) -> ListOModules.
%% 
%% @doc 
%%  Take the analysis from test running and output it to an html file.
%% @end
%% @private
%%--------------------------------------------------------------------
output_code_coverage(DocDir, [Module | T], Acc) ->
    File = lists:flatten([atom_to_list(Module), ".html"]),
    OutFile = filename:join([DocDir, File]),
    case cover:analyse_to_file(Module, OutFile, [html]) of
        {ok, _} ->
            output_code_coverage(DocDir, T, [{File, Module} | Acc]);
        {error, _} ->
            ewl_talk:say("Unable to write coverage information for ~w", 
                         [Module]),
            output_code_coverage(DocDir, T, Acc)
    end;
output_code_coverage(_DocDir, [], Acc) ->
    Acc.
%%--------------------------------------------------------------------
%% @spec run_module_tests(Modules) -> ok.
%% @doc
%%   Run tests for each module that has a test/0 function
%% @end 
%% @private
%%--------------------------------------------------------------------
run_module_tests([Module | T]) ->
    ewl_talk:say("Running tests for ~w.", [Module]),
    eunit:test(Module),
    run_module_tests(T);
run_module_tests([]) ->
    ok.

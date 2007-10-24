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
%%% @author Eric Merritt
%%% @doc
%%%   Uses dialyzer to analyze the project sources and output messages.
%%% @end
%%% Created : 31 Jan 2007 by Eric Merritt <cyberlync@gmail.com>
%%%---------------------------------------------------------------------------
-module(sin_analyze).

%% API
-export([analyze/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%   Run the analyze task.
%% 
%% @spec analyze() -> ok.
%% @end
%%--------------------------------------------------------------------
analyze(BuildRef, _) ->
    ewl_talk:say("Starting analyzation, this may take awhile ..."),
    Args = fconf:get_value(BuildRef, "build.args"),
    BuildDir = fconf:get_value(BuildRef, "build.dir"),
    PltPath = filename:join([BuildDir, "info", "dialyzer_plt"]),
    case Args of
        ["init" | _] ->
            generate_local_plt(BuildRef, PltPath);
        _ ->
            run_analyzer(BuildRef, BuildDir, PltPath)
    end.

%%====================================================================
%%% Internal functions
%%====================================================================


%%--------------------------------------------------------------------
%% @spec run_analyze(BuildDir, PltPath) -> ok.
%% 
%% @doc 
%%  Analyze the current project repos and print error and warning 
%%  messages.
%% @end
%%--------------------------------------------------------------------
run_analyzer(BuildRef, BuildDir, PltPath) ->         
    AppList = fconf:get_value(BuildRef, "project.apps"),
    RepoAppList = fconf:get_value(BuildRef, "project.repoapps"),
    case sin_utils:file_exists(PltPath) of
        false ->
            generate_local_plt(BuildRef, PltPath);
        true ->
            ok
    end,
    BuildPath = filename:join([BuildDir, "apps"]),
    Repo = fconf:get_value(BuildRef, "project.repository"),
    Codepaths = get_code_paths(Repo, RepoAppList, []),
    Codepaths2 = get_code_paths(BuildPath, AppList, [Codepaths]),
    Opts = [{files_rec, Codepaths2}, {from, byte_code},
            {init_plt, PltPath},
            {warnings,   [no_return,
                          no_unused,
                          no_improper_lists,
                          no_tuple_as_fun,
                          no_fun_app,
                          no_match,
                          no_comp,
                          no_guards,
                          no_unsafe_beam,
                          no_fail_call,
                          error_handling]}],
    ewl_talk:say("Starting analysis..."),
    output_info(dialyzer:run(Opts)).
    
%%--------------------------------------------------------------------
%% @spec generate_local_plt(PltPath) -> ok.
%% 
%% @doc 
%%  Generate a base plt to make use of.
%% @end
%%--------------------------------------------------------------------
generate_local_plt(BuildRef, PltPath) ->
    ewl_talk:say("Generating base plt, this could take a "
                 "really long time ..."),
    RepoAppList = fconf:get_value(BuildRef, "project.repoapps"),
    ProjectRepo = fconf:get_value(BuildRef, "project.repository"),
    Codepaths = get_code_paths(ProjectRepo, RepoAppList, []),
    Opts = [{files_rec, Codepaths}, 
            {from, byte_code},
            {output_plt, PltPath},
            {warnings,   [no_return,
                          no_unused,
                          no_improper_lists,
                          no_tuple_as_fun,
                          no_fun_app,
                          no_match,
                          no_comp,
                          no_guards,
                          no_unsafe_beam,
                          no_fail_call,
                         error_handling]}],
    output_info(dialyzer:run(Opts)).
    


%%--------------------------------------------------------------------
%% @spec output_info(Result) -> ok.
%% 
%% @doc 
%%  Print out teh informational output from dialyzer.
%% @end
%% @private
%%--------------------------------------------------------------------
output_info({ok, Warnings}) ->
    output_warnings(1, Warnings);
output_info({error, Warnings, Errors}) ->
    output_warnings(1, Warnings),
    output_errors(1, Errors);
output_info({error, Message}) ->
    ewl_talk:say(Message).

%%--------------------------------------------------------------------
%% @spec output_warnings(WarningList) -> ok.
%% 
%% @doc 
%%  Print out dialyzer warnings.
%% @end
%% @private
%%--------------------------------------------------------------------
output_warnings(N, [{{Module, Function, Arity}, Message} | T]) ->
    ewl_talk:say("Warning ~w: ~s:~w ~s", [N, Module, Function, Arity, Message]),
    output_warnings(N + 1, T);
output_warnings(N, [{{Module, Line}, Message} | T]) ->
    ewl_talk:say("Warning ~w: ~s:~w ~s", [N, Module, Line, Message]),
    output_warnings(N + 1, T);
output_warnings(_N, []) ->
    ok.

%%--------------------------------------------------------------------
%% @spec output_errors(ErrorList) -> ok.
%% 
%% @doc 
%%  Print out errors from dialyzer.
%% @end
%% @private
%%--------------------------------------------------------------------
output_errors(N, [Error | T]) ->
    ewl_talk:say("Error ~w: ~s", [N, Error]),
    output_errors(N + 1, T);
output_errors(_N, []) ->
    ok.


%%--------------------------------------------------------------------
%% @spec get_code_paths(BuildDir, Apps, Acc) -> EbinPaths.
%% 
%% @doc 
%%  Generate the code paths for the apps we will check with 
%%  dialyzer.
%% @end
%% @private
%%--------------------------------------------------------------------
get_code_paths(BuildDir, [{AppName, Vsn, _} | T], Acc) ->
    get_code_paths(BuildDir, [{AppName, Vsn} | T], Acc);
get_code_paths(BuildDir, [{AppName, Vsn} | T], Acc) ->
    Dir = filename:join([BuildDir, 
                         lists:flatten([atom_to_list(AppName), "-", Vsn]),
                         "ebin"]),
    get_code_paths(BuildDir, T, [Dir | Acc]); 
get_code_paths(_BuildDir, [], Acc) ->
    Acc.



%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%   Runs the 'test' function on all modules in an application
%%%   if that function exits.
%%% @end
%%% @copyright (C) 2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_xref).

-behaviour(sin_task).

-export([description/0, do_task/2]).

-include_lib("sinan/include/sinan.hrl").

-define(TASK, xref).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================
%% @doc provide a description for this task
-spec description() -> sin_task:task_description().
description() ->

    Desc = "This command the built in erlang xref checker on all code in the
        project. It outputs several different sections corresponding to the
        information available from xref. xref does have the problem that if
        dependent code is not compiled with debug_info it will display any calls
        to that code as unresolved. This is unfortunate much of the code
        in the core library is not compiled with debug info. So you will get
        false positive reports for this code.",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = false,
          deps = ?DEPS,
          desc = Desc,
          short_desc = "Runs xref on the project, to detect problems",
          example = "xref",
          opts = []}.

%% @doc do the xref task
-spec do_task(sin_config:config(), sin_state:state()) ->
                     sin_state:state().
do_task(_Config, State) ->
    ServerName = get_a_uniquish_name(),
    ExistingPaths = code:get_path(),
    xref:start(ServerName),

    Apps = lists:map(fun(#app{name=App}) ->
                             App
                     end, sin_state:get_value(release_apps, State)),

    ModuleInfo =
        lists:flatten(lists:map(fun(App) ->
                                        xref_app(State, ServerName, App),
                                        gather_modules(State, App)
                                end,
                                Apps)),

    lists:foreach(fun({Analysis, Name}) ->
                          ewl_talk:say("Looking for ~s", [Name]),
                          notify_user(State, ModuleInfo, Analysis,
                                      xref:analyze(ServerName, Analysis))
                  end,
                  [{undefined_function_calls, "Undefined Function Calls"},
                   {locals_not_used, "Unused Local Functions"},
                   {exports_not_used, "Unused Exported Functions"},
                   {deprecated_function_calls, "Calls to Deprecated Functions"}]),
    code:set_path(ExistingPaths),
    xref:stop(ServerName),
    State.

%% Get the module detail information from the config
-spec gather_modules(sin_state:state(), AppName::string()) ->
    [{Filename::string(), AppName::atom(), Extentions::string()}].
gather_modules(State, AppName) ->
    sin_state:get_value({apps, AppName, module_detail}, [], State).


%% @doc add the application to the specified xref system
-spec xref_app(sin_state:state(),
               ServerName::atom(), AppName::atom()) ->
    ok | fail.
xref_app(State, ServerName, AppName) ->
    Paths = sin_state:get_value({apps, AppName, code_paths}, State),

    xref:set_library_path(ServerName, Paths),

    AppDir = sin_state:get_value({apps, AppName, builddir}, State),

    case xref:add_application(ServerName, AppDir,
                              [{warnings, true}]) of
        {ok, _AppNameVsn} ->
            ok;
        Error = {error, Module, Reason} ->
            ewl_talk:say(Module:format_error(Reason)),
            ?SIN_RAISE(State, {Error, Module:format_error(Reason)})
    end.

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc print out the appropriate message for the responce
-spec notify_user(sin_state:state(),
                  [term()], atom(), {error, atom(), term()} |
                  {ok, [term()]}) ->
    ok.
notify_user(State, _, _, Error = {error, Module, Reason}) ->
    ewl_talk:say(Module:format_error(Reason)),
    ?SIN_RAISE(State, {Error, Module:format_error(Reason)});
notify_user(_, ModuleInfo, Analysis, {ok, AnswerList}) ->
    lists:foreach(fun(Answer) ->
                          display_answer(Analysis, ModuleInfo, Answer)
                  end, AnswerList).

%% @doc print out an answer from the xref system
-spec display_answer(atom(), [term()], term()) ->
    ok.
display_answer(exports_not_used, _, MFA) ->
    case is_eunit_test(MFA) of
        false ->
            ewl_talk:say("~s is exported but not used", [format_mfa(MFA)]);
        true ->
            ok
    end;
display_answer(locals_not_used, _, MFA) ->
    ewl_talk:say("~s is defined but not used", [format_mfa(MFA)]);
display_answer(undefined_function_calls, ModuleInfo, {Caller, Callee}) ->
    ewl_talk:say("~s:~s calls the undefined function ~s ",
                 [find_module(ModuleInfo, Caller),
                  format_mfa(Caller),
                  format_mfa(Callee)]);
display_answer(deprecated_function_calls, ModuleInfo, {Caller, Callee}) ->
    ewl_talk:say("~s:~s calls the deprecated function ~s ",
                 [find_module(ModuleInfo, Caller),
                  format_mfa(Caller),
                  format_mfa(Callee)]).

%% @doc return a name that is probably unique for the xref server.
-spec get_a_uniquish_name() ->
    atom().
get_a_uniquish_name() ->
    erlang:list_to_atom(erlang:integer_to_list(erlang:phash2({node(), now()}))).

%% @doc format the module, function, arity in a way that is readable to a human
-spec format_mfa({atom(), atom(), integer()}) ->
    string().
format_mfa({Module, Function, Arity}) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]).

%% @doc check to see if the M,F, A is a test function, if so don't worry about
%% exporting it
-spec is_eunit_test({atom(), atom(), integer()}) ->
    boolean().
is_eunit_test({M, F, _A}) ->
    HasTest = case lists:keyfind(exports, 1, M:module_info()) of
                  {exports, List} ->
                      lists:member({test, 0}, List);
                  _ ->
                      false
              end,
    HasTest orelse check_test(F, "_test") orelse check_test(F, "_test_").

%% @doc check if this is a test funciotn
-spec check_test(atom(), string()) ->
     boolean().
check_test(F, T) ->
    FunStr = atom_to_list(F),
    Len = erlang:length(FunStr),
    Index = string:str(FunStr, T),
    Len - Index == 0.

%% @doc find the module file in the list of modules that was built
-spec find_module([term()], {Module::atom(), atom(), atom()}) ->
    File::string().
find_module(ModuleInfo, {M, _, _}) ->
    case lists:keyfind(M, 2, ModuleInfo) of
        {File, M, _} ->
            File;
        _ ->
            ""
    end.

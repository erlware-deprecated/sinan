%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%  Supports building individual erl files in an application
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%--------------------------------------------------------------------------
-module(sin_task_build).

-behaviour(sin_task).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("internal.hrl").

%% API
-export([description/0, do_task/1]).

-record(env,  {project_dir,
               build_dir,
               apps_build_dir,
               sig_dir,
               app_list,
               deps}).

-define(SIGNS, "erldeps").
-define(TASK, build).
-define(DEPS, [depends]).

%%====================================================================
%% API
%%====================================================================

%% @doc return a description of this task to the caller
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Compiles all of the compilable files in the project",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  example = "build",
	  short_desc = "compiles the files in the project",
	  desc = Desc,
	  opts = []}.

%% @doc run the build task.
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    ensure_build_dir(BuildRef),
    Apps = sin_config:get_value(BuildRef, "project.allapps"),
    NApps = reorder_apps_according_to_deps(Apps),
    RawArgs = sin_config:get_value(BuildRef,
                                         "tasks.build.compile_args", ""),
    NArgs = sin_build_arg_parser:compile_build_args(RawArgs),
    build_apps(BuildRef, NApps, NArgs).

%%====================================================================
%%% Internal functions
%%====================================================================

%% @doc Given a list of apps and dependencies creates an ordered build list for
%% the apps.
-spec reorder_apps_according_to_deps([AppInfo::term()]) -> list().
reorder_apps_according_to_deps(AllApps) ->
    ReOrdered = lists:foldr(
                  fun ({App, _, {Deps, IncDeps}, _}, Acc) ->
                          AllDeps = Deps++IncDeps,
                          case map_deps(App, AllDeps, AllApps) of
                              [] ->
                                  [{'NONE', to_list(App)} | Acc];
                              Else ->
                                  Else ++ Acc
                          end
                  end, [], AllApps),
    case sin_topo:sort(ReOrdered) of
        {ok, DepList} ->
            DepList;
        {cycle, CycleList} ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE_DA(cycles_detected,
                          "A cycle was detected in the dependency graph "
                          " I don't know how to build cycles. ~p",
                          [CycleList])
    end.

%% @doc Map the lists of dependencies for 'App' into a pairs for the topo sort.
-spec map_deps(atom(), list(), list()) -> list().
map_deps(App, Deps, AllApps) ->
 	 lists:foldr(fun (DApp, Acc) ->
 			     case lists:keymember(DApp, 1, AllApps) of
 				 true ->
 				     [{to_list(DApp), to_list(App)} | Acc];
 				 false ->
 				     Acc
 			     end
 		     end, [], Deps).

%% @doc Change an atom to a list of the argument is an atom, otherwise just
%% return the arg.
-spec to_list(atom()) -> string().
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Atom) when is_list(Atom) ->
    Atom.

%% @doc Build the apps in the list.
build_apps(BuildRef, Apps, Args) ->
    AppList = sin_config:get_value(BuildRef, "project.allapps"),
    AllDeps = sin_config:get_value(BuildRef, "project.alldeps"),
    ProjectDir = sin_config:get_value(BuildRef, "project.dir"),
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    AppBDir = filename:join([BuildDir, "apps"]),
    SigDir = filename:join([BuildDir, "sigs"]),

    build_apps(BuildRef, #env{project_dir=ProjectDir,
                              build_dir=BuildDir,
                              apps_build_dir=AppBDir,
                              sig_dir=SigDir,
                              app_list=AppList,
                              deps=AllDeps},
               Apps, Args).

%% @doc build the apps as they come up in the list.
build_apps(BuildRef, BuildSupInfo, AppList, Args) ->
    lists:foldl(fun ('NONE', BuildRef2) ->
			% We ignore an app type of none, its a remnent
			% of the reorder process.
			BuildRef2;
		    (App, BuildRef2) ->
			build_app(BuildRef2, BuildSupInfo, App, Args)
                  end, BuildRef, AppList).

%% @doc Build an individual otp application.
build_app(BuildRef, Env, AppName, Args) ->
    AppVsn = sin_config:get_value(BuildRef, "apps." ++ AppName ++ ".vsn"),
    AppDir = sin_config:get_value(BuildRef, "apps." ++ AppName
                                        ++ ".basedir"),
    BuildTarget = lists:flatten([AppName, "-", AppVsn]),
    AppBuildDir = filename:join([Env#env.apps_build_dir, BuildTarget]),
    BuildRef2 = sin_config:store(BuildRef, "apps." ++ AppName ++ ".builddir",
				       AppBuildDir),
    Target = filename:join([AppBuildDir, "ebin"]),
    TargetSrcDir = filename:join([AppBuildDir, "src"]),
    SrcDir = filename:join([AppDir, "src"]),
    ExistingPaths = code:get_path(),
    {EbinPaths, Includes} = setup_code_path(BuildRef2, Env, AppName),
    BuildRef3 = sin_config:store(BuildRef2, "apps." ++ AppName ++ ".code_paths",
                   [Target | EbinPaths]),
    Options = Args ++ [{outdir, Target}, strict_record_tests,
                       return_errors, return_warnings,
                       {i, filename:join([AppDir, "include"])},
                       % Search directory with .hrl files
                       % generated from .asn1 files.
                       {i, TargetSrcDir} | Includes],
    event_compile_args(BuildRef3, Options),
    Ignorables = sin_config:get_value(BuildRef3, "ignore_dirs", []),

    % Ignore the build dir when copying or we will create a deep monster in a
    % few builds
    BuildDir = sin_config:get_value(BuildRef3, "build_dir"),
    sin_utils:copy_dir(
      AppBuildDir, AppDir, "", [BuildDir | Ignorables]),
    code:add_patha(Target),
    Modules = gather_modules(BuildRef3, AppName, SrcDir),
    BuildRef4 = sin_config:store(BuildRef3, "apps." ++ AppName ++ ".module_detail",
				 Modules),

    NModules = lists:map(fun({File, _AbsName, Ext}) ->
                                 build_file(BuildRef4, SrcDir, File, Ext,
                                            Options, Target)
                         end, Modules),
    check_for_errors(NModules),
    code:set_path(ExistingPaths),
    BuildRef4.

event_compile_args(BuildRef, Options) ->
	case sin_config:get_value(BuildRef, "task.build.print_args", undefined) of
	    undefined ->
	        ok;
	    true ->
                ewl_talk:say("Compile args:~n~p", [Options]);
	    "True" ->
		ewl_talk:say("Compile args:~n~p", [Options])
         end.

%% @doc Check the module list for errors throw an exceptions.
check_for_errors(ModuleList) ->
    case lists:member({sinan, error}, ModuleList) of
	true ->
	    sin_error_store:signal_error(),
	    ?SIN_RAISE(build_errors);
	false ->
	    ok
    end.

%% @doc Gather code paths and includes from the dependency list.
setup_code_path(BuildRef, Env, AppName) ->
    AtomApp = list_to_atom(AppName),
    case get_app_from_list(AtomApp, Env#env.app_list) of
        not_in_list ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE_DA(app_name_not_in_list,
                          "App ~s is not in the list of project apps. "
                          "This shouldn't happen!!",
                          [AppName]);
        {_, _, {Deps, IncDeps}, _} ->
            extract_info_from_deps(BuildRef, Deps++IncDeps, element(1,Env#env.deps), [], [], [])
	end.

%% @doc Gather path and include information from the dep list.
extract_info_from_deps(BuildRef, [AppName | T], AppList, Marked, Acc, IAcc) ->
    case lists:member(AppName, Marked) of
        false ->
            case get_app_from_list(AppName, AppList) of
                not_in_list ->
		    sin_error_store:signal_error(),
                    ?SIN_RAISE_DA(app_name_not_in_list,
                                  "App ~s is not in the list of project apps. "
                                  "This shouldn't happen!!!",
                                  [AppName]);
                {_, _, {Deps, IncDeps}, Path} ->
                    Ebin = filename:join([Path, "ebin"]),
                    Include = {i, filename:join([Path, "include"])},
                    code:add_patha(Ebin),
                    extract_info_from_deps(BuildRef, T, AppList ++ Deps ++ IncDeps,
                                           Marked,
                                           [Ebin | Acc],
                                           [Include | IAcc])
            end;
        true ->
            extract_info_from_deps(BuildRef, T, AppList, Marked, Acc,
                                   IAcc)
    end;
extract_info_from_deps(_, [], _, _, Acc, IAcc) ->
    {Acc, IAcc}.

%% @doc Get the app from the app list.
get_app_from_list(App, AppList) ->
    case lists:keysearch(App, 1, AppList) of
        {value, Entry} ->
            Entry;
        false ->
            not_in_list
    end.

%% @doc Gather the list of modules that currently may need to be built.
gather_modules(BuildRef, AppName, SrcDir) ->
    ModuleList = sin_config:get_value(BuildRef,
                                 "apps." ++ AppName ++ ".modules"),
    FileList =
        filelib:fold_files(SrcDir,
                           "(.+\.erl|.+\.yrl|.+\.asn1|.+\.asn)$",
                   true, % Recurse into subdirectories of src
                   fun(File, Acc) ->
                           Ext = filename:extension(File),
                           [{File, module_name(File), Ext} | Acc]
                   end, []),
    reorder_list(ModuleList,
                 filter_file_list(FileList, ModuleList)).


%% @doc Extract the module name from the file name.
module_name(File) ->
    list_to_atom(filename:rootname(filename:basename(File))).

%% @doc Reorder the list according to whats in the *.app. This will allow intra
%% application compile time dependencies.
reorder_list(ModList, FileList) ->
    Res = lists:foldr(
	    fun (Mod, {Acc,OkFlag}) ->
		    case get_file_list(Mod, FileList) of
			not_in_list ->
			    ewl_talk:say("The module specified by ~w is not "
					 "on the filesystem!! Not building.", [Mod]),
			    {Acc, not_ok};
			Entry ->
			    {[Entry | Acc], OkFlag}
		    end
	    end, {[], ok}, ModList),
    case Res of
	{Acc, ok} ->
	    lists:reverse(Acc);
	{_, not_ok} ->
	    sin_error_store:signal_error(),
	    ?SIN_RAISE(build_errors)
    end.

%% @doc Get the entry specified by name from the list in module list.
get_file_list(ModuleName, FileList) ->
    case lists:keysearch(ModuleName, 2, FileList) of
	{value, Entry} ->
	    Entry;
	false ->
	    not_in_list
    end.

%% @doc Filter the list of files keeping those that are in the module list.
filter_file_list(FileList, ModuleList) ->
    lists:foldr(
      fun ({File, AbsName, _}=Entry, Acc) ->
	      case lists:member(AbsName, ModuleList) of
		  true ->
		      [Entry | Acc];
		  false ->
		      ewl_talk:say("Module (~w) in file ~s is not in the "
				   "module list. Removing from build queue.",
				   [AbsName, File]),
		      Acc
	      end
      end, [], FileList).

%% @doc Build the file specfied by its arguments
build_file(BuildRef, SrcDir, File, Ext, Options, Target) ->
    FileName = filename:join([SrcDir, File]),
    build_file(BuildRef, FileName, Ext, Options, Target).

%% @doc Do the actual compilation on the file.
build_file(BuildRef, File, ".erl", Options, Target) ->
   case needs_building(BuildRef, File, ".erl", Target, ".beam") of
       true ->
	   ewl_talk:say("Building ~s", [File]),
           Result = case compile:file(File, Options) of
			{ok, ModuleName} ->
			    ModuleName;
			{ok, ModuleName, []} ->
			    ModuleName;
			{ok, ModuleName, Warnings} ->
			    ewl_talk:say(gather_fail_info(Warnings, "warning")),
			    ModuleName;
			{error, Errors, Warnings} ->
			    ewl_talk:say(lists:flatten([gather_fail_info(Errors, "error"),
							gather_fail_info(Warnings, "warning")])),
			    {sinan,  error};
			error ->
			    ewl_talk:say("Unknown error occured during build"),
			    {sinan, error}
		    end,
	   case Result of
	       Res when is_atom(Res) ->
		   save_module_dependencies(BuildRef, File, Options);
	       _ ->
		   Result
	   end;
       false ->
           ok
   end;
build_file(BuildRef, File, ".yrl", Options, Target) ->
    case needs_building(BuildRef, File, ".yrl", Target, ".beam") of
        true ->
            ErlFile = filename:basename(File, ".yrl"),
            AppDir = filename:dirname(Target),
            ErlTarget = filename:join([AppDir,"src"]),
            ErlName = filename:join([ErlTarget,
                                     lists:flatten([ErlFile, ".erl"])]),
            ewl_talk:say("Building ~s", [File]),
            case yecc:file(File, [{parserfile, ErlName} |
                                  strip_options(Options)]) of
                {ok, _ModuleName} ->
                    build_file(BuildRef, ErlName, ".erl",
                               Options, Target);
                {ok, _ModuleName, []} ->
                    build_file(BuildRef, ErlName, ".erl",
                               Options, Target);
                {ok, _ModuleName, Warnings} ->
                    ewl_talk:say(gather_fail_info(Warnings, "warning")),
                    ok;
                {error, Errors, Warnings} ->
                    ewl_talk:say(lists:flatten([gather_fail_info(Errors, "error"),
                                                gather_fail_info(Warnings, "warning")])),
                    error
            end;
        false ->
            ok
    end;
build_file(BuildRef, File, Ext=".asn1", Options, Target) ->
    build_asn1(BuildRef, File, Ext, Options, Target);
build_file(BuildRef, File, Ext=".asn", Options, Target) ->
    build_asn1(BuildRef, File, Ext, Options, Target);
build_file(_BuildRef, File, _, _Options, _Target) ->
    ewl_talk:say("Got file ~s with an extention I do not know how to build. "
		 "Ignoring!",  [File]).

%% @doc Do the actual compilation on the .asn1/.asn file.
build_asn1(BuildRef, File, Ext, Options, Target) ->
    case needs_building(BuildRef, File, Ext, Target, ".beam") of
        true ->
            ErlFile = filename:basename(File, Ext),
            AppDir = filename:dirname(Target),
            ErlTarget = filename:join([AppDir,"src"]),
            ErlName = filename:join([ErlTarget,
                                     lists:flatten([ErlFile, ".erl"])]),
            ewl_talk:say("Building ~s", [File]),
            case asn1ct:compile(File, [{outdir, ErlTarget}, noobj] ++
                                strip_options(Options)) of
                ok ->
                    build_file(BuildRef, ErlName, ".erl", Options, Target);
                {error, Errors} ->
                    ewl_talk:say(gather_fail_info(Errors, "error")),
                    error
            end;
        false ->
            ok
    end.

%% @doc Strip options for the yecc. Otherwise we get a bad arg error.
strip_options(Opts) ->
    lists:foldr(
      fun (Opt = {parserfile, _}, Acc) ->
	      [Opt | Acc];
	  (Opt = {includefile, _}, Acc) ->
	      [Opt | Acc];
	  (Opt = {report_errors, _}, Acc) ->
	      [Opt | Acc];
	  (Opt = {report_warnings, _}, Acc) ->
	      [Opt | Acc];
	  (Opt = {report, _}, Acc) ->
	      [Opt | Acc];
	  (Opt = {return_warnings, _}, Acc) ->
	      [Opt | Acc];
	  (Opt = {verbose, _}, Acc) ->
	      [Opt | Acc];
	  (_, Acc) ->
	      Acc
      end, [], Opts).

%% @doc Check to see if the file needs building. If it does run the passed in
%% build fin. If thats successful then update the sig.
base_needs_building(FileName, Ext, TargetDir, TargetExt) ->
    Name = filename:basename(FileName, Ext),
    NewFile = lists:flatten([Name, TargetExt]),
    TFileName = filename:join([TargetDir, NewFile]),
    sin_sig:target_changed(FileName, TFileName).

%% @doc Check to see if the file needs building. If it does run the passed in
%% build fin. If thats successful then update the sig.
needs_building(BuildRef, FileName, ".erl", TargetDir, TargetExt) ->
    case base_needs_building(FileName, ".erl", TargetDir, TargetExt) of
	false ->
	    check_module_deps(BuildRef, FileName);
	_ ->
	    true
    end;
needs_building(_, FileName, Ext, TargetDir, TargetExt) ->
    base_needs_building(FileName, Ext, TargetDir, TargetExt).

%% @doc Check to see if any of the dependencies on a file have changed. If they
%% have then return true, otherwise false.
check_module_deps(BuildRef, FileName) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    case sin_sig:get_sig_info(?SIGNS, BuildDir, FileName) of
        undefined ->
            false;
        Terms ->
            lists:foldl(fun({Include, Ts}, Acc) ->
                                case file:read_file_info(Include) of
                                    {ok, TargetInfo} when TargetInfo#file_info.mtime > Ts ->
                                        true;
                                    _ ->
                                        Acc
                                end
                        end,
                        false,
                        Terms)
    end.

%% @doc Ensure that the build dir exists and is ready to accept files.
ensure_build_dir(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    AppsDir = lists:flatten([BuildDir, "apps", "tmp"]),
    filelib:ensure_dir(AppsDir).

%% @doc Gather up all the errors and warnings for output.
gather_fail_info(ListOfProblems, Type) ->
    R = lists:foldr(fun ({File, Problems}, Acc) ->
			    gather_fail_info(File, Problems, Acc, Type)
		    end, [], ListOfProblems),
    lists:reverse(R).

%% @doc Actual get the failer detail information and add it to the accumulator.
gather_fail_info(File, ListOfProblems, Acc, WoE) ->
    lists:foldr(
      fun ({Line, Type, Detail}, Acc1) when is_atom(Line) ->
              [lists:flatten([File, $:, atom_to_list(Line),
                              $:, WoE, $:, Type:format_error(Detail),
                              $\n]) | Acc1];
          ({Line, Type, Detail}, Acc1) when is_integer(Line) ->
              [lists:flatten([File, $:, integer_to_list(Line),
                              $:, WoE, $:, Type:format_error(Detail),
                              $\n]) | Acc1];
          ({Type, Detail}, Acc1) ->
              [lists:flatten([File, ":noline:", WoE, $:,
                              Type:format_error(Detail), $\n]) | Acc1]
      end, Acc, ListOfProblems).

%% @doc Get the list of processed included files from the specified *.erl
get_hrl_files(File, Includes) ->
    {ok, Forms} = epp:parse_file(File, Includes,[]),
    HrlFiles = lists:foldl(fun({attribute, _ , file, {Include, _}}, Acc) ->
                                   case Include of
                                       File ->
                                           Acc;
                                       [] ->
                                           Acc;
                                       _ ->
                                           [Include | Acc]
                                   end;
                              (_, Acc) ->
                                   Acc
                           end,
                           [],
                           Forms),
    lists:foldl(fun(Hrl, Acc) ->
                        {ok, FileInfo} =  file:read_file_info(Hrl),
                        [{Hrl, FileInfo#file_info.mtime} | Acc]
                end,
                [],
                HrlFiles).

%% @doc Find and save eh module dependencies for a specific module.
save_module_dependencies(BuildRef, File, Options) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    IncludeDirs = lists:reverse(lists:foldl(fun({i, Include}, Acc) ->
                                                    [Include | Acc];
                                               (_, Acc) ->
                                                    Acc
                                            end,
                                            [],
                                            Options)),
    sin_sig:save_sig_info(?SIGNS, BuildDir, File, get_hrl_files(File, IncludeDirs)).

%%====================================================================
%% Tests
%%====================================================================
reorder_app_test() ->
    AppList = [{app1, "123", {[app2, stdlib], []}, "path"},
               {app2, "123", {[app3, kernel], []}, "path"},
               {app3, "123", {[kernel], []}, "path"}],
    NewList  = reorder_apps_according_to_deps(AppList),
    ?assertMatch(['NONE', "app3", "app2", "app1"], NewList),
    AppList2 = [{app1, "123", {[app2, zapp1, stdlib], []}, "path"},
                {app2, "123", {[app3, kernel], []}, "path"},
                {app3, "123", {[kernel, zapp2], []}, "path"},
                {zapp1, "vsn", {[app2, app3, zapp2], []}, "path"},
                {zapp2, "vsn", {[kernel], []}, "path"},
                {zapp3, "vsn", {[], []}, "path"}],
    NewList2 = reorder_apps_according_to_deps(AppList2),
    ?assertMatch(['NONE',"zapp2","app3","app2","zapp1","app1",
                  "zapp3"], NewList2).




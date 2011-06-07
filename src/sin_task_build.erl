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
-export([description/0,
	 do_task/1,
	 gather_fail_info/2,
	 strip_options/1,
	 get_target/4,
	 format_exception/1]).

-record(env,  {project_dir,
               build_dir,
               apps_build_dir,
               sig_dir,
               app_list,
               deps}).

-define(SIGNS, "moddeps").
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

%% @doc Gather up all the errors and warnings for output.
-spec gather_fail_info([term()], string()) ->
    [string()].
gather_fail_info(ListOfProblems, Type) ->
    R = lists:foldr(fun ({File, Problems}, Acc) ->
			    gather_fail_info(File, Problems, Acc, Type)
		    end, [], ListOfProblems),
    lists:reverse(R).

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
get_target(FileName, Ext, TargetDir, TargetExt) ->
    Name = filename:basename(FileName, Ext),
    NewFile = lists:flatten([Name, TargetExt]),
    filename:join([TargetDir, NewFile]).


%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

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
            ?SIN_RAISE(cycles_detected,
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
    Cache = dict:new(),
    {_NewCache, NewBuildRef} =
	lists:foldl(fun ('NONE', Acc) ->
			    % We ignore an app type of none, its a remnent
			    % of the reorder process.
			    Acc;
			(App, {Cache2, BuildRef2}) ->
			    build_app(BuildRef2, Cache2, BuildSupInfo, App, Args)
		    end, {Cache, BuildRef}, AppList),
    NewBuildRef.

%% @doc Build an individual otp application.
build_app(BuildRef, Cache, Env, AppName, Args) ->
    AppVsn = sin_config:get_value(BuildRef, "apps." ++ AppName ++ ".vsn"),
    AppDir = sin_config:get_value(BuildRef, "apps." ++ AppName
                                        ++ ".basedir"),
    BuildTarget = lists:flatten([AppName, "-", AppVsn]),
    AppBuildDir = filename:join([Env#env.apps_build_dir, BuildTarget]),
    BuildRef2 = sin_config:store(BuildRef, "apps." ++ AppName ++ ".builddir",
				       AppBuildDir),
    Target = filename:join([AppBuildDir, "ebin"]),

    Ignorables = sin_config:get_value(BuildRef2, "ignore_dirs", []),

    % Ignore the build dir when copying or we will create a deep monster in a
    % few builds
    BuildDir = sin_config:get_value(BuildRef2, "build_dir"),
    sin_utils:copy_dir(AppBuildDir, AppDir, "", [BuildDir | Ignorables]),


    {EbinPaths, Includes} = setup_code_path(BuildRef2, Env, AppName),

    ExistingPaths = code:get_path(),
    code:add_patha(Target),
    {NewCache, NewFileList} = process_source_files(BuildRef2,
						    Cache,
						    Env#env.build_dir,
						    Target,
						    Includes,
						    gather_modules(BuildRef,
								   AppName)),
    BuildRef3 = sin_config:store(BuildRef2,
				 [{"apps." ++ AppName ++ ".code_paths",
				  [Target | EbinPaths]},
				  {"apps." ++ AppName ++ ".file_list",
				   NewFileList}]),

    build_sources(BuildRef3, NewFileList,
		  Includes, Args, AppDir, Target),

    % Do the source build
    code:set_path(ExistingPaths),
    {NewCache, BuildRef3}.

%% @doc go through each source file building with the correct build module.
-spec build_sources(sin_config:config(), [tuple()], [string()],
		    list([term()]), string(), string()) ->
    ok.
build_sources(BuildRef, FileList, Includes,
	       Args, AppDir,  Target) ->

    lists:map(fun({{File, _, _, _, _}, {changed, _, _, BuildModule}}) ->
		      Options = Args ++ [{outdir, Target},
					 strict_record_tests,
					 return_errors, return_warnings,
					 {i, filename:join([AppDir, "include"])},
					 % Search directory with .hrl files
					 Includes],
		      event_compile_args(BuildRef, Options),
		      BuildModule:build_file(BuildRef, File, Options, Target);
		 (_) ->
		      []
	      end, FileList).

%% @doc if the right config values is specified event the compile args
event_compile_args(BuildRef, Options) ->
	case sin_config:get_value(BuildRef,
				  "task.build.print_args", undefined) of
	    undefined ->
	        ok;
	    true ->
                ewl_talk:say("Compile args:~n~p", [Options]);
	    "True" ->
		ewl_talk:say("Compile args:~n~p", [Options])
         end.

%% @doc process dependencies for each source file, the sort the list in build
%% order.
process_source_files(BuildRef, Cache, BuildDir, TargetDir,
		     Includes, FileList) ->
   {NewCache, TmpNewList} =
	lists:foldl(fun(FileInfo, {Cache2, Acc}) ->
			    {Cache3, NewFileInfo} =
				process_source_file(BuildRef,
						    Cache2,
						    FileList,
						    BuildDir,
						    TargetDir,
						    Includes,
						    FileInfo),
			    {Cache3, [NewFileInfo | Acc]}
		    end, {Cache, []}, FileList),
    {NewCache, topo_sort_file_list(TmpNewList)}.

topo_sort_file_list(FileList) ->
    {ok, Data} =
	sin_topo:sort(
	  lists:flatten(
	    lists:map(fun({{File, _, _, _, []}, _}) ->
			      {'NONE', File};
			 ({{File, _, _, _, Deps}, _}) ->
			      lists:map(fun({DepFile, _}) ->
						{DepFile, File}
					end, Deps)
		      end,
		      FileList))),
    lists:reverse(
      lists:foldl(fun('NONE', Acc) ->
			  Acc;
		     (File, Acc) ->
			  case ec_lists:find(fun({{LFile, _, _, _, _}, _}) ->
						     LFile == File
					     end, FileList) of
			      error ->
				  Acc;
			      {ok, Item} ->
				  [Item | Acc]
			  end
		  end,
		  [], Data)).

process_source_file(_BuildRef,
		    Cache,
		    FileList,
		    BuildDir,
		    TargetDir,
		    Includes,
		    {File, Module, Ext, AtomExt, TestImplementations}) ->
    BuildModule = get_build_module(AtomExt),
    {Changed, {NewCache, {Deps, NewTI, TestedModules}}} =
	has_changed(BuildDir, TargetDir, Cache, File, Ext, Includes,
		BuildModule, TestImplementations,
		FileList),
    {NewCache, {{File, Module, Ext, AtomExt, Deps},
		{Changed, NewTI, TestedModules, BuildModule}}}.

has_changed(BuildDir, TargetDir, Cache, File, Ext, Includes,
	    BuildModule, TestImplementations,
	    FileList) ->
        case contents_changed(TargetDir, File, Ext, BuildModule) of
	    true ->
		Ret = save_real_dependencies(Cache, BuildDir,
					     FileList, TestImplementations,
					     File, Includes),
		{changed, Ret};
	    false ->
		dependencies_have_changed(BuildDir, Cache, FileList,
					  TestImplementations, File, Includes)
	end.

dependencies_have_changed(BuildDir, Cache, FileList, TestImplementations,
			  File, Includes) ->
        case sin_sig:get_sig_info(?SIGNS, BuildDir, File) of
	    undefined ->
		Ret = save_real_dependencies(Cache, BuildDir, FileList,
					     TestImplementations, File,
					     Includes),
		{changed, Ret};
	    Terms  ->
		case check_deps_for_change(Terms) of
		    true ->
			Ret = save_real_dependencies(Cache, BuildDir, FileList,
						     TestImplementations, File,
						     Includes),
			{changed, Ret};
		    false ->
			{not_changed, {Cache, Terms}}
		end
	end.

check_deps_for_change({Deps, _, _}) ->
   check_deps_for_change(Deps);
check_deps_for_change([{File, TS} | Rest]) ->
    case file:read_file_info(File) of
	{ok, TargetInfo}
	when TargetInfo#file_info.mtime > TS ->
	    true;
	_ ->
	    check_deps_for_change(Rest)
    end;
check_deps_for_change([]) ->
    false.

save_real_dependencies(Cache, BuildDir,
		       FileList, TestImplementations, File, Includes) ->
    {NewCache, Dependencies} = resolve_dependencies(Cache, FileList,
						    TestImplementations,
						    File,
						    Includes),
    sin_sig:save_sig_info(?SIGNS, BuildDir, File, Dependencies),
    {NewCache,  Dependencies}.

resolve_dependencies(Cache, FileList, TestImplementations,  File, Includes) ->
    {DepArtifacts, NewTestImplementations, TestedModules} =
	parse_attribute_dependencies(TestImplementations, File, Includes),

    {NewCache, NewDeps} = realize_dependent_artifacts(Cache, DepArtifacts,
						     FileList),
    {NewCache, {NewDeps,
		NewTestImplementations, TestedModules}}.

realize_dependent_artifacts(Cache, DepArtifacts, FileList) ->
    lists:foldl(fun({file, File}, {Cache2, Acc}) ->
		      {Cache3, TS} = get_ts(Cache2, File),
		       {Cache3, [{File, TS} | Acc]};
		  ({module, Module}, {Cache2, Acc}) ->
		       File = realize_module(Module, FileList),
		       {Cache3, TS} = get_ts(Cache2, File),
		       {Cache3, [{File, TS} | Acc]}
	      end, {Cache, []}, DepArtifacts).

get_ts(Cache, File) ->
    case dict:find({ts, File}, Cache) of
	{ok, TS} ->
	    {Cache, TS};
	error ->
	    TS = case file:read_file_info(File) of
		{ok, FileInfo} ->
			 FileInfo#file_info.mtime;
		     Error ->
			 ?SIN_RAISE({error_reading_timestamp, File, Error})
		 end,
	    {dict:store({ts, File}, TS, Cache), TS}
    end.
realize_module(Module, FileList) ->
    case lists:keyfind(Module, 2, FileList) of
	{File, Module, _, _, _} ->
	    File;
	false ->
	    %% Its a dependent file then, somewhere in the code path
	    case code:which(Module) of
		FileName when is_list(FileName) ->
		    FileName;
		_ ->
		    ?SIN_RAISE({dependent_module_not_found, Module})
	    end
    end.

parse_attribute_dependencies(TestImplementations, File, Includes) ->
    %% Drop off the first attribute, we don't care so much about that, it just
    %% points to itself.
    {ok, Forms} =
	epp:parse_file(File, Includes, []),
    lists:foldl(fun(Attr, Acc) ->
			parse_form(File, Attr, Acc)
		end,
		{[], TestImplementations, []},
		Forms).

parse_form(_File, {attribute, _ , file, {[], _}}, Acc) ->
    Acc;
parse_form(File, {attribute, _ , file, {File, _}}, Acc) ->
    Acc;
parse_form(_File, {attribute, _ , file, {Include, _}},
	   {Deps, TestImplementations, TestedModules}) ->
    {[{file, Include} | Deps], TestImplementations, TestedModules};
parse_form(_File, {attribute, _, compile, {parse_transform, proper_transformer}},
	   {Deps, TestImplementations, TestedModules}) ->
    {Deps, [proper | TestImplementations], TestedModules};
parse_form(_File, {attribute, _, compile, {parse_transform, eunit_autoexport}},
	   {Deps, TestImplementations, TestedModules}) ->
    {Deps, [eunit | TestImplementations], TestedModules};
parse_form(_File, {attribute, _, compile, {parse_transform, Module}},
	   {Deps, TestImplementations, TestedModules}) ->
    {[{module, Module} | Deps], TestImplementations, TestedModules};
parse_form(_File, {attribute, _, behaviour, Module},
	   {Deps, TestImplementations, TestedModules}) ->
    {[{module, Module} | Deps], TestImplementations,
     TestedModules};
parse_form(_File, {attribute, _, behavior, Module},
	   {Deps, TestImplementations, TestedModules}) ->
    {[{module, Module} | Deps], TestImplementations,
     TestedModules};
parse_form(_File, {attribute, _, tested_modules, ModList},
	  {Deps, TestImplementations, TestedModules}) ->
    {Deps, TestImplementations, ModList ++ TestedModules};
parse_form(_, _, Acc) ->
    Acc.

contents_changed(BuildDir, File, Ext, BuildModule) ->
    TargetFile = BuildModule:get_target(BuildDir, File, Ext),
    case sin_sig:target_changed(File, TargetFile) of
	false ->
	    false;
	_ ->
	    true
    end.

get_build_module('.erl') ->
    sin_compile_erl;
get_build_module('.yrl') ->
    sin_compile_yrl;
get_build_module(Ext) ->
    ?SIN_RAISE({unsupported_file_type, Ext}).

%% @doc Gather code paths and includes from the dependency list.
setup_code_path(BuildRef, Env, AppName) ->
    AtomApp = list_to_atom(AppName),
    case get_app_from_list(AtomApp, Env#env.app_list) of
        not_in_list ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE(app_name_not_in_list,
		       "App ~s is not in the list of project apps. "
		       "This shouldn't happen!!",
		       [AppName]);
        {_, _, {Deps, IncDeps}, _} ->
            get_compile_time_deps(BuildRef,
				  extract_info_from_deps(BuildRef,
							 Deps ++ IncDeps,
							 element(1, Env#env.deps),
							 [], [], []))
	end.
%% @doc gather up the static compile time dependencies
get_compile_time_deps(BuildRef, {Acc, IAcc}) ->
    lists:foldl(fun({_, _, _, Path}, {NA, NIA}) ->
			Ebin = filename:join([Path, "ebin"]),
			Include = {i, filename:join([Path, "include"])},
			{[Ebin | NA], [Include | NIA]}
		end,
		{Acc, IAcc},
		sin_config:get_value(BuildRef, "project.compile_deps")).

%% @doc Gather path and include information from the dep list.
extract_info_from_deps(BuildRef, [AppName | T],
		       AppList, Marked, Acc, IAcc) ->
    case lists:member(AppName, Marked) of
        false ->
            case get_app_from_list(AppName, AppList) of
                not_in_list ->
		    sin_error_store:signal_error(),
                    ?SIN_RAISE(app_name_not_in_list,
			       "App ~s is not in the list of project apps. "
			       "This shouldn't happen!!!",
			       [AppName]);
                {_, _, {Deps, IncDeps}, Path} ->
                    Ebin = filename:join([Path, "ebin"]),
                    Include = {i, filename:join([Path, "include"])},
                    code:add_patha(Ebin),
                    extract_info_from_deps(BuildRef, T, AppList ++ Deps ++
					   IncDeps,
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
gather_modules(BuildRef, AppName) ->
    sin_config:get_value(BuildRef, "apps." ++ AppName ++ ".all_modules").


%% @doc Ensure that the build dir exists and is ready to accept files.
ensure_build_dir(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    AppsDir = lists:flatten([BuildDir, "apps", "tmp"]),
    filelib:ensure_dir(AppsDir).


%% @doc Actual get the failer detail information and add it to the accumulator.
gather_fail_info(File, ListOfProblems, Acc, WoE) ->
    lists:foldl(
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




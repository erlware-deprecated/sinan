%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Builds the *.script and *.boot from the project rel file.
%%% @end
%%% @copyright (C) 2006-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_release).

-behaviour(sin_task).

-include("internal.hrl").

%% API
-export([description/0, do_task/1, format_exception/1]).

-define(TASK, release).
-define(DEPS, [build]).

%%====================================================================
%% API
%%====================================================================

%% @doc describes this task to the system
-spec description() -> sin_task:task_description().
description() ->
    Desc = "Creates the *.rel, *.boot and *.script into the \n"
	"<build-area>/realeases/<vsn> directory. It also builds up a \n"
	"release tar bal into the <build-area>/tar/ directory",
    #task{name = ?TASK,
	  task_impl = ?MODULE,
	  bare = false,
	  deps = ?DEPS,
	  example = "release",
	  short_desc = "Creates an otp release for the system",
	  desc = Desc,
	  opts = []}.

%% @doc create an otp release
-spec do_task(sin_config:config()) -> sin_config:config().
do_task(BuildRef) ->
    BuildDir = sin_config:get_value(BuildRef, "build.dir"),
    {ReleaseName, Version} =
	case sin_config:get_value(BuildRef, "-r") of
	    undefined ->
		{project_name(BuildRef), project_version(BuildRef)};
	    R ->
		{R, sin_config:get_value(BuildRef, "releases." ++ R ++ ".vsn")}
	end,
    ReleaseInfo = generate_rel_file(BuildRef, BuildDir, ReleaseName, Version),
    BuildRef2 = sin_config:store(BuildRef, "project.release_info", ReleaseInfo),
    copy_or_generate_sys_config_file(BuildRef2, BuildDir, ReleaseName, Version),
    make_boot_script(BuildRef2, ReleaseInfo),
    BuildRef2.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Generate release information from info available in the project.
-spec generate_rel_file(sin_config:config(), string(), string(), string()) ->
    {ReleaseFile::string(), ReleaseInfo::term()}.
generate_rel_file(BuildRef, BuildDir, Name, Version) ->
    BuildFlavor = sin_config:get_value(BuildRef, "build.flavor"),
    {ProjectName, ProjectVsn} =
	case sin_config:get_value(BuildRef, "-r") of
	    undefined ->
		{sin_config:get_value(BuildRef,
					    "project.name"),
		 sin_config:get_value(BuildRef, "project.vsn")};
	    ReleaseName ->
		{ReleaseName, sin_config:get_value(BuildRef, "releases."++
						   ReleaseName ++".vsn")}
	end,

    RootDir = sin_config:get_value(BuildRef, "project.dir"),

    case sin_release:get_release(RootDir, BuildFlavor, ProjectName,
                                 ProjectVsn) of
        no_file ->

            Erts = get_erts_info(),

            Deps =
		process_deps(BuildRef,
			     element(1,sin_config:get_value(BuildRef,
							    "project.deps")), []),
            Deps2 =
		process_deps(BuildRef,
			     element(2,sin_config:get_value(BuildRef,
							    "project.deps")), []),

            Deps3 = lists:map(fun({App, AppVersion}) ->
                                      {App, AppVersion, load}
                              end, Deps2),

            Release = {release, {Name, Version}, {erts, Erts},
                      lists:ukeymerge(1, lists:sort(Deps), lists:sort(Deps3))};

        Release ->
            ok
    end,
    {save_release(BuildDir, Name, Version, Release),
     Release}.

%% @doc return project version or throw(no_project_version)
-spec project_version(sin_config:config()) -> Vsn::string().
project_version(BuildRef) ->
    case sin_config:get_value(BuildRef, "project.vsn") of
        undefined ->
	    ewl_talk:say("No project version defined in build config; "
			 "aborting!"),
            ?SIN_RAISE(no_project_version);
        Vsn ->
            Vsn
    end.

%% @doc return project name or throw(no_project_version) area.
-spec project_name(sin_config:config()) -> Name::string().
project_name(BuildRef) ->
    case sin_config:get_value(BuildRef, "project.name") of
        undefined ->
	    ewl_talk:say("No project name defined in build config; "
			 "aborting!"),
            ?SIN_RAISE(no_project_name);
        Nm ->
            Nm
    end.

%% @doc Process the dependencies into a format useful for the rel depends area.
-spec process_deps(sin_config:config(),
		   [AppInfo::tuple()], [AppInfo::tuple()]) ->
    [AppInfo::tuple()].
process_deps(BuildRef, [{App, Vsn, _, _} | T], Acc) ->
    NewApp = stringify(App),
    case {sin_config:get_value(BuildRef,
                                     "project.release." ++ NewApp ++ ".type"),
          sin_config:get_value(BuildRef,
                                     "project.release." ++ NewApp ++
                                     ".include_apps")} of
        {undefined, undefined} ->
            process_deps(BuildRef, T, [{App, Vsn} | Acc]);
        {Type, undefined} ->
            process_deps(BuildRef, T, [{App, Vsn, list_to_atom(Type)} | Acc]);
        {undefined, IncList} ->
            process_deps(BuildRef, T,
                         [{App, Vsn, process_inc_list(IncList, [])} | Acc]);
        {Type, IncList} ->
            process_deps(BuildRef,
                         T, [{App, Vsn, list_to_atom(Type),
                              process_inc_list(IncList, [])} | Acc])
    end;
process_deps(_BuildRef, [], Acc) ->
    Acc.

%% @doc Process the optional include list into a list of atoms.
-spec process_inc_list([string()], [atom()]) ->
    [atom()].
process_inc_list([H | T], Acc) ->
    process_inc_list(T, [list_to_atom(H) | Acc]);
process_inc_list([], Acc) ->
    Acc.

%% @doc Save the release terms to a releases file for later use by the system.
-spec save_release(sin_config:config(), string(), string(), term()) ->
    {Location::string(), RelBase::string()}.
save_release(BuildDir, Name, Version, RelInfo) ->
    Location = filename:join([BuildDir, "releases", Name ++ "-" ++ Version]),
    filelib:ensure_dir(filename:join([Location, "tmp"])),
    Relbase = filename:join([Location, Name]),
    Relf = lists:flatten([Relbase, ".rel"]),
    case file:open(Relf, write) of
        {error, _} ->
	    sin_error_store:signal_error(),
	    ewl_talk:say("Couldn't open ~s for writing. Unable to "
			 "write release information",
			 [Relf]),
            ?SIN_RAISE(unable_to_write_rel_info);
        {ok, IoDev} ->
            io:format(IoDev, "~p.", [RelInfo]),
            file:close(IoDev)
    end,
    {Location, Relbase}.

%% @doc Get the system erts version.
-spec get_erts_info() -> ErtsVersion::string().
get_erts_info() ->
    erlang:system_info(version).

%% @doc Gather up the path information and make the boot/script files.
-spec make_boot_script(sin_config:config(), {{string(), string()}, term()}) ->
    ok.
make_boot_script(BuildRef, {{Location, File}, {release, {Name, _}, _, _}}) ->
    Options = [{path, [Location | get_code_paths(BuildRef)]},
               no_module_tests, silent],
    case systools_make:make_script(Name, File, [{outdir, Location} | Options]) of
        ok ->
	    ok;
        error ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE(release_script_generation_error);
        {ok, _, []} ->
	    ok;
        {ok,Module,Warnings} ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE(release_script_generation_error,
		       "~s~n", [Module:format_warning(Warnings)]);
        {error,Module,Error} ->
	    sin_error_store:signal_error(),
            ?SIN_RAISE(release_script_generation_error,
		       "~s~n", [Module:format_error(Error)])
    end.


%% @doc copy config/sys.config or generate one to releases/VSN/sys.config
-spec copy_or_generate_sys_config_file(sin_config:config(),
				       string(), string(), string()) ->
    ok.
copy_or_generate_sys_config_file(BuildRef, BuildDir, Name, Version) ->
    RelSysConfPath = filename:join([BuildDir, "releases", Name ++ "-" ++
                                    Version, "sys.config"]),
    case sin_config:get_value(BuildRef, "config_dir") of
        undefined ->
            generate_sys_config_file(RelSysConfPath);
        ConfigDir ->
            ConfSysConfPath = filename:join([ConfigDir, "sys.config"]),
            case filelib:is_regular(ConfSysConfPath) of
                false ->
                    generate_sys_config_file(RelSysConfPath);
                true ->
                    file:copy(ConfSysConfPath, RelSysConfPath)
            end
    end.

%% @doc write a generic sys.config to the path RelSysConfPath
-spec generate_sys_config_file(string()) -> ok.
generate_sys_config_file(RelSysConfPath) ->
    {ok, Fd} = file:open(RelSysConfPath, [write]),
    io:format(Fd,
              "%% Thanks to Ulf Wiger at Ericcson for these comments:~n"
              "%%~n"
              "%% This file is identified via the erl command line option -config File.~n"
              "%% Note that File should have no extension, e.g.~n"
              "%% erl -config .../sys (if this file is called sys.config)~n"
              "%%~n"
              "%% In this file, you can redefine application environment variables.~n"
              "%% This way, you don't have to modify the .app files of e.g. OTP applications.~n"
              "[].~n", []),
    file:close(Fd).



%% @doc Generates the correct set of code paths for the system.
-spec get_code_paths(sin_config:config()) -> sin_config:config().
get_code_paths(BuildRef) ->
    ProjApps = sin_config:get_value(BuildRef, "project.apps"),
    ProjPaths = lists:merge(
                  lists:map(
                    fun({App, _, _, _}) ->
                            sin_config:get_value(BuildRef,
                                            "apps." ++ atom_to_list(App) ++
                                              ".code_paths")
                    end, ProjApps)),
    RepoPaths = lists:map(
                  fun ({_App, _Vsn, _, Path}) ->
                          filename:join([Path, "ebin"])
                  end, sin_config:get_value(BuildRef, "project.repoapps")),
    lists:merge([ProjPaths, RepoPaths]).

%% @doc Convert the value to a string if it is an atom
-spec stringify(string() | atom()) -> string().
stringify(Value) when is_list(Value) ->
    Value;
stringify(Value) when is_atom(Value) ->
    atom_to_list(Value).

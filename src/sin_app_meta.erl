%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%% Populate the application metadata.
%%% @end
%%% @copyright (C) 2011 Erlware, LLC.
%%%---------------------------------------------------------------------------
-module(sin_app_meta).


-include_lib("sinan/include/sinan.hrl").

%% API
-export([build_meta/3,
         populate_modules/1,
         write_app_file/1,
         format_exception/1]).

-define(SLASH_RE,
        {re_pattern,0,0,
         <<69,82,67,80,62,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,
           0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,5,27,47,83,0,5,27,92,
           84,0,10,0>>}).

%%====================================================================
%% API
%%====================================================================
write_app_file(#app{name=AppName,vsn=Vsn,path=Path,
                    description=Desc,registered=Reg,
                    applications=Appls,included_applications=IA,
                    mod=Mod,modules=AppModules,
                    id=Id,maxP=MaxP,maxT=MaxT,env=Env,
                    start_phases=StartPhases}) ->
    MetaDataPath = filename:join([Path, "ebin",
                                  erlang:atom_to_list(AppName) ++
                                      ".app"]),
    MetaData = {application, AppName,
                [{description, Desc},
                 {id, Id},
                 {vsn, Vsn},
                 {modules, AppModules},
                 {maxP, MaxP},
                 {maxT, MaxT},
                 {registered, Reg},
                 {included_applications, IA},
                 {applications, Appls},
                 {env, Env},
                 {mod, Mod},
                 {start_phases, StartPhases}]},
    file:write_file(MetaDataPath,
                    io_lib:format("~p.\n",
                                  [MetaData])).

populate_modules(App=#app{path=Path}) ->
    EbinDir = filename:join(Path, "ebin"),
    Modules =
        filelib:fold_files(EbinDir,
                           ".+\.beam",
                           true,
                           fun(File, Acc) ->
                                   PathName = filename:rootname(File, ".beam"),
                                   Name = string:substr(PathName,
                                                        erlang:length(EbinDir) + 2),
                                   ModuleName =
                                       erlang:list_to_atom(erlang:binary_to_list(
                                                             erlang:iolist_to_binary(
                                                               re:replace(Name,
                                                                          ?SLASH_RE,
                                                                          ".",
                                                                          [global])))),
                                   [ModuleName | Acc]
                           end,
                           []),
    App#app{modules=Modules}.

build_meta(Config, State0, AppDir) ->
    AppFile  = get_app_file(State0, AppDir),
    case file:consult(AppFile) of
        {ok, [Detail={application, _, _Details}]} ->
            build_out_app_description(Config, State0, AppDir, AppFile, Detail);
        {error, {_, Module, Desc}} ->
            Error = Module:format_error(Desc),
            ?SIN_RAISE(State0, {invalid_app_file, AppFile, Error});
        {error, Error} ->
            ?SIN_RAISE(State0,
                       {no_app_file_available, AppFile, Error})
    end.

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
                              string().
format_exception(?SIN_EXEP_UNPARSE(_, {unable_to_read_config,
                                       AppFilePath, Error})) ->
    io_lib:format("unable parse *.app file ~s:~s.",
                  [AppFilePath, file:format_error(Error)]);
format_exception(?SIN_EXEP_UNPARSE(_, {invalid_app_file, bad_name,
                                       AppFile, AppName, WrongAppName})) ->
    io_lib:format("~s file has wrong app name. Should be ~p,"
                  " but is ~p.", [AppFile, AppName, WrongAppName]);
format_exception(?SIN_EXEP_UNPARSE(_, {invalid_app_file, AppFile,
                                       _AppName, Error})) ->
    io_lib:format("Unable to parse *.app file ~s due to ~p.",
                  [AppFile, Error]);
format_exception(?SIN_EXEP_UNPARSE(_, {no_app_file_available,
                                       AppFile, _AppName, Error})) ->
    io_lib:format("Unable to access file ~s due to  ~p.", [AppFile, Error]);
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
rewrite_vsn(Config, State, AppDir, BaseDetails) ->
    RWVSN = fun({vsn, VsnSpec}) ->
                    {vsn, vcs_vsn(Config, State, VsnSpec, AppDir)};
               (El) -> El
            end,
    [RWVSN(Detail) || Detail <- BaseDetails].

build_out_app_description(Config, State,
                          AppDir, AppFile,
                          {application, AppName, Details0}) ->
    Details1 = rewrite_vsn(Config, State, AppDir, Details0),
    Applications =
        case proplists:get_value(applications, Details1) of
            undefined ->
                [];
            AppList ->
                AppList
        end,
    IncludedApps =
        case proplists:get_value(included_applications, Details1) of
            undefined ->
                [];
            IncAppList ->
                IncAppList
        end,
    DepConstraints =
        case proplists:get_value(dep_constraints, Details1) of
            undefined ->
                [];
            DepCons ->
                DepCons
        end,
    Vsn = proplists:get_value(vsn, Details1),
    #app{name=AppName,
         vsn=Vsn,
         id = proplists:get_value(id, Details1, ""),
         maxP = proplists:get_value(maxP, Details1, infinity),
         maxT = proplists:get_value(maxT, Details1, infinity),
         env = proplists:get_value(env, Details1, []),
         start_phases = proplists:get_value(start_phases,
                                            Details1, undefined),
         basedir = AppDir,
         description = proplists:get_value(description, Details1),
         registered =
             case proplists:get_value(registered, Details1) of
                 undefined ->
                     [];
                 Value ->
                     Value
             end,
         applications = Applications,
         included_applications = IncludedApps,
         mod = proplists:get_value(mod, Details1),
         dotapp=AppFile,
         dep_constraints = DepConstraints,
         deps=Applications ++ IncludedApps}.

get_app_file(State, AppDir) ->
    EbinDir = filename:join([AppDir, "ebin"]),
    SrcDir = filename:join([AppDir, "src"]),
    case sin_utils:get_file_with_ext(EbinDir, "app") of
        false ->
            case sin_utils:get_file_with_ext(SrcDir, "app.src") of
                false ->
                    %% This shouldn't occur
                    ?SIN_RAISE(State, unable_to_find_app_file);
                SrcAppFile ->
                    SrcAppFile
            end;
        AppFile ->
            AppFile
    end.

vcs_vsn(Config, State, Vcs, Dir) ->
    case vcs_vsn_cmd(Vcs) of
        {rm_v, Cmd} ->
            strip_v(get_vsn(Config, State, Cmd, Dir));
        Cmd ->
            get_vsn(Config, State, Cmd, Dir)
    end.

strip_v([$v | Vsn]) ->
    Vsn;
strip_v([$V | Vsn]) ->
    Vsn;
strip_v(Vsn) ->
    Vsn.

get_vsn(_Config, _State, {explicit_vsn, VsnString}, _Dir) ->
    VsnString;
get_vsn(Config, State, {cmd, CmdString}, Dir) ->
    vcs_vsn_invoke(Config, State, CmdString, Dir);
get_vsn(Config, State, Cmd, Dir) ->
    vcs_vsn_invoke(Config, State, Cmd, Dir).

vcs_vsn_cmd({rm_v, Vcs}) ->
    {rm_v, vcs_vsn_cmd(Vcs)};
vcs_vsn_cmd(git) ->
    %% Explicitly git-describe a committish to accommodate for projects
    %% in subdirs which don't have a GIT_DIR. In that case we will
    %% get a description of the last commit that touched the subdir.
    case os:type() of
        {win32,nt} ->
            "FOR /F \"usebackq tokens=* delims=\" %i in "
                "(`git log -n 1 \"--pretty=format:%h\" .`) do "
                "@git describe --always --tags %i";
        _ ->
            "git describe --always --tags `git log -n 1 --pretty=format:%h .`"
    end;
vcs_vsn_cmd(hg)  -> "hg identify -i";
vcs_vsn_cmd(bzr) -> "bzr revno";
vcs_vsn_cmd(svn) -> "svnversion";
vcs_vsn_cmd({cmd, _Cmd}=Custom) -> Custom;
vcs_vsn_cmd(Version) -> {explicit_vsn, Version}.

vcs_vsn_invoke(Config, State, Cmd, Dir) ->
    case sin_sh:sh(Config, Cmd, [{cd, Dir}]) of
        {ok, VsnString} ->
            string:strip(VsnString, right, $\n);
        {error, Error} ->
            ?SIN_RAISE(State, {error_executing, Cmd, Error})
    end.

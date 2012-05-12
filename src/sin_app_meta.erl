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
-export([populate/2,
         rewrite_vsn/4,
         format_exception/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc do the system preparation
-spec populate(sin_config:matcher(), sin_state:state()) -> sin_state:state().
populate(Config, State0) ->
    ProjectApps = sin_state:get_value(project_apps, State0),
    lists:foldl(fun(App, State1) ->
                        prepare_app(Config, State1, App)
                  end, State0, ProjectApps).

rewrite_vsn(Config, State, AppDir, BaseDetails) ->
    RWVSN = fun({vsn, VsnSpec}) ->
                    {vsn, vcs_vsn(Config, State, VsnSpec, AppDir)};
               (El) -> El
            end,
    [RWVSN(Detail) || Detail <- BaseDetails].

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal functions
%%====================================================================
-spec prepare_app(sin_config:config(), sin_state:state(), string()) ->
                         sin_config:config().
prepare_app(_Config, State0, #app{name=AppName, path=AppBuildDir}) ->
    BaseDetails = populate_modules(State0, AppName),
    DotApp = filename:join([AppBuildDir, "ebin",
                            erlang:atom_to_list(AppName) ++ ".app"]),

    ok = file:write_file(DotApp,
                    io_lib:format("~p.\n",
                                  [{application, AppName, BaseDetails}])),

    State0.

populate_modules(State, AppName) ->
    Details = sin_state:get_value({apps, AppName, base}, State),

    SourceModuleList = lists:map(fun({_, Module, _, _, _}) ->
                                         Module
                                 end,
                                 sin_state:get_value({apps, AppName, src_modules_detail},
                                                     State)),

    lists:reverse(
      lists:foldl(fun(Element = {vsn, _}, Acc) ->
                          [{modules, SourceModuleList}, Element | Acc];
                     (Element, Acc) ->
                          [Element | Acc]
                  end, [], lists:keydelete(modules, 1, Details))).

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

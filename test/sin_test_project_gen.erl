%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  Test the ability to generate a project programatically
%%% @end
%%% Created :  5 Sep 2011 by Eric Merritt <ericbmerritt@gmail.com>
%%%-------------------------------------------------------------------
-module(sin_test_project_gen).

-export([single_app_project/2,
         validate_single_app_project/2]).

-include_lib("eunit/include/eunit.hrl").

single_app_project(BaseDir, ProjectName) ->
    ProjectDir = filename:join([BaseDir, ProjectName]),
    Env = [{year, "2010"},
           {username, "ATestUser"},
           {email_address, "ATestUser@noreply.com"},
           {copyright_holder, "ATestUser"},
           {project_version, "0.1.0"},
           {project_name, ProjectName},
           {project_dir, ProjectDir},
           {erts_version, erlang:system_info(version)},
           {single_app_project, true},
           {wants_build_config, true}],
    sin_gen:gen(Env),
    {ProjectDir, Env}.

validate_single_app_project(ProjectDir, ProjectName) ->
    ?assertMatch(true, filelib:is_file(ProjectDir)),
    Config = filename:join([ProjectDir, "config"]),
    ?assertMatch(true, filelib:is_file(Config)),
    ?assertMatch(true, filelib:is_regular(filename:join([Config, "sys.config"]))),
    Src = filename:join([ProjectDir, "src"]),
    ?assertMatch(true, filelib:is_file(Src)),
    ?assertMatch(true,
                 filelib:is_regular(filename:join([Src,
                                                   ProjectName ++ ".app.src"]))),

    ?assertMatch(true, filelib:is_regular(filename:join([Src, ProjectName ++ "_app.erl"]))),
    ?assertMatch(true, filelib:is_regular(filename:join([Src, ProjectName ++ "_sup.erl"]))),
    ?assertMatch(true, filelib:is_file(filename:join([ProjectDir, "include"]))),
    ?assertMatch(true, filelib:is_file(filename:join([ProjectDir, "doc"]))),
    ?assertMatch(true, filelib:is_regular(filename:join([ProjectDir, "sinan.config"]))),
    EBin = filename:join([ProjectDir, "ebin"]),
    ?assertMatch(true, filelib:is_file(EBin)),
    ?assertMatch(true, filelib:is_regular(filename:join([EBin, "overview.edoc"]))),
    AppDir = filename:join([ProjectDir, "_build", ProjectName, "apps",
                            ProjectName ++ "-0.1.0"]),
    ?assertMatch(true, filelib:is_file(AppDir)),
    BConfig =  filename:join([AppDir, "config"]),
    ?assertMatch(true, filelib:is_file(BConfig)),
    ?assertMatch(true, filelib:is_regular(filename:join([BConfig, "sys.config"]))),
    BSrc = filename:join([AppDir, "src"]),
    ?assertMatch(true, filelib:is_file(BSrc)),
    ?assertMatch(true, filelib:is_regular(filename:join([BSrc, ProjectName ++ "_app.erl"]))),
    ?assertMatch(true, filelib:is_regular(filename:join([BSrc, ProjectName ++ "_sup.erl"]))),
    ?assertMatch(true, filelib:is_file(filename:join([AppDir, "include"]))),
    ?assertMatch(true, filelib:is_file(filename:join([AppDir, "doc"]))),
    BEBin = filename:join([AppDir, "ebin"]),
    ?assertMatch(true, filelib:is_file(BEBin)),
    ?assertMatch(true, filelib:is_regular(filename:join([BEBin, ProjectName ++ "_app.beam"]))),
    ?assertMatch(true, filelib:is_regular(filename:join([BEBin, ProjectName ++ "_sup.beam"]))),
    ?assertMatch(true, filelib:is_regular(filename:join([BEBin, ProjectName ++ ".app"]))),
    ok.

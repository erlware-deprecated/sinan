%% -*- mode: Erlang; fill-column: 80; comment-column: 70; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang
%%%  project
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_skel).

%%% API
-export([application/3,
         supervisor/3,
         app_info/4,
         edoc_overview/3,
         build_config/2,
         sysconfig/2]).

-export_type([env/0]).

%%============================================================================
%% Types
%%============================================================================

-type env() :: [{Key::atom(), Value::term()}].

%%====================================================================
%% API
%%====================================================================

%% @doc Writes out the application file
-spec application(env(), FileName::string(), AppName::string()) -> ok.
application(Env, FileName, AppName) ->
    Data = [header(Env),
            "-module(", AppName, "_app).\n"
            "\n"
            "-behaviour(application).\n"
            "\n"
            "%% Application callbacks\n"
            "-export([start/2, stop/1]).\n"
            "\n"
            "%%%===================================================================\n"
            "%%% Application callbacks\n"
            "%%%===================================================================\n"
            "\n"
            "%% @private\n"
            "-spec start(normal | {takeover, node()} | {failover, node()},\n"
            "            any()) -> {ok, pid()} | {ok, pid(), State::any()} |\n"
            "                      {error, Reason::any()}.\n"
            "start(_StartType, _StartArgs) ->\n"
            "    case ", AppName, "_sup:start_link() of\n"
            "        {ok, Pid} ->\n"
            "            {ok, Pid};\n"
            "        Error ->\n"
            "            Error\n"
            "    end.\n"
            "\n"
            "%% @private\n"
            "-spec stop(State::any()) -> ok.\n"
            "stop(_State) ->\n"
            "    ok.\n"
            "\n"
            "%%%===================================================================\n"
            "%%% Internal functions\n"
            "%%%===================================================================\n"],
    ok = file:write_file(FileName, Data).



%% @doc Writes out a generic supervisor to the filename provided.
-spec supervisor(env(), FileName::string(), AppName::string()) -> ok.
supervisor(Env, FileName, AppName) ->
    Data =
        [header(Env),
         "-module(", AppName, "_sup).\n"
         "-behaviour(supervisor).\n"
         "\n"
         "%% API\n"
         "-export([start_link/0]).\n"
         "\n"
         "%% Supervisor callbacks\n"
         "-export([init/1]).\n"
         "\n"
         "-define(SERVER, ?MODULE).\n"
         "\n"
         "%%%===================================================================\n"
         "%%% API functions\n"
         "%%%===================================================================\n"
         "\n"
         "-spec start_link() -> {ok, pid()} | any().\n"
         "start_link() ->\n"
         "    supervisor:start_link({local, ?SERVER}, ?MODULE, []).\n"
         "\n"
         "%%%===================================================================\n"
         "%%% Supervisor callbacks\n"
         "%%%===================================================================\n"
         "\n"
         "\n"
         "%% @private\n"
         "-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |\n"
         "                       ignore | {error, Reason::any()}.\n"
         "init([]) ->\n"
         "    RestartStrategy = one_for_one,\n"
         "    MaxRestarts = 1000,\n"
         "    MaxSecondsBetweenRestarts = 3600,\n"
         "\n"
         "    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},\n"
         "\n"
         "    Restart = permanent,\n"
         "    Shutdown = 2000,\n"
         "    Type = worker,\n"
         "\n"
         "    AChild = {'AName', {'AModule', start_link, []},\n"
         "              Restart, Shutdown, Type, ['AModule']},\n"
         "\n"
         "    {ok, {SupFlags, [AChild]}}.\n"
         "\n"
         "%%%===================================================================\n"
         "%%% Internal functions\n"
         "%%%===================================================================\n"
         "\n"
         "%%%====================================================================\n"
         "%%% tests\n"
         "%%%====================================================================\n"],
    ok = file:write_file(FileName, Data).

%% @doc Writes out a generic application to the filename provided.
-spec app_info(env(), FileName::string(), AppName::string(), AppVsn::string()) -> ok.
app_info(_Env, FileName, AppName, AppVsn) ->
    Data = ["%% This is the application resource file (.app file) for the,\n"
            "%% application.\n"
            "{application, ", AppName, ",\n"
            "  [{description, \"Your Desc HERE\"},\n"
            "   {vsn, \"", AppVsn, "\"},\n"
            "   {modules, [", AppName, "_app,\n"
            "              ", AppName, "_sup]},\n"
            "   {registered,[", AppName, "_sup]},\n"
            "   {applications, [kernel, stdlib]},\n"
            "   {mod, {", AppName, "_app,[]}},\n"
            "   {start_phases, []}]}.\n"],
    ok = file:write_file(FileName, Data).

%% @doc Writes out a overview.edoc to the filename provided.
-spec edoc_overview(env(), FileName::string(), AppName::string()) -> ok.
edoc_overview(Env, FileName, _AppName) ->
    {username, UserName} = lists:keyfind(username, 1, Env),
    {email_address, EmailAddress} = lists:keyfind(email_address, 1, Env),
    {year, Year} = lists:keyfind(year, 1, Env),
    {copyright_holder, CopyrightHolder}= lists:keyfind(copyright_holder, 1, Env),
    Data = ["@author ", UserName, "  <", EmailAddress, ">\n"
            "@copyright ", Year, " ", CopyrightHolder, "\n"
            "@version {@vsn}\n"],
    ok = file:write_file(FileName, Data).


%% @doc Writes the build_config to the specified library with the specified
%% repo.
-spec build_config(env(), FileName::string()) -> ok.
build_config(Env, FileName) ->
    {project_name, ProjectName} = lists:keyfind(project_name, 1, Env),
    {project_version, ProjectVersion} = lists:keyfind(project_version, 1, Env),
    Data = ["{project_name, ", ProjectName, "}.\n"
            "{project_vsn, \"", ProjectVersion, "\"}.\n"
            "\n"
            "{build_dir,  \"_build\"}.\n"
            "\n"
            "{ignore_dirs, [\"_\", \".\"]}.\n"
            "\n"
            "{ignore_apps, []}.\n"],
    ok = file:write_file(FileName, Data).


%% @doc Writes the sys config out to the specifiecd place
-spec sysconfig(env(), Filename::string()) -> ok.
sysconfig(Env, FileName) ->
    {project_name, ProjectName} = lists:keyfind(project_name, 1, Env),
    Data = ["%%% -*- mode:erlang -*-\n"
            "%%% Warning - this config file *must* end with <dot><whitespace>\n"
            "\n"
            "[ {", ProjectName, ", []} ].\n"],
    ok = file:write_file(FileName, Data).



%%====================================================================
%% Internal Functions
%%====================================================================
header(Env) ->
    {username, UserName} = lists:keyfind(username, 1, Env),
    {email_address, EmailAddress} = lists:keyfind(email_address, 1, Env),
    {year, Year} = lists:keyfind(year, 1, Env),
    {copyright_holder, CopyrightHolder}= lists:keyfind(copyright_holder, 1, Env),
    ["%%%----------------------------------------------------------------\n"
     "%%% @author  ", UserName, " <", EmailAddress, ">\n"
     "%%% @doc\n"
     "%%% @end\n"
     "%%% @copyright ", Year, " ", CopyrightHolder, "\n"
     "%%%----------------------------------------------------------------\n"].


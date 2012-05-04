%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang
%%%  project
%%% @end
%%% @copyright (C) 2007-2011 Erlware
%%%---------------------------------------------------------------------------
-module(sin_task_gen).

-behaviour(sin_task).

-include_lib("sinan/include/sinan.hrl").

%% API
-export([description/0,
         do_task/2]).

-define(TASK, gen).
-define(DEPS, []).

%%============================================================================
%% Types
%%============================================================================

-type env() :: [{Key::atom(), Value::term()}].

%%============================================================================
%% API
%%============================================================================

%% @doc describe this task to the system.
-spec description() -> ok.
description() ->

    Desc = "
gen Task
========

This command generates a new Erlang project. It asks the user a series of
questions about the new project that he wants to create. It then generates a
buildable skeleton for that project in the current directory. Try it out,
you can always delete it later.",

    #task{name = ?TASK,
          task_impl = ?MODULE,
          bare = true,
          deps = ?DEPS,
          example = "gen",
          short_desc = "generates a new skeleton project",
          desc = Desc,
          opts = []}.

%% @doc Do the generation of an erlang project
-spec do_task(sin_config:config(), sin_state:state()) -> sin_state:state().
do_task(Config, State) ->
    {{Year, _, _}, {_, _, _}} = erlang:localtime(),
    get_user_information(Config, [{year, integer_to_list(Year)}]),
    State.

%%============================================================================
%% Internal functions
%%============================================================================

%% @doc Queries the user for various information that we need to generate the
%% project
-spec get_user_information(sin_config:config(), env()) -> ok.
get_user_information(Config, Env) ->
    sin_log:normal(Config, "Please specify your name "),
    Name = ec_talk:ask("your name"),
    sin_log:normal(Config, "Please specify your email address "),
    Address = ec_talk:ask("your email"),
    sin_log:normal(Config, "Please specify the copyright holder "),
    CopyHolder = ec_talk:ask_default("copyright holder", Name),
    Env2 = [{username, Name}, {email_address, Address},
           {copyright_holder, CopyHolder} | Env],
    get_project_information(Config, Env2).

%% @doc Get the project name, either from the command line or from the user.
-spec get_project_name(sin_config:config()) -> string().
get_project_name(Config) ->
    case Config:match(additional_args) of
        [] ->
            sin_log:normal(Config, "Please specify name of your project"),
            ec_talk:ask("project name");
        [Value] ->
            Value
    end.

%% @doc Queries the user for the name of this project
-spec get_project_information(sin_config:config(), env()) -> ok.
get_project_information(Config, Env) ->
    {ok, CDir} = file:get_cwd(),
    Name = get_project_name(Config),
    Dir = filename:join(CDir, Name),
    sin_log:normal(Config, "Please specify version of your project"),
    Version = ec_talk:ask("project version"),
    ErtsVersion = ec_talk:ask_default("Please specify the ERTS version",
                                       erlang:system_info(version)),
    Env2 = [{project_version, Version},
            {project_name, Name},
            {project_dir, Dir},
            {erts_version, ErtsVersion} | Env],

    Env3 =
        case ec_talk:ask_default("Is this a single application project",
                                  boolean, "n") of
            false ->
                get_application_names(Config, [{single_app_project, false} | Env2]);
            true ->
                [{single_app_project, true} | Env2]
        end,
    Env4 = case ec_talk:ask_default("Would you like a build config?",
                                     boolean, "y") of
               true ->
                   [{wants_build_config, true} | Env3];
               false ->
                   [{wants_build_config, false} | Env3]
           end,
    all_done(Config, sin_gen:gen(Config, Env4)),
    Config.

%% @doc Queries the user for a list of application names. The user can choose
%% to skip this part.
-spec get_application_names(sin_config:config(), env()) -> ok.
get_application_names(Config, Env) ->
    sin_log:normal(Config, "Please specify the names of the OTP apps"
                   " that will be developed under this project. One "
                   "application to a line. Finish with a blank line."),

    get_application_names(Config, Env, ec_talk:ask("app"), []).

-spec get_application_names(sin_config:config(), env(), [string()], []) -> [Names::string()].
get_application_names(_Config, Env, no_data, Acc) ->
    [{apps, Acc} | Env];
get_application_names(Config, Env, App, Acc) ->
    NewAcc = case lists:member(App, Acc) of
                 true ->
                     sin_log:normal(Config, "App ~s is already specified", [App]),
                     Acc;
                 false ->
                     [App | Acc]
             end,
    get_application_names(Config, Env, ec_talk:ask_default("app", ""), NewAcc).


%% @doc Prints out a nice error message if everything was ok.
-spec all_done(sin_config:config(), ok) -> ok.
all_done(Config, ok) ->
    sin_log:normal(Config, "Project was created, you should be good to go!").


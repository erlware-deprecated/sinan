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

    Desc = "This command asks the user a series of questions about the new
    project that he wants to create. It then generates a buildable skeleton for
    that project in the current directory. Try it out, you can always delete it
    later.",

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
    ewl_talk:say("Please specify your name "),
    Name = ewl_talk:ask("your name"),
    ewl_talk:say("Please specify your email address "),
    Address = ewl_talk:ask("your email"),
    ewl_talk:say("Please specify the copyright holder "),
    CopyHolder = ewl_talk:ask_default("copyright holder", Name),
    Env2 = [{username, Name}, {email_address, Address},
           {copyright_holder, CopyHolder} | Env],
    get_project_information(Config, Env2).

%% @doc Get the project name, either from the command line or from the user.
-spec get_project_name(sin_config:config()) -> string().
get_project_name(Config) ->
    case Config:match(additional_args) of
        [] ->
            ewl_talk:say("Please specify name of your project"),
            ewl_talk:ask("project name");
        [Value] ->
            Value
    end.

%% @doc Queries the user for the name of this project
-spec get_project_information(sin_config:config(), env()) -> ok.
get_project_information(Config, Env) ->
    {ok, CDir} = file:get_cwd(),
    Name = get_project_name(Config),
    Dir = filename:join(CDir, Name),
    ewl_talk:say("Please specify version of your project"),
    Version = ewl_talk:ask("project version"),
    ErtsVersion = ewl_talk:ask_default("Please specify the ERTS version",
                                       erlang:system_info(version)),
    Env2 = [{project_version, Version},
            {project_name, Name},
            {project_dir, Dir},
            {erts_version, ErtsVersion} | Env],

    Env3 =
        case ewl_talk:ask_default("Is this a single application project",
                                  boolean, "n") of
            false ->
                get_application_names([{single_app_project, false} | Env2]);
            true ->
                [{single_app_project, true} | Env2]
        end,
    Env4 = case ewl_talk:ask_default("Would you like a build config?",
                                     boolean, "y") of
               true ->
                   [{wants_build_config, true} | Env3];
               false ->
                   [{wants_build_config, false} | Env3]
           end,
    all_done(sin_gen:gen(Env4)),
    Config.

%% @doc Queries the user for a list of application names. The user can choose
%% to skip this part.
-spec get_application_names(env()) -> ok.
get_application_names(Env) ->
    ewl_talk:say("Please specify the names of the OTP apps"
                 " that will be developed under this project. One "
                 "application to a line. Finish with a blank line."),

    get_application_names(Env, ewl_talk:ask("app"), []).

-spec get_application_names(env(), [string()], []) -> [Names::string()].
get_application_names(Env, no_data, Acc) ->
    [{apps, Acc} | Env];
get_application_names(Env, App, Acc) ->
    NewAcc = case lists:member(App, Acc) of
                 true ->
                     ewl_talk:say("App ~s is already specified", [App]),
                     Acc;
                 false ->
                     [App | Acc]
             end,
    get_application_names(Env, ewl_talk:ask_default("app", ""), NewAcc).


%% @doc Prints out a nice error message if everything was ok.
-spec all_done(ok) -> ok.
all_done(ok) ->
    ewl_talk:say("Project was created, you should be good to go!").


%%%-------------------------------------------------------------------
%%% @author Eric Merritt <cyberlync@gmail.com>
%%% @doc
%%%  Provides utitlities to generate an polar complient otp/erlang 
%%%  project
%%% @end
%%% @copyright 2006 Eric Merritt <cyberlync@gmail.com>
%%%-------------------------------------------------------------------
-module(pjg_gen).

%% API
-export([gen/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec gen() -> ok.
%% @doc
%%  Kicks off the generation process. Handles the individual steps
%%  in new project generation.
%% @end
%%--------------------------------------------------------------------
gen() ->
    {{Year, _, _}, {_, _, _}} = erlang:localtime(),
    get_user_information([{year, integer_to_list(Year)}]).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec all_done() -> ok.
%% @doc
%%  Prints out a nice error message if everything was ok.
%% @end
%%--------------------------------------------------------------------
all_done() ->
    io:put_chars("Project was created, you should be good to go!\n").

%%--------------------------------------------------------------------
%% @spec build_out_build_config(Env) -> ok.
%% @doc
%%  Builds the build config dir in the root of the project.
%% @end
%%-------------------------------------------------------------------- 
build_out_build_config(Env) ->
    ProjectDir = get_env(project_dir, Env),
    ConfName = filename:join([ProjectDir, "_build.cfg"]),
    pjg_skel:build_config(Env, ConfName),
    all_done().


%%--------------------------------------------------------------------
%% @spec build_out_applications(ProjDir, Apps) -> ok.
%% @doc
%%  Given the project directory and a list of application names, builds
%%  out the application directory structure.
%% @end
%%--------------------------------------------------------------------
build_out_applications(Env) ->
    Apps = get_env(apps, Env),
    build_out_applications(Env, Apps).

build_out_applications(Env, [AppName | T]) ->
    ProjDir = get_env(project_dir, Env),
    AppDir = filename:join([ProjDir, "lib", AppName]),
    case filelib:is_dir(AppDir) of
        false ->
            make_dir(AppDir),
            make_dir(filename:join(AppDir, "ebin")),
            make_dir(filename:join(AppDir, "include")),
            AppSrc = make_dir(filename:join(AppDir, "src")),
            build_out_otp(Env, AppSrc, AppName),
            build_out_applications(Env, T);
       true ->
            ok
    end;
build_out_applications(Env, []) ->
    build_out_build_config(Env).

%%--------------------------------------------------------------------
%% @spec build_out_otp(UserAddress, CopyHolder, App, AppSrc) -> ok
%% @doc
%% Build out the top level otp parts of the application.
%% @end
%%--------------------------------------------------------------------
build_out_otp(Env, AppSrc, App) ->
    FileName = filename:join(AppSrc, App ++ "_app.erl"),
    case filelib:is_file(FileName) of
        true ->
            build_out_super(Env, AppSrc, App);
        false ->
            pjg_skel:application(Env, FileName, App), 
            build_out_super(Env, AppSrc, App)
    end.


%%--------------------------------------------------------------------
%% @spec build_out_super(UserAddress, CopyHolder, App, AppSrc) -> ok.
%% @doc
%% Builds out the supervisor for the app.
%% @end
%%--------------------------------------------------------------------
build_out_super(Env, AppSrc, App) ->
    FileName = filename:join(AppSrc, App ++ "_sup.erl"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            pjg_skel:supervisor(Env, FileName, App), 
            build_out_app_src(Env, App)
    end.

%%--------------------------------------------------------------------
%% @spec build_out_app_src(App, AppSrc) -> ok.
%% @doc
%% Builds out the app descriptor for the app.
%% @end
%%--------------------------------------------------------------------
build_out_app_src(Env, App) ->
    ProjDir = get_env(project_dir, Env),
    AppEbin = filename:join([ProjDir, "lib", App, "ebin"]),
    FileName = filename:join(AppEbin, App ++ ".app"),
    case filelib:is_file(FileName) of
        true ->
            ok;
        false ->
            pjg_skel:app_info(Env, FileName, App) 
    end.

%%--------------------------------------------------------------------
%% @spec build_out_skeleton(ProjDir, Apps) -> ok.
%% @doc
%%  Given the project directory builds out the various directories 
%%  required for an application.
%% @end
%%--------------------------------------------------------------------
build_out_skeleton(Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(filename:join(ProjDir, "doc")),
    build_out_applications(Env).

build_out_project(Env) ->
    ProjDir = get_env(project_dir, Env),
    make_dir(ProjDir),
    build_out_skeleton(Env).


%%--------------------------------------------------------------------
%% @spec get_application_names() -> AppNames.
%% @doc
%%  Queries the user for a list of application names. The user 
%% can choose to skip this part.
%% @end
%%--------------------------------------------------------------------
get_application_names(Env) ->
    io:put_chars(["Please specify the names of the OTP apps", 
                  " that belong to this project. One application to a",
                  " line. Finish with a blank line.\n"]),
    get_application_names(Env, trim(io:get_line('app> ')), []).

get_application_names(Env, [], Acc) ->
    Env2 = [{apps, Acc} | Env],
    build_out_project(Env2);
get_application_names(Env, App, Acc) ->
    get_application_names(Env, trim(io:get_line('app> ')), [App | Acc]).
    
%%--------------------------------------------------------------------
%% @spec get_new_project_name(Env) -> Env2.
%% @doc
%% Queries the user for the name of this project
%% @end
%%--------------------------------------------------------------------
get_new_project_name(Env) ->
    {ok, CDir} = file:get_cwd(),
    io:put_chars(["Please specify name of your project \n"]), 
    Name = trim(io:get_line('project name> ')),
    Dir = filename:join(CDir, Name),
    io:put_chars(["Please specify version of your project \n"]), 
    Version = trim(io:get_line('project version> ')),
    Env2 = [{project_version, Version}, 
            {project_name, Name}, 
            {project_dir, Dir} | Env],
    get_application_names(Env2).
    


%%--------------------------------------------------------------------
%% @spec get_user_information() -> Env.
%% @doc
%% Queries the user for his name and email address
%% @end
%%--------------------------------------------------------------------
get_user_information(Env) ->
    io:put_chars("Please specify your name \n"), 
    Name = trim(io:get_line('your name> ')),
    io:put_chars("Please specify your email address \n"),
    Address = trim(io:get_line('your email> ')),
    io:put_chars("Please specify the copyright holder \n"),
    CopyHolder = trim(io:get_line('copyright holder> ')),
    io:put_chars("Where is the remote code repository? \n"),
    Repositories = get_repositories(),
    Env2 = [{username, Name}, {email_address, Address},
           {copyright_holder, CopyHolder},
           {repositories, Repositories} | Env],
    get_new_project_name(Env2).


get_repositories() ->
    io:put_chars(["Please specify the locations of the repositories. "
                  " One repository to a line. Finish with a blank line.\n"]),
    get_repositories(trim(io:get_line('repo> ')), []).

get_repositories([], Acc) ->
    Acc;
get_repositories(Repo, []) ->
    get_repositories(trim(io:get_line('repo> ')), [$\", Repo, $\"] );
get_repositories(Repo, Acc) ->
    get_repositories(trim(io:get_line('repo> ')), Acc ++ [$,, $\", Repo, $\"]).

%%--------------------------------------------------------------------
%% @spec trim(String::string()) -> NewString::string().
%% @doc
%% Helper function that removes whitespace from both sides of the 
%% string.
%% @end
%%--------------------------------------------------------------------
trim(String) ->
    lists:reverse(strip(lists:reverse(strip(String)))).

%%--------------------------------------------------------------------
%% @spec strip(String::string()) -> NewString::string().
%% @doc
%% Helper function that removes whitespace From the front of a string.
%% @end
%%--------------------------------------------------------------------
strip([$   | Cs]) -> strip(Cs);
strip([$\t | Cs]) -> strip(Cs);
strip([$\r | Cs]) -> strip(Cs);
strip([$\n | Cs]) -> strip(Cs);
strip(Cs) -> Cs.

%%--------------------------------------------------------------------
%% @spec make_dir(DirName) -> ok.
%% @doc
%% Helper function that makes the specified directory and all parent
%% directories.
%% @end
%%--------------------------------------------------------------------
make_dir(DirName) ->
    filelib:ensure_dir(DirName),
    is_made(DirName, file:make_dir(DirName)),
    DirName.

%%--------------------------------------------------------------------
%% @spec is_made(DirName, Output) -> ok.
%% @doc
%% Helper function that makes sure a directory is made by testing 
%% the output of file:make_dir().
%% @end
%%--------------------------------------------------------------------
is_made(DirName, {error, eexists})->
    io:put_chars([DirName, " exists ok.\n"]);
is_made(DirName, ok) ->
    io:put_chars([DirName, " created ok.\n"]).

%%--------------------------------------------------------------------
%% @spec get_env(Name, Env) -> Value.
%% 
%% @doc 
%%  Get the value from the environment.
%% @end
%%--------------------------------------------------------------------
get_env(Name, Env) ->
    {value, {Name, Value}} = lists:keysearch(Name, 1, Env),
    Value.

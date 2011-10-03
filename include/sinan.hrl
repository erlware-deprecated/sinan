%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Provides the task  and resource definitions for the system.
%%% @end
%%% @copyright 2007 Erlware
%%%---------------------------------------------------------------------------

%% @doc Describes n application, and whether or not the application is a
%% runtime dep or compile time dep, also wether it is a project
%% application or a dependency
-record(app, {name :: atom(),
              vsn :: string(),
              path :: string(),
              type :: runtime | compiletime,
              project :: boolean(),
              modules :: sin_file_info:mod(),
              sources :: sin_file_info:mod()}).

%% @doc describes a module in the system
%%  - name: is the name of the module
-record(module, {name :: module(),
                 type :: hrl | yrl | erl | {other, string()},
                 path :: string(),
                 module_deps :: [module()],
                 includes :: [atom()],
                 include_timestamps :: [{string(), sin_file_info:date_time()}],
                 tags :: [atom()],
                 called_modules :: [atom()],
                 changed :: sin_file_info:date_time(),
                 change_sig :: number()}).


-record(task,  {name :: atom(),            % The 'user friendly' name of the task
                task_impl :: atom(),       % The implementation of the task, maybe fun or
                bare :: boolean(),         % Indicates whether a build config is needed
                deps :: [atom()],          % The list of dependencies
                desc :: string(),          % The description for the task
                short_desc :: string(),    % A one line short description of the task
                example :: string(),       % An example of the task usage
                opts :: list()}).          % The list of options that the task requires/understands



-define(SIN_RAISE(State, Problem),
        throw({pe,
               sin_state:add_run_error(?MODULE,
                                       {?MODULE, ?LINE, Problem}, State),
               {?MODULE, ?LINE, Problem}})).

-define(SIN_RAISE(State, Problem, Description),
        throw({pe,
               sin_state:add_run_error(?MODULE,
                                       {?MODULE, ?LINE,
                                        {Problem, Description}}, State),
                                        {?MODULE, ?LINE,
                                         {Problem, Description}}})).

-define(SIN_RAISE(State, Problem, Description, DescArgs),
        throw({pe,
               sin_state:add_run_error(?MODULE,
                                       {?MODULE, ?LINE,
                                        {Problem,
                                         io_lib:format(Description,
                                                       DescArgs)}}, State),
               {?MODULE, ?LINE,
                {Problem, io_lib:format(Description, DescArgs)}}})).

-define(WARN(State, Warnings),
        ((fun() ->
                  WarnRef =
                      sin_state:add_run_warning(?MODULE, Warnings, State),
                  ewl_talk:say(Warnings),
                  WarnRef
          end)())).


-define(WARN(State, Warnings, Detail),
        ((fun() ->
                WarnRef =
                     sin_state:add_run_warning(?MODULE,
                                                io_lib:format(Warnings, Detail), State),
                 ewl_talk:say(Warnings, Detail),
                 WarnRef
          end)())).

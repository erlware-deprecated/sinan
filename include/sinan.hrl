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



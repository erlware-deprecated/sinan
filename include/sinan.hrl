%% Describes an application, and whether or not the application is a
%% runtime dep or compile time dep, also wether it is a project
%% application or a dependency
-record(app, {name, vsn, path, type, project}).


-module(sint_cuke_support_funs).

-export([app_src/1, delete_if_exists/1]).

app_src(Name) ->
    ["%% This is the application resource file (.app file) for the app2,\n"
     "%% application.\n"
     "{application, ", Name, ",\n"
     "[{description, \"Your Desc HERE\"},\n"
     "  {vsn, \"0.1.0\"},\n"
     "  {modules, []},\n"
     "  {registered,[", Name, "_sup]},\n"
     "  {applications, [kernel, stdlib]},\n"
     "  {mod, {", Name, "_app,[]}}, \n"
     "  {start_phases, []}]}.\n"].

delete_if_exists(Path) ->
    case file:delete(Path) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        Error ->
            throw(Error)
    end.

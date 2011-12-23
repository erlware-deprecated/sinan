%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @copyright Erlware, LLC
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Parse an erlang file (yrl, hrl, erl) and information about the file
%%% that is relevant to the build system
%%% @end
%%%-------------------------------------------------------------------
-module(sin_file_info).

%% API
-export([process_file/3, format_exception/1]).
-export_type([date/0, time/0, date_time/0, mod/0]).

-include_lib("sinan/include/sinan.hrl").
-include_lib("kernel/include/file.hrl").


%%====================================================================
%% Types
%%====================================================================
-type date() :: {Year :: non_neg_integer(),
                 Month :: non_neg_integer(),
                 Day :: non_neg_integer()}.

-type time() :: {Hour :: non_neg_integer(),
                 Minute :: non_neg_integer(),
                 Second :: non_neg_integer()}.

-type date_time() :: {date() , time()}.
-type mod() :: record(module).
-type type() :: hrl | erl | yrl | {other, string()}.

%%====================================================================
%% API
%%====================================================================
-spec process_file(sin_state:state(), string(), [string()]) ->
                          sinan:mod().
process_file(State0, Path0, Includes) ->
    sin_sig:do_if_changed(?MODULE,
                          Path0,
                          fun deps_changed/2,
                          fun(Path1, State1) ->
                                  do_extract(State1, Path1,  Includes)
                          end, State0).

%% @doc Format an exception thrown by this module
-spec format_exception(sin_exceptions:exception()) ->
    string().
format_exception(?SIN_EXEP_UNPARSE(_, {unable_to_include, Include, Name})) ->
    io_lib:format("Unable to find include \"~s\" when processing module: ~p",
                  [Include, Name]);
format_exception(?SIN_EXEP_UNPARSE(_, {unable_to_process, Name,
                                       {_, Module, Error}})) ->
        io_lib:format("Unable to find process ~p due to the following"
                      "error: ~s",
                      [Name, lists:flatten(Module:format_error(Error))]);
format_exception(?SIN_EXEP_UNPARSE(_, {unable_to_parse_file, Path,
                                       {_, Module, Error}})) ->
        io_lib:format("Unable to find process ~s due to the following"
                      "error: ~s",
                      [Path, lists:flatten(Module:format_error(Error))]);
format_exception(Exception) ->
    sin_exceptions:format_exception(Exception).

%%====================================================================
%% Internal Functions
%%====================================================================
-spec deps_changed(mod(), sin_state:state()) -> boolean().
deps_changed(#module{include_timestamps=Includes}, State) ->
    lists:any(fun({Path, Stamp}) ->
                      not Stamp == get_timestamp_info(State, Path)
              end, Includes).

-spec do_extract(sin_state:state(), string(), [string()]) ->
                        {sin_state:state(), mod()}.
do_extract(State, Path, Includes) ->
    case epp:parse_file(Path, Includes, []) of
        {ok, AST} ->
            Mod0 =
                case filename:extension(Path) of
                    ".erl" ->
                        parse_form(State, AST, initialize(Path, erl));
                    ".hrl" ->
                        parse_form(State, AST, initialize(Path, hrl));
                    ".yrl" ->
                        parse_form(State, AST, initialize(Path, yrl));
                    Other ->
                        parse_form(State, AST, initialize(Path, {other, Other}))
                end,
            Mod1 = add_stamp_info(State, Path, Mod0, AST),
            {State, remove_self(Path, Mod1)};
        Error ->
            ?SIN_RAISE(State, {unable_to_parse_file, Path, Error})
    end.

-spec initialize(string(), type()) -> mod().
initialize(Path, Type) ->
    #module{type=Type,
            path=Path,
            module_deps=sets:new(),
            includes=sets:new(),
            tags=sets:new(),
            include_timestamps=[],
            called_modules=sets:new()}.

-spec remove_self(string(), mod()) -> mod().
remove_self(Path, Mod1=#module{includes=Includes}) ->
     Mod1#module{includes=sets:del_element(Path, Includes)}.

-spec add_stamp_info(sin_state:state(), string(), mod(), term()) -> mod().
add_stamp_info(State, Path, Mod, AST) ->
    Mod#module{changed=get_timestamp_info(State, Path),
               change_sig=erlang:phash2(AST)}.

-spec get_timestamp_info(sin_state:state(), string()) -> non_neg_integer().
get_timestamp_info(State, Path) ->
    case file:read_file_info(Path) of
        {ok, FileInfo} ->
            FileInfo#file_info.mtime;
        {error, enoent} ->
            0;
        {error, Reason} ->
            ?SIN_RAISE(State, {unable_to_get_file_info, Path, Reason})
    end.

-spec parse_form(sin_state:state(), term(), mod()) -> mod().
parse_form(State, Tuple, Mod) when is_tuple(Tuple) ->
    parse_tuple(State, Tuple, Mod);
parse_form(State, List, Mod0) when is_list(List) ->
    lists:foldl(fun(Element, Mod1) ->
                        parse_form(State, Element, Mod1)
                end, Mod0, List);
parse_form(_, _, Mod0) ->
    Mod0.

-spec parse_tuple(sin_state:state(), term(), mod()) -> mod().
parse_tuple(_State, {attribute, _, module, Name}, Mod) ->
    Mod#module{name=Name};
parse_tuple(State, {attribute, _ , file, {Include, _}},
           Mod = #module{includes=Includes, include_timestamps=ITS}) ->
    Mod#module{includes=sets:add_element(Include, Includes),
              include_timestamps=[{Include, get_timestamp_info(State, Include)} | ITS]};
parse_tuple(_State, {attribute, _, compile, {parse_transform, proper_transformer}},
           Mod = #module{tags=Tags}) ->
    Mod#module{tags=sets:add_element(proper, Tags)};
parse_tuple(_State, {attribute, _, compile, {parse_transform, eunit_autoexport}},
           Mod = #module{tags=Tags}) ->
    Mod#module{tags=sets:add_element(eunit, Tags)};
parse_tuple(_State, {attribute, _, compile, {parse_transform, Module}},
           Mod = #module{module_deps=Modules}) ->
    Mod#module{module_deps=sets:add_element(Module, Modules)};
parse_tuple(_State, {attribute, _, behaviour, Module},
           Mod = #module{module_deps=Modules}) ->
    Mod#module{module_deps=sets:add_element(Module, Modules)};
parse_tuple(State, {error,{_,epp,{include,lib,Include}}}, #module{name=Name}) ->
    ?SIN_RAISE(State, {unable_to_include, Include, Name});
parse_tuple(State, {error,{_,epp,{include,file,Include}}}, #module{name=Name}) ->
    ?SIN_RAISE(State, {unable_to_include, Include, Name});
parse_tuple(_, _, Mod) ->
    Mod.

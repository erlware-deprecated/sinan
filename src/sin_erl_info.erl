%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%%---------------------------------------------------------------------------
%%% @copyright Erlware, LLC
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Parse an erlang file (yrl, hrl, erl) and information about the file
%%% that is relevant to the build system
%%% @end
%%%-------------------------------------------------------------------
-module(sin_erl_info).

%% API
-export([process_file/3, format_exception/1]).

-include_lib("sinan/include/sinan.hrl").
-include_lib("kernel/include/file.hrl").


%%====================================================================
%% Types
%%====================================================================
-type type() :: hrl | erl | yrl | {other, string()}.

%%====================================================================
%% API
%%====================================================================
-spec process_file(sin_state:state(), string(), [string()]) ->
                          sinan:mod().
process_file(State0, Path0, Includes) ->
    case epp:parse_file(Path0, Includes, []) of
        {ok, AST} ->
            Mod0 =
                case filename:extension(Path0) of
                    ".erl" ->
                        parse_form(State0, AST, initialize(Path0, erl));
                    ".hrl" ->
                        parse_form(State0, AST, initialize(Path0, hrl));
                    ".yrl" ->
                        parse_form(State0, AST, initialize(Path0, yrl));
                    Other ->
                        parse_form(State0, AST, initialize(Path0, {other, Other}))
                end,
            {State0, remove_self(Path0, Mod0), erlang:phash2(AST)};
        Error ->
            ?SIN_RAISE(State0, {unable_to_parse_file, Path0, Error})
    end.


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

-spec initialize(string(), type()) ->
                        sin_file_info:mod().
initialize(Path, Type) ->
    #module{type=Type,
            path=Path,
            module_deps=sets:new(),
            includes=sets:new(),
            tags=sets:new(),
            include_timestamps=[],
            called_modules=sets:new()}.

-spec remove_self(string(),  sin_file_info:mod()) ->
                         sin_file_info:mod().
remove_self(Path, Mod1=#module{includes=Includes}) ->
     Mod1#module{includes=sets:del_element(Path, Includes)}.

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

-spec parse_form(sin_state:state(), term(), sin_file_info:mod()) ->
                         sin_file_info:mod().
parse_form(State, Tuple, Mod) when is_tuple(Tuple) ->
    parse_tuple(State, Tuple, Mod);
parse_form(State, List, Mod0) when is_list(List) ->
    lists:foldl(fun(Element, Mod1) ->
                        parse_form(State, Element, Mod1)
                end, Mod0, List);
parse_form(_, _, Mod0) ->
    Mod0.

-spec parse_tuple(sin_state:state(), term(), sin_file_info:mod()) ->
                         sin_file_info:mod().
parse_tuple(_State, {attribute, _, module, Name}, Mod) ->
    Mod#module{name=Name};
parse_tuple(State, {attribute, _ , file, {Include, _}},
           Mod = #module{includes=Includes, include_timestamps=ITS}) ->
    Mod#module{includes=sets:add_element(Include, Includes),
              include_timestamps=[{Include, get_timestamp_info(State, Include)} | ITS]};
parse_tuple(_State, {attribute, _, compile, {parse_transform, proper_transformer}},
           Mod = #module{tags=Tags}) ->
    Mod#module{tags=sets:add_element(proper, Tags)};
parse_tuple(_State, {attribute, _, compile, {parse_transform, eqc_warn}},
           Mod = #module{tags=Tags}) ->
    Mod#module{tags=sets:add_element(eqc, Tags)};
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

-module(sint_yrl_support).

-include_lib("eunit/include/eunit.hrl").

yrl_test() ->
    {ok, {ProjectDir, ProjectName}} = sint_test_project_gen:a_generated_project(),

    write_yrl(ProjectDir),

    Ret = sinan:run_sinan(["-s", ProjectDir, "build"]),
    {ok, BuildState} = Ret,
    ?assertMatch([], sin_state:get_run_errors(BuildState)),
    BinFile = filename:join([ProjectDir, "_build", ProjectName, "lib", ProjectName ++ "-0.1.0",
                             "ebin", "test.beam"]),
    ?assertMatch(true, sin_utils:file_exists(sin_state:new(), BinFile)).


write_yrl(ProjectDir) ->
    ?assertMatch(ok,
                 ec_file:mkdir_path(filename:join([ProjectDir, "src"]))),
    ImplPath = filename:join([ProjectDir, "src", "test.yrl"]),
    file:write_file(ImplPath, get_yrl_contents()),
    ImplPath.

get_yrl_contents() ->
    "
Nonterminals
predicates predicate list element elements.

Terminals '(' ')' ','
atom var integer string set union intersection comparator.

Rootsymbol predicates.

predicates -> predicate : '$1'.
predicates -> predicate union predicate : {union, '$1', '$3'}.
predicates -> predicates union predicate : {union, '$1', '$3'}.

predicates -> predicate intersection predicate : \
              {intersection, '$1', '$3'}.

predicate -> var set list : \
            {predicate, {var, unwrap('$1')}, memberof, '$3'}.

predicate -> var comparator element : \
            {predicate, {var, unwrap('$1')}, unwrap('$2'), '$3'}.

list -> '(' ')' : nil.
list -> '(' elements ')' : {list,'$2'}.

elements -> element : ['$1'].
elements -> element ',' elements : ['$1'] ++ '$3'.
element -> atom : '$1'.
element -> var : unwrap('$1').
element -> integer : unwrap('$1').
element -> string : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.
".

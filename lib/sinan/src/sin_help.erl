%%%-------------------------------------------------------------------
%%% Copyright (c) 2006, 2007 Eric Merritt
%%%
%%% Permission is hereby granted, free of charge, to any 
%%% person obtaining a copy of this software and associated 
%%% documentation files (the "Software"), to deal in the 
%%% Software without restriction, including without limitation 
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit 
%%% persons to whom the Software is furnished to do so, subject to 
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall 
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @doc
%%%   Describes the extant tasks.
%%% @end
%%% @copyright 2007 Eric Merritt
%%%---------------------------------------------------------------------------
-module(sin_help).


%% API
-export([help/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Run the help command.
%% @spec help(BuildRef) -> ok
%% @end
%%--------------------------------------------------------------------
help(BuildRef, _) ->
    ewl_talk:say("Describing tasks ..."),
    case fconf:get_value(BuildRef, "tasks") of
        undefined ->
            ewl_talk:say("No tasks to describe.");
        Tasks ->
            dict:map(fun process_task_entry/2, Tasks)
    end.


%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc 
%%  Prints out the task description.
%%
%% @spec process_task_entry(Key, Value) -> ok. 
%% @end
%%--------------------------------------------------------------------
process_task_entry(Key, Value) ->
    Description = 
        try dict:fetch("description", Value) of
            Desc ->
                lists:flatten(Desc)
        catch
            error:function_clause ->
                []
        end,
    Dependencies = 
        try dict:fetch("depends", Value) of
            DepList ->
                DepList
        catch
            error:function_clause ->
                []
        end,
    ewl_talk:say("~s", [Key]),
    ewl_talk:say("   ~s", [Description]),
    ewl_talk:say(" depends on: ~s", [print_dependencies(Dependencies, [])]),
    ewl_talk:say("~n").


%%--------------------------------------------------------------------
%% @doc 
%%  Print the dependency list.
%% @spec print_dependencies(DepList, Acc) -> ok
%% @end
%%--------------------------------------------------------------------
print_dependencies([H | T], []) ->
    print_dependencies(T, [H]);
print_dependencies([H | T], Acc) ->
    print_dependencies(T, [", ", H | Acc]);
print_dependencies([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

%%%-------------------------------------------------------------------
%%% Copyright (c) 2007 Erlware
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
%%%   Provides the task definition for the system.
%%% @end
%%% @copyright 2007 Erlware
%%%---------------------------------------------------------------------------

-define(SIN_RAISE(Problem), throw({sin_excep, Problem})).

-define(SIN_RAISE_D(Problem, Description),
        throw({sin_excep, Problem, Description})).

-define(SIN_RAISE_DA(Problem, Description, DescArgs),
        throw({sin_excep, Problem, {Description, DescArgs}})).


-record(task,  {name :: atom(),            % The 'user friendly' name of the task
                task_impl :: atom(),       % The implementation of the task, maybe fun or
		bare :: boolean(),         % Indicates whether a build config is needed
                deps :: [atom()],          % The list of dependencies
                desc :: string(),          % The description for the task
		short_desc :: string(),    % A one line short description of the task
		example :: string(),       % An example of the task usage
                opts :: list()}).          % The list of options that the task requires/understands

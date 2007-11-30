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

-define(ETA_RAISE(Problem), throw({eta_excep, Problem})).

-define(ETA_RAISE_D(Problem, Description),
        throw({eta_excep, Problem, Description})).

-define(ETA_RAISE_DA(Problem, Description, DescArgs),
        throw({eta_excep, Problem, {Description, DescArgs}})).


-record(task,  {name,            % The 'user friendly' name of the task
                task_impl,       % The implementation of the task, maybe fun or
                deps,            % The list of dependencies
                desc,            % The description for the task
                callable,        % Whether or not the task is callable may be true|false
                opts}).          % The list of options that the task requires/understands

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

-define(SIN_RAISE(Config, Problem),
        throw({pe,
               sin_config:add_run_error(Config,
                                        ?MODULE,
                                        {?MODULE, ?LINE, Problem}),
               {?MODULE, ?LINE, Problem}})).

-define(SIN_RAISE(Config, Problem, Description),
        throw({pe,
               sin_config:add_run_error(Config,
                                        ?MODULE,
                                        {?MODULE, ?LINE,
                                         {Problem, Description}}),
                                        {?MODULE, ?LINE,
                                         {Problem, Description}}})).

-define(SIN_RAISE(Config, Problem, Description, DescArgs),
        throw({pe,
               sin_config:add_run_error(Config, ?MODULE,
                                        {?MODULE, ?LINE,
                                         {Problem,
                                          io_lib:format(Description,
                                                        DescArgs)}}),
               {?MODULE, ?LINE,
                {Problem, io_lib:format(Description, DescArgs)}}})).

-define(WARN(Config, Warnings),
        ((fun() ->
                  WarnRef =
                      sin_config:add_run_warning(Config, ?MODULE, Warnings),
                  ewl_talk:say(Warnings),
                  WarnRef
          end)())).


-define(WARN(Config, Warnings, Detail),
        ((fun() ->
                WarnRef =
                     sin_config:add_run_warning(Config, ?MODULE,
                                                io_lib:format(Warnings, Detail)),
                 ewl_talk:say(Warnings, Detail),
                 WarnRef
          end)())).




-record(task,  {name :: atom(),            % The 'user friendly' name of the task
                task_impl :: atom(),       % The implementation of the task, maybe fun or
                bare :: boolean(),         % Indicates whether a build config is needed
                deps :: [atom()],          % The list of dependencies
                desc :: string(),          % The description for the task
                short_desc :: string(),    % A one line short description of the task
                example :: string(),       % An example of the task usage
                opts :: list()}).          % The list of options that the task requires/understands


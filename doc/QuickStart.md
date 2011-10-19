---
layout: default
title: Getting Started Guide
---

What is Sinan
-------------

Sinan is a build tool designed to build
[Sinan Projects](SinanProjects.html) and
[OTP Applications](OTPApplications.html), Releases and
Applications. Sinan leverages the metadata artifacts provided by [OTP](OTP.html)
to do a good job building, testing, releasing, etc with very little or
no additional input from the developer.

Quick start
-----------

Lets start by generate a project with a single application. Sinan has
the gen task and this allows you to generate a buildable skeleton for
your new project. It wont do much, but it will give you an idea of how
sinan works.

Lets call our new project foo. On the command line type the following
code.

### Generating the project

    $> sinan gen foo

This will take you through a series of quetions about yourself and the
project. I have provided a series of answers here, that illustrate how
to answer these questions.

#### Answering the generated questions
    starting: gen
    Please specify your name
    your name> Eric Merritt
    Please specify your email address
    your email> ericbmerritt@gmail.com
    Please specify the copyright holder
    copyright holder ("Eric Merritt")> Erlware, LLC
    Please specify version of your project
    project version> 0.11.0
    Please specify the ERTS version ("5.8.2")>
    Is this a single application project ("n")> y

    /Users/emerritt/tmp/foo/doc created ok.
    /Users/emerritt/tmp/foo/bin created ok.
    /Users/emerritt/tmp/foo/config created ok.
    /Users/emerritt/tmp/foo/ebin created ok.
    /Users/emerritt/tmp/foo/src created ok.
    /Users/emerritt/tmp/foo/include created ok.
    /Users/emerritt/tmp/foo/doc exists ok.

    Would you like a build config? ("y")> n
    Project was created, you should be good to go!

In this example we are going to generate a project with only one [OTP](OTP.html)
application. The gen system needs to know a bit of information about
you, your name, who is going to hold the copyright, the project
version etc. For now, leave the [ERTS](ERTS.html) version as is, you can do that by
just hitting enter after the promyt. This is a very simple project so
we don't need a build config either. Just enter 'n' after that
request.

### Building our new project

Now we have a project. Take a look and poke around. It should look
something like this

    foo
    |-- bin
    |-- config
    |-- doc
    |-- ebin
    |    `-- overview.edoc
    |-- include
    `-- src
         |-- foo.app.src
         |-- foo_app.erl
         `-- foo_sup.erl

As your project grows you will have more source files, documentation
etc. Thats to be expected. but lets get onto the more fun things. Lets
build the project.

### Build with sinan

     $> cd <my project foo directory>
     $> sinan build

cd into the top level of the project and run sinan build. This first
time its important that you do it from the top level.

### The built project

    foo
     |-- _build
     |   `-- foo
     |       |-- apps
     |       |   `-- foo-0.11.0
     |       |       |-- bin
     |       |       |-- config
     |       |       |-- doc
     |       |       |-- ebin
     |       |       |   |-- foo.app
     |       |       |   |-- foo_app.beam
     |       |       |   |-- foo_sup.beam
     |       |       |   `-- overview.edoc
     |       |       |-- include
     |       |       `-- src
     |       |           |-- foo_app.erl
     |       |           |-- foo.app.src
     |       |           `-- foo_sup.erl
     |-- bin
     |-- config
     |-- doc
     |-- ebin
     |   `-- overview.edoc
     |-- include
     `-- src
         |-- foo.app.src
         |-- foo_app.erl
         `-- foo_sup.erl

One thing to notice here immediately is that there is a new directory
called _build. Sinan never, ever touches your the files in
your root directory. It creates a version of the directory under build
and does all of its changes there.

The directory of interest to us right now is the app directory. If you
look there you will see that we have a directory there in the built to
conform to Erlang's expectations for
[OTP Applications](OTPApplications.html). That is the version of our
app, with the version number postpended. We have all of our source
built into ebin and the source in src
([OTP Applications](OTPApplications) are almost always distributed
with source).

Thats the build comand, but sinan can do a much more. Lets explore
some of the other commands.

### Testing the project

Sinan has the ability to run eunit, proper and cucumber tests in any
project Lets add a test to the application behavior (foo_app.erl) file
and run our new tests. Currently the app file looks as follows.

    {% highlight erlang %}
    %%%----------------------------------------------------------------
    %%% @author Eric Merritt <ericbmerritt@gmail.com>
    %%% @doc
    %%%
    %%% @end
    %%% @copyright 2011 Erlware, LLC
    %%%----------------------------------------------------------------,
    -module(foo_app).

    -behaviour(application).

    %% Application callbacks
    -export([start/2, stop/1]).

    %%%===================================================================
    %%% Application callbacks
    %%%===================================================================

    %% @private
    -spec start(normal | {takeover, node()} | {failover, node()},
                any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                          {error, Reason::any()}.
    start(_StartType, _StartArgs) ->
        case foo_sup:start_link() of
            {ok, Pid} ->
                {ok, Pid};
            Error ->
                Error
        end.

    %% @private
    -spec stop(State::any()) -> ok.
    stop(_State) ->
        ok.

    %%%===================================================================
    %%% Internal functions
    %%%===================================================================
    {% endhighlight %}

For now we are just going to add a test section and a single test that
doesn't do much that is very interesting.

First we need to import the eunit header (don't worry sinan makes sure
its avaibale). Then we can add a test section right below the internal
functions section.

    {% highlight erlang %}
     %%%----------------------------------------------------------------
     %%% @author Eric Merritt <ericbmerritt@gmail.com>
     %%% @doc
     %%%
     %%% @end
     %%% @copyright 2011 Erlware, LLC
     %%%----------------------------------------------------------------,
     -module(foo_app).

     -behaviour(application).

     %% Application callbacks
     -export([start/2, stop/1]).

     %%%===================================================================
     %%% Application callbacks
     %%%===================================================================

     %% @private
     -spec start(normal | {takeover, node()} | {failover, node()},
                 any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                           {error, Reason::any()}.
     start(_StartType, _StartArgs) ->
         case foo_sup:start_link() of
             {ok, Pid} ->
                 {ok, Pid};
             Error ->
                 Error
         end.

     %% @private
     -spec stop(State::any()) -> ok.
     stop(_State) ->
         ok.

     %%%===================================================================
     %%% Internal functions
     %%%===================================================================

     %%%===================================================================
     %%% Tests
     %%%===================================================================
     -ifndef(NOTEST).
     -include_lib("eunit/include/eunit.hrl").

     something_test() ->
        ?assertMatch(foo, foo).

     -endif.
    {% endhighlight %}
Notice the include_lib belowe the export and the new function
something_test in the tests section. Now that we have built once you
can be anywhere under the build dir and sinan will know how to find
everything.

    $> sinan test
    starting: build
    Building /Users/emerritt/tmp/foo/src/foo_app.erl
    starting: test
    Testing foo
    foo_app:  Test passed.
    foo_sup:  There were no tests to run.

We can see that we had a test in foo_app and that our test passed with
no problems. Sinan has added somethings to the _build area to give us
some more information about things like test
results.

### Creating an [OTP](OTP.html) release

Creating a release for [OTP](OTP.html) manually can be a pain in the butt. Sinan,
however, makes it trivial. We just need to run the sinan rel task to
get all of the rel, build, and script artifacts.

Lets do that now

     $> sinan release
     starting: depends
     starting: build
     starting: release

There isn't much to look at in the output of the release task, but
there are some new interesting things in the +_build+ area. Lets take
a look at that.

     |-- _build
     |   `-- foo
     |       |-- releases
     |       |   `-- foo-0.11.0
     |       |       |-- foo.boot
     |       |       |-- foo.rel
     |       |       |-- foo.script
     |       |       `-- sys.config

Once again everything that was there remains there, I have just cut
down the example to new items. Sinan has generated all of the release
artifacts for your new project by looking in your [OTP](OTP.html) lib dir. In fact
it has also generated the release files as well.

The *.rel file is by far the most interesting. Lets take a look at
what sinan generated.

     {release,{"foo","0.11.0"},
             {erts,"5.8.2"},
             [{foo,"0.11.0"},
              {kernel,"2.14.2"},
              {stdlib,"1.17.2"}]}.

This is a good, fully expanded release file with all the current
dependencies, and with those dependencies resolved.

Finially, we want to distribute this wonderful project to other
folks. Of course, [OTP](OTP.html) provides the framework, but sinan knows how to
do the work for you. Lets look at that.

#### Creating an [OTP](OTP.html) distribution

The dist task is much like the release task. It doesn't have much
output but it does create artifacts in +_build+ that we care
about. Lets run dist now and see what happens.

    $> sinan dist
    starting: depends
    starting: build
    starting: release
    starting: dist

As you can see, not much output, but the real interesting stuff is in
the _build directory.

     |-- _build
     |   `-- foo
     |       `-- tar
     |           `-- foo-0.11.0.tar.gz

You can see that we have a tarball now that contains the a fully
deployable distribution of the project.

Lets look at the distribution tarball. I have left off the application
dir contents in the interests of berevity.

        `-- foo-0.11.0
        |-- bin
        |-- lib
        |   |-- foo-0.11.0
        |   |-- kernel-2.14.2
        |   `-- stdlib-1.17.2
        `-- releases
            |-- foo.boot
            |-- foo.rel
            `-- foo.script

This is a normal [OTP](OTP.html) distribution tarball with all of the dependencies included.

Sinan has a few more commands and things can get much more complex if
you need them to be. If you need further help you can always run the
sinan help command.

     $> sinan
    starting: help
    Usage:  [-v <verbose>] [-s <start_dir>] [-r <release>] [-p <project>] [-n <version>] [command] [option1 option2]....

      -v, --verbose     Be verbose about what gets done
      -s, --start-dir   The search location for the project
      -r, --release     the release to build
      -p, --project     the name of the project
      -n, --nversion    the version of the project
      var=value         Variables that will affect the compilation (e.g. debug=1)
      command           Commands that will be executed by erlb (e.g. compile)

     available commands are as follows


      depends             : dependency resolution for the project
      prepare             : build system preparation
      version             : Provides sinan version information
      test                : Runs all of the existing eunit unit tests in the project
      shell               : Provides an erlang repl on the project
      release             : Creates an otp release for the system
      help                : Provides help information for the available tasks
      gen                 : generates a new skeleton project
      doc                 : Genarates edoc documentation for the project
      dist                : Provides a standard erlang distribution tarball
      clean               : removes all build artifacts in the system
      build               : compiles the files in the project
      xref                : Runs xref on the project, to detect problems
      erts                : A very simple command that prints erts version sinan is running on
      escript             : Provides a standard erlang escript
      cucumber            : Runs all of the cucumber features in the project

    for more information run 'sinan help <command>'

Lets get some detail on the cucumber task and se what the system has to say.

    $> sinan help cucumber
    starting: help

    example: sinan cucumber [gen <feature-name> where <app-name>]

    This command makes use of the cucumberl to run cucumberl on any
    features in the <project-root>/features directory of project. The
    implemenation of your features can be in any OTP Application in the
    system in other the src or test directories. Check the documentation
    for cucumberl and Cucumber for details of these systems.

    You may also use this task to generate the cucumberl implementation
    after the feature card is written with the following syntax:

    sinan cucumber gen my_cool_feature where my_cool_app

    This will result in a my_cool_feature.erl skeleton implementation in
    the test directory of my_cool_app in your project. The feature name
    should be specified by name only. That is, without the .feature part
    of the file name.

    don't rely on the output here too much, go ahead and run help for
    yourself. The output changes and (hopefully) becomes more useful over
    time.

#### Other Helpful Tasks

There are quite a few other tasks that it is useful to be aware of.

- The Xref Task: A task that runs the xref tool

---
layout: default
title: Getting Started
---


What is Sinan
-------------

Sinan is a build tool designed to build Erlang/OTP Projects. The
output of the build system is OTP compliant Releases and
Applications. Sinan leverages the metadata artifacts provided by OTP
to do a good job building, testing, releasing, etc with very little or
no additional input from the developer.

More Information and FAQ
------------------------

Sinan has extensive further documentation in its
[wiki on github](https://github.com/erlware/sinan/wiki). Check there
for more information.

The Sinan FAQ is available at
[here](https://github.com/erlware/sinan/wiki/FAQ).

### The Community

A community exists around Sinan and the other Erlware projects. You
may participate in the community and ask questions by joining the
[erlware-questions](http://groups.google.com/group/erlware-questions)
mailing list.



Generate A Project
------------------

You can skip this step if you already have project. If not continue.

    $> sinan gen foo

This will take you through a series of quetions about yourself and the
project. I have provided a series of answers here, that illustrate how
to answer these questions.

Building
--------

Now just cd into your project top level directory and type

    sinan build

This will give you an fully built project under the _build
directory. You can then run the command

    sinan shell

to get an erlang shell with all the paths pointing correctly to the
various parts of your system.

Other Interesting Things
------------------------

If you want to get adventurous you can run all the eunit tests in your
app by running:

    sinan test

and finially, if you want to package up a normal erlang release
tarball you can run

    sinan dist

The tarball will end up in

    <project-root>/_build/<release-name>/tar/<app-name>-<app-vsn>.tar.gz

To get a list of all tasks currently available run the command

    sinan help

Hopefully thats enough to get you started, but sinan has many options
to do various things with projects from small single app projects to
very large multiple app projects. To get more information take a look
at the sinan manual.

Trouble Shooting
----------------

If you end up seeing an error that looks like the following:

    It looks like we couldn't satisfy all the dependency constraints We are
    going to search the space to see what the problem is but this could take a while
    Getting the powerset of all constraints
    Power set contains 4 elements
    Doing optimal sort of the power set
    Looking for the first passing constraint set
    Unable to resolve compile time dependencies, probably do to the
    following constraints:
    constraint on proper with constraints [proper] originating from these
    application(s) ['__top_level__'] constraint on eunit with constraints
    [eunit] originating from these application(s) ['__top_level__']

Its probably because you are missing some of the compile time
dependencies that all sinan projects need. Unfortunately, we can't
distribute these dependencies with sinan due to some of the
limitations with escript. However, they have been packaged up for
download [here](https://github.com/downloads/erlware/sinan). Unzip
these applications into your erlang lib or into a location pointed to
by ERL_LIB and it should solve this problem.

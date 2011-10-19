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

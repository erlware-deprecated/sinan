---
layout: default
title: Build Hooks
---

Sinan provides a mechenism to integrate with external tools. This is
via the hooks functionality. This functionality is used by putting a
directory called '_hooks' in the root of your project. Inside this
directory you place executable files that implement the functionality
you want to run. When these files are run is determenid by there
name. The are named in the form of &lt;when&gt;-&lt;task&gt;. The when is may be
one of two values, either 'pre' or 'post'. The task is
the name of the task you want the thing to run around. For example, if
you wanted something to run just after the build task is complete you
would create a file called 'post-build' and make it
executable. This file will then be run (in the context of the root
directory) immediatly after the build task and before any other tasks
or run. A file called 'pre-build' would do just the opposite,
running imeditaly before the build task.

Having the ability to run these files at specific times is
good. However, without getting some information from sinan its not
terribly useful. Fortunatly, there is a mechenism for getting this
information. Sinan passes it to the running script via environmental
variables. There are two sets of environmental variables that are
passed. The first is the global information about the project. These
variables are available as follows.

PREFIX
: The file path of the erlang/erlware installation that sinan is
  running out of.

ERTS_VSN
: The erts version that sinan is building of

BUILD_DIR
: The build directory where the projectis being built too

BUILD_FLAVOR
: The current build flavor of the system

BUILD_REF
: The unique id for this run of sinan

PROJECT_DIR
: The project root directory

PROJECT_NAME
: The project name as defined in the build config

PROJECT_VSN
: The project version

PROJECT_APPS
: A comma seperated list of apps that are part of the project.

PROJECT_DEPS
: A comma seperated list of apps that are dependencies of the project


The second set environmental variables that are available for each
application and dependency in the project. The 'APP' in the following
descriptions is replaced by the name of the app in upper case. These
values are as follows.

APP_VSN
: The version of the application

APP_LOCATION
: The file path to the application that sinan is using.

APP_DEPS
: A comma seperated list of the names of the dependent
  applicatinos for this application.

The combination of scripts and environmental variables should be
enough to do most required actions on the these tertiary builds.

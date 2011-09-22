---
layout: default
title: Sinan Projects
---

A project is a convienience tool used in developement and a way to
group applications that you are working on together that will be part
of the same release. Physically a project should be group of
applications under a single file hierarchy. Applications can be spread
out under this file heirarchy in any way you, the developer, would
like.

Sinan recognizes two types of projects Single-App projects and
Multi-App projects. For the most part these are treated the same, but
there are some minor differences. The differences are in how sinan
gets the information it needs for running its tasks. Sinan needs two
bits of additional information to be able to run all of its commands
on a project. Those are the project name and the project version. For
Single-App projects it simple uses the name and version of the
application that is part of the project, getting that information from
the *.app file in the ebin directory. For the larger, Multi-App
project there is no way for sinan to extrapolate this information,
there for you must provide it on the command line. The way of
gathering this information is the big difference between single-app
and multi-app projects.  To make a bit more sense of things, lets look
at some examples

    my_app
        |- ebin
        |- include
        |- priv
        |- src

As you can see, this is a normal OTP Application. You just change
directory to the root and build and everything should happen as you
expect.

    my_project
        |- bin
        |- some_strange_app_dir
                  |- app1
                  |- app2
                  |- app3
        |- some_other_app_dir
                  |- app4
        |- app5
        |- app6

Sinan will find and build all of the apps, 1 - 6, in this project
without a problem. Sinan doesn't care how you organize your apps as
long as they are under the same heirarchy sinan will build them. That
said, you probably don't want to organize your project this way. Its
much better to be consistant and its even better to be consistant with
the conventions in the Erlang world. In the Erlang world its much,
much more common to have all of your applications under a lib dir in
your project and all of your non-otp dirs at the same level as your
lib. Lets take our previous example and make it consistant with this
standard.

    my_project
        |- bin
        |- lib
            |- app1
            |- app2
            |- app3
            |- app4
            |- app5
            |- app6
        |- docs
        |- other_stuff

Again, Sinan will happily build this project, but its layed out more
consistantly both with what you will probably do in the future and
what other folks with OTP projects tend to do. In the end its up to
you and what works for your project and development style, but we
recommend that you start with this.

### Defining a Project

In general you don't actually have to do anything to define a
project. Just run the sinan command that you would like to run in the
root of your project. If its a one app project that is all you ever need
to do, Sinan will pick up the project name and the project version
from the applications *.app metadata file. However, if you are in a
multi-app project there is no easy way for sinan to find this
information. To get around this problem you must provide the
project name and version on the command line when you build in the
root of a multi-app project. You can do this as follows.


    $> sinan -p my_project -n 0.1.0.0

That will give sinan the information that it needs to do the task
correctly.

Always having to change directory up to the project root to run a
command can be a bit of a pain though. There are two ways to get
around this, the first and easiest way is to simply have already done
at least one command in the project root already. Sinan will look for
it's _build output directory and use the directory that contains that
_build derectory as the project root. That tends to workout quite
well if you have already built once before. However, another way is to
provide a sinan.cfg file in your project root. If the sinan.cfg exists
sinan will find it and use that as the project root, from which it
will look for your OTP applications. If you do decide to use a project
config, you can put your project name and version number there. Then
you will no longer need to specify the project name and version on the
command line when you build multi-project applications. See the
section on the project config to get more information on what you can
put in your project config file.


---
layout: default
title: OTP Applications
---

OTP applications are just groups of related code. They can be what we
refer to as library applications:a collection of modules to be used by
other applications. More often, OTP applications are living things
that are started, run for some time, and eventually shut down. These
are active applications. An active application has an application
behavior and a root supervisor whose job is to manage the processes of
the application.

In the context of OTP, the word application has a specific meaning: an
application is a software component consisting of a number of modules
bundled together with a few additional metadata files, and organized
on disk according to certain conventions. This allows the system to
know which applications are currently installed, and, for example,
lets you start or stop an application by its name.

OTP Application Directory Structure
-----------------------------------

Creating an OTP application consists mostly of setting up a standard
directory struc- ture and writing some application metadata. This
metadata tells the system what it Terminology: applications needs to
know to start and stop the application. It also specifies the
dependencies of the application, such as what other applications need
to be present or started beforehand.

OTP applications use a simple directory layout.

    <application-name>
        |- ebin
        |- src
        |- include
        |- doc

You should of course replace <application-name> with the name of your
application. The names of the subdirectories are self-explanatory.

### Subdirectories of the application directory

- src: This is where your *.erl files and private *.hrl files go
- doc: Documentation. If you generate documentation from EDoc, you put
  your overview.edoc file here.
- ebin: It’s the location of the .app file, in a fully built system
  (as part of the release), sinan also puts the beam files here.
- include: Public header files. Any .hrl file that is part of your
  public API should be kept in this directory. Private .hrl files that
  are only used within your code and aren’t intended for public
  consumption should be kept under src with the rest of your source
  code.


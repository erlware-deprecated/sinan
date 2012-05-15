Building Sinan from Source
==========================

The easiest way to get and use Sinan is to download the precompiled
version from [the github sinan site]
(https://github.com/downloads/erlware/sinan/sinan)

To build Sinan, start with the Makefile in the root of the Sinan
project. Unfortunately, Sinan can not actually build itself directly
due to the lack of sandboxing support in the Erlang code loading
subsystem. The purpose of the Makefile based bootstraping in the Sinan
project is to get around that limitation.

To get started building Sinan from source we need to have a few
dependencies in place.

Dependencies
------------

* [erlware_commons](https://github.com/erlware/erlware_commons)
* [cucumberl](https://github.com/membase/cucumberl)
* [getopt](https://github.com/jcomellas/getopt)
* [proper](https://github.com/manopapad/proper)
* [joxa](https://github.com/erlware/joxa)

The versions of these OTP Applications that are required are detailed
in the sinan.config located in the top level of the sinan project. If
no version is specified there then any version will do quite well.

Building the System
-------------------

Make sure you are in the root of the system. Then run the following
command:

    $> make build

Assuming everything built correctly you are ready to move on to the
next step.

Testing The System
------------------

You may, at your discression, run sinan's test suite. There are four
different kinds of tests in Sinan and all four sets of tests must pass
for for the built version of Sinan to be considered stable. To run
these tests enter the following commands on the command line.

    $> make eunit
    $> make cucumber
    $> make proper
    $> make smoketests

The smoke tests have a lot of output. Its actually easier to see
failures if you pipe standard out into /dev/null and then look for
errors from standard error as follows:

    $> make smoketests > /dev/null

With this command all errors in the smoketest should be immediately
obvious.

Assuming all of these tests pass you are ready to move on to
packaging.

Packaging Sinan as an Escript
-----------------------------

As with everything else you can package sinan simply by running the
following command:

    $> make escript

This will run Sinan's escript task on Sinan itself producing a 'sinan'
escript. The location of the escript will be printed as the last line
of output after you run the command.

Starting Development
--------------------

Checkout the
[sinan wiki](https://github.com/erlware/sinan/wiki/_pages) for more
information about sinan and how to get started with it. Also checkout
the
[Building From Source](https://github.com/erlware/sinan/wiki/BuildingFromSource)
document for expanded coverage of building sinan.

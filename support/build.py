 #! /bin/python
"""Support for building sinan, bootstraping it on a new version of erlang"""


import sys
import os
import commands
from optparse import OptionParser

class BuildError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return repr(self.value)

ERTS_VERSION = "5.6.3"

BUILD_PATH = "_build/development/apps/%s/ebin"

ERLWARE_PATH = "/usr/local/erlware"

ERLC = "erlc +debug_info "

LOCAL_APPS = [("etask", "0.5.0"),
              ("sinan", "0.10.0.14"),
              ("sinan_web_api", "0.1.0.4")]

ERLWARE_APPS = ["fconf-0.3.0.0",
                "ktuo-0.4.0.1",
                "crary-0.2.3",
                "eunit-2.0",
                "cryptographic-0.2.1",
                "ewlib-0.8.2.0",
                "ewrepo-0.18.6.0",
                "gas-6.1.1",
                "kernel-2.12.3",
                "ibrowse-1.4",
                "uri-0.2.0"]


def generate_local_path(app):
    ebin = "_build/development/apps/%s-%s/ebin" % (app[0], app[1])
    include = "_build/development/apps/%s-%s/include" % (app[0], app[1])

    if not os.path.isdir(ebin):
        raise BuildError(ebin + " is not a directory")

    return " -pa %s -I %s " % (ebin, include)

def generate_erlware_path(path):
    ebin = "%s/packages/%s/lib/%s/ebin" % (ERLWARE_PATH, ERTS_VERSION, path)
    include = "%s/packages/%s/lib/%s/include" % (ERLWARE_PATH, ERTS_VERSION, path)

    if not os.path.isdir(ebin):
        raise BuildError(ebin + " is not a directory")


    return " -pa %s -I %s " % (ebin, include)

def compile_app(app):
    ebin = "_build/development/apps/%s-%s/ebin" % (app[0], app[1])
    compile_command = ("erlc +debug_info %s %s -o %s/ ./server/%s/src/*.erl" %
                       (' '.join(map(generate_local_path, LOCAL_APPS)),
                        ' '.join(map(generate_erlware_path, ERLWARE_APPS)),
                        ebin,
                        app[0]))

    (status, out) = commands.getstatusoutput(compile_command)

    if 0 != status:
        raise BuildError(out)


def compile_apps():
    for app in LOCAL_APPS:
        compile_app(app)

def main():
    parser = OptionParser()
    parser.add_option("-e", "--erlware",
                      dest="erlware",
                      type="string",
                      default="/usr/local/erlware",
                      help="The location of Erlware")

    (options, args) = parser.parse_args()

    ERLWARE_PATH = options.erlware

    compile_apps()

if __name__ == "__main__":
    main()

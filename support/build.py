#! /usr/bin/python
"""Support for building sinan, bootstraping it on a new version of erlang"""


import sys
import os
import commands
import support
from optparse import OptionParser

def compile_app(app):
    ebin = "_build/development/apps/%s-%s/ebin" % (app[0], app[1])
    compile_command = ("erlc +debug_info %s %s -o %s/ ./server/%s/src/*.erl" %
                       (' '.join(map(support.generate_local_path,
                                     support.LOCAL_APPS)),
                        ' '.join(map(support.generate_erlware_path,
                                     support.ERLWARE_APPS)),
                        ebin,
                        app[0]))

    print compile_command
    (status, out) = commands.getstatusoutput(compile_command)

    print out

def compile_apps():
    for app in support.LOCAL_APPS:
        compile_app(app)

def main():
    parser = OptionParser()
    parser.add_option("-e", "--erlware",
                      dest="erlware",
                      type="string",
                      default="/usr/local/erlware",
                      help="The location of Erlware")

    (options, args) = parser.parse_args()

    support.ERLWARE_PATH = options.erlware

    compile_apps()

if __name__ == "__main__":
    main()

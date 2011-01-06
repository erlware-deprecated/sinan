#! /usr/bin/python
"""Support for building sinan, bootstraping it on a new version of erlang"""


import sys
import os
import commands
import support
from optparse import OptionParser

def get_ebin(app):
   return "_build/development/apps/%s-%s/ebin" % (app[0], app[1])

def run_app(prefix):
    run = get_ebin(("sinan", support.VERSION)) + ' ' + ' '.join(map(support.generate_erlware_path,
                                                                  support.ERLWARE_APPS))
    return run

def main():
    parser = OptionParser()
    parser.add_option("-e", "--erlware",
                      dest="erlware",
                      type="string",
                      default="/usr/local/lib/erlang",
                      help="The location of Erlware")

    (options, args) = parser.parse_args()

    support.ERLWARE_PATH = options.erlware

    run = run_app(options.erlware)

    args = filter(lambda a: a != "", ["erl"] + run.split(" "))

    print "erl ", " ".join(args)

    os.execvp("erl", args)

    if 0 != status:
        raise BuildError(out)


if __name__ == "__main__":
    main()

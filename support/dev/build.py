#! /usr/bin/python
"""Support for building sinan, bootstraping it on a new version of erlang"""


import sys
import os
import commands
import support
from optparse import OptionParser

import os, errno

def mkdir_p(path):
    try:
        os.makedirs(path)
    except OSError as exc: # Python >2.5
        if exc.errno == errno.EEXIST:
            pass
        else: raise

def compile_app():
    ebin = "_build/development/apps/sinan-%s/ebin" % support.VERSION

    mkdir_p(ebin)

    compile_command = ("erlc +debug_info -pa %s  %s -o %s ./src/*.erl" %
                        (ebin,
                         ' '.join(map(support.generate_erlware_path,
                                     support.ERLWARE_APPS)),
                        ebin))

    print compile_command
    (status, out) = commands.getstatusoutput(compile_command)

    print out


def main():
    parser = OptionParser()
    parser.add_option("-e", "--erlware",
                      dest="erlware",
                      type="string",
                      default="/usr/local/lib/erlang",
                      help="The location of Erlware")

    (options, args) = parser.parse_args()

    support.ERLWARE_PATH = options.erlware

    compile_app()

if __name__ == "__main__":
    main()

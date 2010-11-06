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

ERTS_VERSION = "5.7.4"

BUILD_PATH = "_build/development/apps/%s/ebin"

ERLWARE_PATH = "/usr/local/lib/erlang"

ERLC = "erlc +debug_info "

VERSION = "0.20.0.0"


ERLWARE_APPS = ["tools-2.6.5.1",
                "ktuo-0.5.0.0",
                "eunit-2.1.5",
                "cryptographic-0.2.2",
                "ewlib-0.9.2.0",
                "ewrepo-0.19.2.0",
                "kernel-2.13.5",
                "sasl-2.1.9",
                "ibrowse-1.4",
                "getopt-0.3.0",
                "sgte-0.7.1",
                "asn1-1.6.13"]


def generate_local_path(app):
    ebin = "_build/development/apps/%s-%s/ebin" % (app[0], app[1])
    include = "_build/development/apps/%s-%s/include" % (app[0], app[1])

    if not os.path.isdir(ebin):
        raise BuildError(ebin + " is not a directory")

    return " -pa %s -I %s " % (ebin, include)

def generate_erlware_path(path):
    ebin = "%s/lib/%s/ebin" % (ERLWARE_PATH, path)
    include = "%s/lib/%s/include" % (ERLWARE_PATH, path)

    if not os.path.isdir(ebin):
        raise BuildError(ebin + " is not a directory")


    return " -pa %s -I %s " % (ebin, include)

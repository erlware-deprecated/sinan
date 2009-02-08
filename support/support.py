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
              ("sinan", "0.11.0.1"),
              ("sinan_web_api", "0.1.0.5")]

ERLWARE_APPS = ["fconf-0.3.0.0",
                "ktuo-0.4.0.1",
                "crary-0.2.3",
                "eunit-2.0",
                "cryptographic-0.2.1",
                "ewlib-0.8.2.0",
                "ewrepo-0.19.0.0",
                "gas-6.1.1",
                "kernel-2.12.3",
                "ibrowse-1.4",
                "uri-0.2.0",
                "sgte-0.7.1",
                "gtime-0.9.4",
                "asn1-1.5.2"]


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

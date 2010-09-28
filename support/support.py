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

ERTS_VERSION = "5.8"

BUILD_PATH = "_build/development/apps/%s/ebin"

ERLWARE_PATH = "/usr/local/erlware"

ERLC = "erlc +debug_info "

LOCAL_APPS = [("etask", "0.6.3"),
              ("sinan", "0.18.0.0"),
              ("sinan_web_api", "0.1.0.6")]

ERLWARE_APPS = ["tools-2.6.6.1",
                "ktuo-0.4.0.3",
                "crary-0.2.5",
                "eunit-2.1.4",
                "cryptographic-0.2.2",
                "ewlib-0.9.2.0",
                "ewrepo-0.18.8.0",
                "kernel-2.14.1",
                "sasl-2.1.9.2",
                "ibrowse-1.4",
                "uri-0.2.0",
                "sgte-0.7.1",
                "gtime-0.9.4",
                "asn1-1.6.14.1"]


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

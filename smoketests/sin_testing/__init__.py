#! /usr/bin/env python
# This smoke test relys on pyexect. You need to have that installed
# before you can run it.  It is available here
# http://www.noah.org/wiki/pexpect

import logging
import os
import pexpect
import unittest
import exceptions
import tempfile
import sys
import re

class TestError(exceptions.Exception):
    """Raised when a test fails """
    def __init__(self, result):
        self.result = result


class LoggerWriter(object):
    def __init__(self):
        pass

    def write(self, data):
        print(data.rstrip())

    def flush(self):
        #no op
        self

def spawn(command):
    child = pexpect.spawn(command)
    child.logfile_read = LoggerWriter()
    return child

def sinan(command):
    def check_accepts(f):
        def new_f(*args, **kwds):
            print("Running Command %s in %s" % (command, os.getcwd()))
            self = args[0]

            ebin = ""
            with open(os.path.join(self.sinan_dir, "sinan.config"), "r") as fl:
                data = fl.read()
                vsn = re.search(r"""{project_vsn, "(.+)"}""", data).group(1)

                ebin = os.path.join(self.sinan_dir,
                                    "_build",
                                    "sinan",
                                    "lib",
                                    "sinan-" + vsn, "ebin")

            child_cmd = ("erl -noshell -pa %s "
                          " -s sinan manual_start"
                          " -s sinan main"
                          " -extra %s" %
                          (ebin, command))
            print child_cmd
            child = spawn(child_cmd)
            res = f(self, child, *(args[1:]), **kwds)
            print("Finished %s successfully" % command)
            return res
        new_f.func_name = f.func_name
        return new_f
    return check_accepts


class AppDesc(object):
    def __init__(self,
                 user_name=None,
                 email=None,
                 copyright_holder=None,
                 project_name=None,
                 project_version=None,
                 app_names=None):
        self.user_name = user_name
        self.email = email
        self.copyright_holder = copyright_holder
        self.project_name = project_name
        self.project_version = project_version
        self.app_names = app_names



def run_tests(class_obj):
    cases = unittest.defaultTestLoader.loadTestsFromTestCase(class_obj)
    result = unittest.TextTestRunner().run(cases)
    if len(result.errors) > 0 or len(result.failures) > 0:
        raise TestError(result)


class SmokeTest(unittest.TestCase):
    def get_project_root(self, cwd):
        return os.path.abspath(cwd)

    def setUp(self):
        self.release_name = None
        self.release_version = None
        self.smokedir = tempfile.mkdtemp(prefix='smoke_test_')

        self.current_dir = os.getcwd()
        self.sinan_dir = self.current_dir
        sys.path.append(self.current_dir)
        os.chdir(self.smokedir)

    def tearDown(self):
        os.chdir(self.current_dir)

    def assert_dirs_exist(self, base, *dirs):
        for d in dirs:
            check_dir = ""
            if type(d) == list:
                check_dir = os.path.join(base, *d)
            else:
                check_dir = os.path.join(base, d)
            self.assertTrue(os.path.isdir(check_dir))

    def assert_files_exist(self, base, *files):
        for f in files:
            check_file = ""
            if type(f) == list:
                check_file = os.path.join(base, *f)
            else:
                check_file = os.path.join(base, f)

            self.assertTrue(os.path.isfile(check_file))

    def do_apply(self, fun_list, arg):
        res = arg
        for n in fun_list:
            f = getattr(self, n)
            res = f(res)
        return res

    @sinan("gen")
    def run_gen(self, child, appdesc):

        child.expect("your name> ")
        child.sendline(appdesc.user_name)
        child.expect("your email> ")
        child.sendline(appdesc.email)
        child.expect('copyright holder \("%s"\)> ' % appdesc.user_name)
        child.sendline()
        child.expect('project name> ')
        child.sendline(appdesc.project_name)
        child.expect('project version> ')
        child.sendline(appdesc.project_version)
        child.expect('Please specify the ERTS version \(".*"\)> ')
        child.sendline()
        child.expect('Is this a single application project \("n"\)> ')
        child.sendline()
        child.expect("app> ")
        child.sendline(appdesc.app_names[0])
        for n in appdesc.app_names[1:]:
            child.expect('app \(""\)> ')
            child.sendline(n)

        child.expect('app \(""\)> ')
        child.sendline()
        child.expect('\("y"\)> ')
        child.sendline()
        child.expect("Project was created, you should be good to go!")
        child.expect(pexpect.EOF)
        return appdesc

    def verify_gen(self, a):
        projdir = os.path.join(os.getcwd(), a.project_name)

        self.assert_dirs_exist(projdir,
                               "config",
                               "lib")

        self.assert_files_exist(projdir,
                                ["config", "sys.config"],
                                "sinan.config")
        for n in a.app_names:
            ppath = os.path.join(projdir, "lib", n)

            self.assert_dirs_exist(ppath,
                                   "ebin",
                                   "src",
                                   "include",
                                   "doc")
            self.assert_files_exist(ppath,
                                    ["src", n + "_app.erl"],
                                    ["src", n + "_sup.erl"],
                                    ["src", n + ".app.src"])

        return a

    # gen a new project in the test dir
    def do_gen(self, appdesc):
        return self.do_apply(["run_gen", "verify_gen"], appdesc)

    # build the project
    @sinan("build")
    def do_build(self, child, appdesc):
        child.expect(pexpect.EOF)

        build_tmp = self.get_build_root_path()
        build_dir = os.path.join(*(build_tmp))
        self.assertTrue(build_dir)

        for n in appdesc.app_names:
            app_dir = os.path.join(build_dir, "lib", "%s-0.1.0" % n)
            print app_dir
            self.assert_dirs_exist(app_dir,
                                   "ebin",
                                   "src",
                                   "include",
                                   "doc")

            self.assert_files_exist(app_dir,
                                    ["src", n + "_sup.erl"],
                                    ["src", n + "_app.erl"],
                                    ["ebin", n + "_sup.beam"],
                                    ["ebin", n + "_app.beam"])

        return appdesc


    # clean the project
    @sinan("clean")
    def do_clean(self, child, appdesc):
        child.expect(pexpect.EOF)
        self.assertTrue(not os.path.isdir(os.path.join(os.getcwd(), "_build")))
        return appdesc

    # test the project
    @sinan("test")
    def do_t(self, child, appdesc):
        child.expect(pexpect.EOF)
        return appdesc

    # release
    @sinan("release")
    def do_release(self, child, appdesc):
        child.expect(pexpect.EOF)
        version = appdesc.project_version
        name = appdesc.project_name
        build_tmp = self.get_build_root_path()
        build_tmp.append("releases"),
        build_tmp.append(version)
        version_dir = os.path.join(*build_tmp)

        print("Checking version directory at %s " % version_dir)
        self.assert_files_exist(version_dir,
                                "%s.boot" % name,
                                "%s.rel" % name,
                                "%s.script" % name,
                                "sys.config")
        return appdesc

    # dist (check the tarball)
    @sinan("dist")
    def do_dist(self, child, appdesc):
        child.expect(pexpect.EOF)
        build_tmp = self.get_build_root_path()
        build_tmp.append("tar")
        build_tmp.append("%s-%s.tar.gz" %
                         (appdesc.project_name, appdesc.project_version))
        tar_file = os.path.join(*build_tmp)

        print tar_file
        self.assertTrue(os.path.isfile(tar_file))

        return appdesc

    def do_run(self, appdesc):
        self.current_app_desc = appdesc
        a = self.do_gen(appdesc)

        self.project_dir = os.path.join(self.smokedir, a.project_name)

        os.chdir(os.path.join(self.project_dir))

        self.do_apply(["do_build",
                       "do_clean",
                       "do_build",
                       "do_t",
                       "do_release",
                       "do_dist"], a)

    def get_build_root_path(self, project_dir=None, release_name=None,
                            release_version=None):
        if not project_dir:
            project_dir = self.project_dir

        if not release_name and not self.release_name:
            release_name = self.current_app_desc.project_name
        elif not release_name:
            release_name = self.release_name

        if not release_version and not self.release_version:
            release_version = self.current_app_desc.project_version
        elif not release_version:
            release_version = self.release_version

        return [project_dir,
                "_build",
                release_name]


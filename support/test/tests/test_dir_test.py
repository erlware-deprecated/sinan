import unittest
import sin_testing as st
import pexpect
import os

class TestFail(st.SmokeTest):


    @st.sinan("build")
    def build(self, child, app_desc):
        if not os.path.isfile(os.path.join(os.getcwd(),
                                           "test", "test_module.erl")):
            raise "Nome module file"

        child.expect(pexpect.EOF)

        if not os.path.isfile(os.path.join(os.getcwd(),
                                           "_build", "development",
                                           "apps", app_desc.project_name + "-" +
                                           app_desc.project_version, "ebin",
                                           "test_module.beam")):
            raise "File Not Built"


    def output_testdir(self):
        path = os.path.join(os.getcwd(), "test")
        try:
            os.makedirs(path)
        except OSError as exc:
            if exc.errno == errno.EEXIST:
                pass
            else: raise

        Module = """
-module(test_module).

-export([test/0]).

test() ->
    ok."""

        module_file = os.path.join(path, "test_module.erl")
        new_file = open(module_file, "w")
        new_file.write(Module)
        new_file.close()

    @st.sinan("gen foo")
    def run_custom_gen(self, child, appdesc):
        child.expect("your name> ")
        child.sendline(appdesc.user_name)
        child.expect("your email> ")
        child.sendline(appdesc.email)
        child.expect('copyright holder \("%s"\)> ' % appdesc.user_name)
        child.sendline()
        child.expect('project version> ')
        child.sendline(appdesc.project_version)
        child.expect('Please specify the ERTS version \(".*"\)> ')
        child.sendline()
        child.expect('Is this a single application project \("n"\)> ')
        child.sendline("y")
        child.expect('Would you like a build config\? \("y"\)> ')
        child.sendline()
        child.expect("Project was created, you should be good to go!")
        child.expect(pexpect.EOF)


    def test_gen_name(self):
        appdesc = st.AppDesc(user_name = "Smoke Test Gen",
                             email = "noreply@erlware.org",
                             copyright_holder = "Smoke Test Copy, LLC.",
                             # This needs to match the gen name since
                             # we are overriding it
                             project_name = "foo",
                             project_version = "0.134.0.0")

        self.run_custom_gen(appdesc)

        currentdir = os.getcwd()
        projdir = os.path.join(currentdir, appdesc.project_name)
        os.chdir(projdir)

        self.output_testdir()
        self.build(appdesc)

        os.chdir(currentdir)

if __name__ == '__main__':
    unittest.main()

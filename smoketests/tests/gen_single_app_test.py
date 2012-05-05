import unittest
import sin_testing as st
import pexpect

class TestFail(st.SmokeTest):

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


if __name__ == '__main__':
    unittest.main()

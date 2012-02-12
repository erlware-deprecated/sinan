import unittest
import pexpect
import sin_testing as st
import os

class TestEcho(st.SmokeTest):

    @st.sinan("echo paths includes")
    def do_echo(self, child):
        child.expect("-pa")
        child.expect("-I")
        child.expect(pexpect.EOF)


    def test_echo(self):
        app_desc = st.AppDesc(user_name = "Smoke Test User",
                              email = "noreply@erlware.org",
                              copyright_holder = "Smoke Test Copy, LLC.",
                              project_name = "smprj",
                              project_version = "0.21.0.0",
                              app_names = ["app1", "app2", "app3"])


        self.do_run(app_desc)
        builddir = os.path.join("_build", app_desc.project_name)

        self.do_echo()


if __name__ == '__main__':
    unittest.main()

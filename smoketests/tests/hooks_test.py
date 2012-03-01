import unittest
import pexpect
import os
import sin_testing as st

class TestHooks(st.SmokeTest):

    def add_hooks(self):
        hooks_dir = os.path.join(self.project_dir, "_hooks")
        os.mkdir(hooks_dir)
        hook_file = os.path.join(hooks_dir, "pre_build")

        f = open(hook_file, "w")
        f.write("""#!/bin/sh
 echo HELLO WORLD!""")
        f.close()
        os.chmod(hook_file, 0777)

    @st.sinan("build -v ")
    def do_test_build(self, child, appdesc):
        child.expect("HELLO WORLD!")
        return self.build_validate(child, appdesc)

    def do_run(self, appdesc):
        self.current_app_desc = appdesc
        a = self.do_gen(appdesc)

        self.project_dir = os.path.join(self.smokedir, a.project_name)

        self.add_hooks()

        os.chdir(os.path.join(self.project_dir))

        na = self.do_test_build(appdesc)

        self.do_apply(["do_clean",
                       "do_build",
                       "do_t",
                       "do_release",
                       "do_dist"], na)

    def test_hooks(self):
        app_desc = st.AppDesc(user_name = "Smoke Test User",
                              email = "noreply@erlware.org",
                              copyright_holder = "Smoke Test Copy, LLC.",
                              project_name = "smprj",
                              project_version = "0.21.0.0",
                              app_names = ["app1", "app2", "app3"])


        self.do_run(app_desc)
        builddir = os.path.join("_build", app_desc.project_name)



if __name__ == '__main__':
    unittest.main()

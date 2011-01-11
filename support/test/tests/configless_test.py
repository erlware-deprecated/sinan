import unittest
import sin_testing as st
import os
import shutil
import pexpect

class TestConfigless(st.SmokeTest):

    # clean the project
    @st.sinan("-p ctest_project -n 0.1.0 clean")
    def clean_configless(self, child, appdesc):
        child.expect(pexpect.EOF)
        self.assertTrue(not os.path.isdir(os.path.join(os.getcwd(), "_build")))
        return appdesc

    @st.sinan("-p ctest_project -n 0.1.0 build")
    def build_configless(self, child, app_desc):
        child.expect(pexpect.EOF)

        build_dir = os.path.join(os.getcwd(), "_build/development/apps/")
        self.assertTrue(os.path.isdir(build_dir))

        for n in app_desc.app_names:
            app_dir = os.path.join(build_dir, "%s-0.1.0" % n)
            self.assert_dirs_exist(app_dir,
                                   "ebin",
                                   "src",
                                   "include",
                                   "doc")

            self.assert_files_exist(app_dir,
                                    [ "src", n + "_sup.erl"],
                                    ["src", n + "_app.erl"],
                                    ["ebin", n + "_sup.beam"],
                                    ["ebin", n + "_app.beam"])


    def test_configless_project(self):
        app_desc = st.AppDesc(user_name = "Smoke Test User",
                              email = "noreply@erlware.org",
                              copyright_holder = "Smoke Test Copy, LLC.",
                              project_name = "configless_project",
                              project_version = "0.1.0.0",
                              app_names = ["app1", "app2", "app3"])

        self.do_run(app_desc)
        currentdir = os.getcwd()
        projdir = os.path.join(currentdir, app_desc.project_name)
        os.chdir(projdir)

        os.remove(os.path.join(projdir, "sinan.cfg"))

        self.clean_configless(app_desc)
        self.build_configless(app_desc)

        os.chdir(currentdir)

    def test_configless_single_app(self):
        app_desc = st.AppDesc(user_name = "Smoke Test User",
                              email = "noreply@erlware.org",
                              copyright_holder = "Smoke Test Copy, LLC.",
                              project_name = "app1",
                              project_version = "0.1.0.0",
                              app_names = ["app1"])

        self.do_run(app_desc)

        currentdir = os.getcwd()
        projdir = os.path.join(currentdir, app_desc.project_name)
        os.chdir(projdir)

        shutil.move(os.path.join(projdir, "lib", "app1", "src"),
                    projdir)

        shutil.move(os.path.join(projdir, "lib", "app1", "include"),
                    projdir)

        shutil.move(os.path.join(projdir, "lib", "app1", "ebin"),
                        projdir)

        shutil.rmtree(os.path.join(projdir, "lib"))
        os.remove(os.path.join(projdir, "sinan.cfg"))

        self.do_clean(app_desc)

        self.do_build(app_desc)

        os.chdir(currentdir)

if __name__ == '__main__':
    unittest.main()

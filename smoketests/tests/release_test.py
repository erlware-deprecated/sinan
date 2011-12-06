import unittest
import sin_testing as st
import os
import pexpect

class TestFoo(st.SmokeTest):



    def gen_release_file(self, app_desc):
        contents =  """
              {project_name, %(project_name)s}.
              {project_vsn, "%(project_version)s"}.

               {{dep_constraints, [{release, r1}]},
                   [{exclude, app4}]}.
               {{dep_constraints, [{release, r2}]},
                   [{exclude, app4},
                    {exclude, app1}]}.
               {{dep_constraints, [{release, r3}]},
                   [{exclude, app4},
                    {exclude, app1}]}.
               {{dep_constraints, [{release, r4}]},
                    [{exclude, app4},
                     {exclude, app1}]}.

               {{include_erts, [{release, r1}]}, true}.
        """ % {"project_name" : app_desc.project_name,
               "project_version" : app_desc.project_version}

        with open("sinan.config", "w") as f:
            f.write(contents)



    def test_foo(self):
        app_desc = st.AppDesc(user_name = "Smoke Test User",
                              email = "noreply@erlware.org",
                              copyright_holder = "Smoke Test Copy, LLC.",
                              project_name = "reltest",
                              project_version = "0.134.0.0",
                              app_names = ["app1", "app2", "app3", "app4"])


        self.do_run(app_desc)

        self.gen_release_file(app_desc)
        for r in ["r1", "r2", "r3", "r4"]:
            def run_dist(self, child):
                child.expect("starting: dist")
                child.expect(pexpect.EOF)

            self.release_name = r
            self.release_version = app_desc.project_version

            ((st.sinan("-r %s dist" % r)(run_dist))(self))

            self.assert_dirs_exist("_build",
                                   [r, "releases",
                                    self.release_version])

            self.assert_files_exist("_build",
                                   [r, "releases",
                                    self.release_version,
                                    self.release_name + ".rel"])

            self.assert_files_exist("_build",
                                   [r, "releases",
                                    self.release_version,
                                    self.release_name + ".boot"])

            self.assert_files_exist("_build",
                                   [r, "releases",
                                    self.release_version,
                                    self.release_name + ".script"])


            self.assert_files_exist("_build",
                                   [r, "tar",
                                    "%s-%s.tar.gz" % (self.release_name,
                                                      self.release_version)])

        BuildDir = os.path.join("_build", "r1")
        BuildDirList = os.listdir(BuildDir)
        self.assertTrue(len([x for x in BuildDirList if x.startswith("erts-")]) == 1)

if __name__ == '__main__':
    unittest.main()

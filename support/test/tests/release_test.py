import unittest
import sin_testing as st
import os
import pexpect

class TestFoo(st.SmokeTest):



    def gen_release_file(self, app_desc):
       contents =  """project : {
                   name : %(project_name)s
                   vsn  : "%(project_version)s"
               },

              releases : {
                 r1   : {
                        vsn : "1.0"
                        apps : ["app1", "app2", "app3"]
                        }
                 r2 : {
                         vsn : "2.0"
                         apps : ["app4", "app1"]
                      }
                 r3 : {
                         vsn : "3.0"
                         apps : ["app3", "app2"]
                      }
                 r4 : {
                         vsn : "1.1"
                         apps : ["app3", "app2"]
                      }
              }
        """ % {"project_name" : app_desc.project_name,
                      "project_version" : app_desc.project_version}

       with open("sinan.cfg", "w") as f:
           f.write(contents)



    def test_foo(self):
       app_desc = st.AppDesc(user_name = "Smoke Test User",
                             email = "noreply@erlware.org",
                             copyright_holder = "Smoke Test Copy, LLC.",
                             project_name = "reltest",
                             project_version = "0.134.0.0",
                             app_names = ["app1", "app2", "app3", "app4"])


       self.do_run(app_desc)



       currentdir = os.getcwd()
       projdir = os.path.join(currentdir, app_desc.project_name)
       builddir = os.path.join(projdir, "_build", "development")
       os.chdir(projdir)
       self.gen_release_file(app_desc)

       for r in ["r1", "r2", "r3", "r4"]:
           child = st.spawn("sinan -r %s dist" % r)
           child.expect("starting: dist")
           child.expect(pexpect.EOF)

       self.assert_dirs_exist(builddir,
                              ["releases", "r1-1.0"],
                              ["releases", "r2-2.0"],
                              ["releases", "r3-3.0"],
                              ["releases", "r4-1.1"])
       self.assert_files_exist(builddir,
                               ["tar", "r1-1.0.tar.gz"],
                               ["tar", "r2-2.0.tar.gz"],
                               ["tar", "r3-3.0.tar.gz"],
                               ["tar", "r4-1.1.tar.gz"],
                               ["releases", "r1-1.0", "r1.rel"],
                               ["releases", "r1-1.0", "r1.boot"],
                               ["releases", "r1-1.0", "r1.script"],
                               ["releases", "r2-2.0", "r2.rel"],
                               ["releases", "r2-2.0", "r2.boot"],
                               ["releases", "r2-2.0", "r2.script"],
                               ["releases", "r3-3.0", "r3.rel"],
                               ["releases", "r3-3.0", "r3.boot"],
                               ["releases", "r3-3.0", "r3.script"],
                               ["releases", "r4-1.1", "r4.rel"],
                               ["releases", "r4-1.1", "r4.boot"],
                               ["releases", "r4-1.1", "r4.script"])


       os.chdir(currentdir)

if __name__ == '__main__':
    unittest.main()

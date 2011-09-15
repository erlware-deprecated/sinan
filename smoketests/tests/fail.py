import unittest
import sin_testing as st

class TestFail(st.SmokeTest):

    def test_fail(self):
        self.do_run(st.AppDesc(user_name = "Smoke Test User",
                               email = "noreply@erlware.org",
                               copyright_holder = "Smoke Test Copy, LLC.",
                               project_name = "reltest",
                               project_version = "0.134.0.0",
                               app_names = ["app1", "app2", "app3", "app3", "app4"]))



if __name__ == '__main__':
    unittest.main()

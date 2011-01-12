import unittest
import sin_testing as st

class TestVsn(st.SmokeTest):

    def test_vsn(self):
        self.do_run(st.AppDesc(user_name = "Smoke Test User",
                               email = "noreply@erlware.org",
                               copyright_holder = "Smoke Test Copy, LLC.",
                               project_name = "smprj",
                               project_version = "0.21.0.0",
                               app_names = ["app1", "app2", "app3"]))

if __name__ == '__main__':
    unittest.main()

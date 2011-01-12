import unittest
import sin_testing as st

class TestAlphaVsn(st.SmokeTest):

    def test_alpha_vsn(self):
        self.do_run(st.AppDesc(user_name = "Smoke Test User",
                               email = "noreply@erlware.org",
                               copyright_holder = "Smoke Test Copy, LLC.",
                               project_name = "smprj_version1",
                               project_version = "V1922A",
                               app_names = ["app1"]))

if __name__ == '__main__':
    unittest.main()

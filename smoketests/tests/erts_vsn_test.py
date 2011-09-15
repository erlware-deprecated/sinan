import unittest
import sin_testing as st

class TestErtsVsn(st.SmokeTest):

    def test_erts_vsn(self):
        self.do_run(st.AppDesc(user_name = "Smoke Test User",
                               email = "noreply@erlware.org",
                               copyright_holder = "Smoke Test Copy, LLC.",
                               project_name = "smprj_version2",
                               project_version = "R12B3-122",
                               app_names = ["app1", "fubyap1"]))

if __name__ == '__main__':
    unittest.main()

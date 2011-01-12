import unittest
import pexpect
import sin_testing as st

class TestHelp(st.SmokeTest):

    @st.sinan("help")
    def test_help(self, child):
        child.expect("Compiles all of the compilable files in the project")
        child.expect(pexpect.EOF)

if __name__ == '__main__':
    unittest.main()

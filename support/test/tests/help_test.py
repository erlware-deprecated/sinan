import unittest
import pexpect
import sin_testing as st

class TestHelp(st.SmokeTest):

    @st.sinan("help")
    def test_help(self, child):
        child.expect("for more information run 'sinan help <command>'")
        child.expect(pexpect.EOF)

if __name__ == '__main__':
    unittest.main()

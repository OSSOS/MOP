import sys
import tempfile

sys.path.append('/Users/kavelaarsj/MOP_src/src/ossos-pipeline')

from unittest import TestCase

__author__ = 'jjk'

import ossos
mpc = ossos.mpc

class TestOSSOSComment(TestCase):
    def test_from_string(self):
        ossos_style_comment = """#O 1698860p25 O13AE2O Y   990.0 3698.0 22.79 0.08 0.096    3 % This is a comment."""
        self.assertIsInstance(mpc.OSSOSComment.from_string(ossos_style_comment),mpc.OSSOSComment)


class TestMPCReader(TestCase):
    def test_init(self):
        filename = tempfile.NamedTemporaryFile()
        filename.write("""#O 1698860p25 O13AE2O Y   990.0 3698.0 22.79 0.08 0.096    3 % This is a comment.""")
        filename.write("""     o3e05    C2013 06 12.37153 14 19 34.135-14 01 53.33                     568""")
        filename.flush()
        r = mpc.MPCReader(filename.name).mpc_observations
        print r
        self.assertIsInstance(r[0], mpc.Observation)

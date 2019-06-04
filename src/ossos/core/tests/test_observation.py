from unittest import TestCase
from mpc import Observation

__author__ = 'jjk'
ted_string = """2011 04  28.39916  18 32 37.019  -21 13 50.20  26.0R NI100     304"""
mpc_string = """     NI100     2011 04 28.39916 18 32 37.019-21 13 50.20         26.0 R      304"""


class TestObservation(TestCase):
    def test_from_ted(self):
        mpc = Observation.from_string(mpc_string)
        mpc.comment = None
        ted = Observation.from_ted(ted_string)
        ted.comment = None
        self.assertEqual(ted, mpc)

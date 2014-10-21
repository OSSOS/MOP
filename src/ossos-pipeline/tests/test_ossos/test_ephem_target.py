from unittest import TestCase

__author__ = 'jjk'

from ossos import ephem_target

class TestEphemTarget(TestCase):

    def test_create(self):
        """Make sure we can build an object with just a name."""
        target_name = "test"
        et = ephem_target.EphemTarget(target_name)
        print et.doc.toxml()
        self.assertIn(target_name, et.name)
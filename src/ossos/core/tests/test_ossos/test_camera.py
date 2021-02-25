from unittest import TestCase
from astropy import units

from ossos import cameras


class TestCamera(TestCase):

    def setUp(self):
        self.camera = cameras.Camera(0*units.degree, 0*units.degree)

    def test_offset(self):
        self.camera.offset(index=22)
        self.assertEqual(self.camera.ra, 0*units.degree)
        self.assertEqual(self.camera.dec, 0*units.degree)
        self.fail()

    def test_coordinate(self):
        self.fail()

    def test_coord(self):
        self.fail()

    def test_ra(self):
        self.fail()

    def test_dec(self):
        self.fail()

    def test_origin(self):
        self.fail()

    def test_origin(self):
        self.fail()

    def test_set_origin(self):
        self.fail()

    def test_polygon(self):
        self.fail()

    def test_geometry(self):
        self.fail()

    def test_separation(self):
        self.fail()

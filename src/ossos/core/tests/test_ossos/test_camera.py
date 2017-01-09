from unittest import TestCase
from astropy.coordinates import SkyCoord
from ...ossos import cameras

class TestCamera(TestCase):

    def setUp(self):
        self.camera = cameras.Camera()

    def test_coordinate(self):
        self.camera.coordinate = 0.0, 0.0
        assert isinstance(self.camera.coordinate, SkyCoord)


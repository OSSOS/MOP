import unittest
from ossos.plant import KBOGenerator
# from ossos.pipeline import plant
from ossos import storage
from astropy.io import fits
from ossos import util
from tempfile import NamedTemporaryFile
import json


class PlantTest(unittest.TestCase):

    def setUp(self):
        self.expnum = 1616681
        self.ccd = 22
        self.version = 'p'
        self.rmin = 0.5
        self.rmax = 1.5
        self.ang = 20
        self.width = 10
        self.mmin = 23.0
        self.mmax = 24.0
        self.number = 10

    def test_KBOGenerator(self):
        filename = storage.get_file(self.expnum, self.ccd, self.version, ext='fits')
        shifts = storage.get_file(self.expnum, self.ccd, self.version, ext='shifts')
        psf = storage.get_file(self.expnum, self.ccd, self.version, ext='psf.fits')
        header = fits.open(filename)[0].header
        bounds = util.get_pixel_bounds_from_datasec_keyword(header.get('DATASEC', '[33:2080,1:4612]'))
        outfile = NamedTemporaryFile()
        shifts = json.loads(open(shifts, 'rb').read())
        kbos = KBOGenerator.get_kbos(n=self.number,
                                     rate=(self.rmin, self.rmax),
                                     angle=(self.ang - self.rmax, self.rmax + self.rmin),
                                     mag=(self.mmin, self.mmax),
                                     x=(bounds[0][0], bounds[0][1]),
                                     y=(bounds[1][0], bounds[1][1]),
                                     filename=outfile.name)

        plant.plant_kbos(filename, psf, kbos, shifts, "fk")

        self.assertEqual(len(kbos), self.number)

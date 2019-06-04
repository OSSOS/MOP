from unittest import TestCase

__author__ = 'jjk'

import storage


class TestReset_datasec(TestCase):
    """
    Test that the logic of the storage.reset_datasec method is working correctly.
    """

    def test_reset_datasec_not_flipped(self):
        datasec = "[100:2000,100:4450]"
        cutout = "[101:200, 101:200]"
        naxis1 = 2048
        naxis2 = 4600
        self.assertEqual(storage.reset_datasec(cutout, datasec, naxis1, naxis2), "[1:1900,1:4350]")

    def test_reset_datasec_flipped(self):
        datasec = "[100:2000,100:4450]"
        cutout = "[200:101, 200:101]"
        naxis1 = 2048
        naxis2 = 4600
        self.assertEqual(storage.reset_datasec(cutout, datasec, naxis1, naxis2), "[1:101,1:101]")

    def test_reset_datasec_flipped_stars(self):
        datasec = "[100:2000,100:4450]"
        cutout = "[-*, -*]"
        naxis1 = 2048
        naxis2 = 4600
        self.assertEqual(storage.reset_datasec(cutout, datasec, naxis1, naxis2), "[49:1949,151:4501]")

    def test_reset_datasec_stars(self):
        datasec = "[100:2000,100:4450]"
        cutout = "[*, *]"
        naxis1 = 2048
        naxis2 = 4600
        self.assertEqual(storage.reset_datasec(cutout, datasec, naxis1, naxis2), "[100:2000,100:4450]")

    def test_get_flipped_image(self):
        """
        This test requires VOSpace be accessible.
        """
        storage.DBIMAGES = 'vos:OSSOS/TEST/dbimages/'
        image = storage.get_image('1667740', ccd=1, return_file=False)
        self.assertEqual(image[0].header['DATASEC'],"[33:2080,33:4644]")
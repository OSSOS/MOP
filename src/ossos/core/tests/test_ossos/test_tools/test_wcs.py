__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that, contains, has_length

from astropy.io import fits

from tests.base_tests import FileReadingTestCase
from tests.matchers import almost_equal
from ossos import wcs

SIGFIGS = 11


class WCSTest(unittest.TestCase):
    """
    Source for test case data:
    http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHTSG/821543p.head
    """
    header=dict(crpix1=-7535.57493517,
                crpix2 = 9808.40914361,
                crval1 = 176.486157083,
                crval2 = 8.03697351091,
                cd1_1 = 5.115244026718E-05, 
                cd1_2 = 7.064503033578E-07,
                cd2_1 = -1.280229655229E-07, 
                cd2_2 = -5.123112374523E-05,
                pv1_1 = -7.030338745606E-03, 
                pv1_2 = 1.01755337222, 
                pv1_3 = 8.262429361142E-03,
                pv1_4 = 0.00000000000, 
                pv1_5 = -5.910145454849E-04, 
                pv1_6 = -7.494178330178E-04,
                pv1_7 = -3.470178516657E-04, 
                pv1_8 = -2.331150605755E-02, 
                pv1_9 = -8.187062772669E-06,
                pv1_10 = -2.325429510806E-02, 
                pv1_11 = 1.135299506292E-04,
                pv2_1 = -6.146513090656E-03, 
                pv2_2 = 1.01552885426, 
                pv2_3 = 8.259666421752E-03,
                pv2_4 = 0.00000000000, 
                pv2_5 = -4.567030382243E-04, 
                pv2_6 = -6.978676921999E-04,
                pv2_7 = -3.732572951216E-04, 
                pv2_8 = -2.332572754467E-02, 
                pv2_9 = -2.354317291723E-05,
                pv2_10 = -2.329623852891E-02, 
                pv2_11 = 1.196394469003E-04,
                nord = 3)



    def test_xy2sky_nord3(self):
        x = 15000
        y = 20000
        crpix1 = -7535.57493517
        crpix2 = 9808.40914361
        crval1 = 176.486157083
        crval2 = 8.03697351091
        cd = [[5.115244026718E-05, 7.064503033578E-07],
              [-1.280229655229E-07, -5.123112374523E-05]]
        pv = [[-7.030338745606E-03, 1.01755337222, 8.262429361142E-03,
               0.00000000000, -5.910145454849E-04, -7.494178330178E-04,
               -3.470178516657E-04, -2.331150605755E-02, -8.187062772669E-06,
               -2.325429510806E-02, 1.135299506292E-04],
              [-6.146513090656E-03, 1.01552885426, 8.259666421752E-03,
               0.00000000000, -4.567030382243E-04, -6.978676921999E-04,
               -3.732572951216E-04, -2.332572754467E-02, -2.354317291723E-05,
               -2.329623852891E-02, 1.196394469003E-04]]
        nord = 3
        import numpy
        x = numpy.array([x,])
        y = numpy.array([y,])
        ra, dec = wcs.xy2skypv(x, y, crpix1, crpix2, crval1, crval2, cd, pv, nord)
        assert_that(ra.to('deg').value, almost_equal(177.62041959006154, SIGFIGS))
        assert_that(dec.to('deg').value, almost_equal(7.5256066570082263, SIGFIGS))

    def test_sky2xy_nord3(self):
        ra = 177.62042274595882
        dec = 7.5256071336988679
        crpix1 = -7535.57493517
        crpix2 = 9808.40914361
        crval1 = 176.486157083
        crval2 = 8.03697351091
        dc = [[19550.08417778, 269.58539826],
              [-48.85428173, -19520.05812122]]
        pv = [[-7.030338745606E-03, 1.01755337222, 8.262429361142E-03,
               0.00000000000, -5.910145454849E-04, -7.494178330178E-04,
               -3.470178516657E-04, -2.331150605755E-02, -8.187062772669E-06,
               -2.325429510806E-02, 1.135299506292E-04],
              [-6.146513090656E-03, 1.01552885426, 8.259666421752E-03,
               0.00000000000, -4.567030382243E-04, -6.978676921999E-04,
               -3.732572951216E-04, -2.332572754467E-02, -2.354317291723E-05,
               -2.329623852891E-02, 1.196394469003E-04]]
        nord = 3

        x, y = wcs.sky2xypv(ra, dec, crpix1, crpix2, crval1, crval2, dc, pv, nord)

        assert_that(x, almost_equal(15000.066582252624, SIGFIGS))
        assert_that(y, almost_equal(19999.992539886229, SIGFIGS))


class WCSParseTest(FileReadingTestCase):
    def setUp(self):
        testfile = "data/image_reading/cutout-1616687p.fits"
        hdulist = fits.open(self.get_abs_path(testfile))
        self.header = hdulist[0].header

    def test_parse_cd_values(self):
        cd = wcs.parse_cd(self.header)

        assert_that(cd, has_length(2))
        assert_that(cd[0], has_length(2))
        assert_that(cd[1], has_length(2))

        # CD1_1, CD1_2
        assert_that(cd[0],
                    contains(-5.156837193510000E-05, -3.588985558218000E-07))

        # CD2_1, CD2_2
        assert_that(cd[1],
                    contains(-1.674871346376000E-07, 5.178515755263000E-05))

    def test_parse_pv_values(self):
        pv = wcs.parse_pv(self.header)

        assert_that(pv, has_length(2))
        assert_that(pv[0], has_length(11))
        assert_that(pv[1], has_length(11))

        # PV1_0 through PV1_10
        assert_that(pv[0],
                    contains(-1.909806626877E-03, 1.00815723310,
                             1.983393486250E-03, 0.00000000000,
                             4.623172227977E-05, -1.020423075276E-05,
                             2.258600565396E-05, -2.354656733463E-02,
                             -1.671912720931E-04, -2.339051960031E-02,
                             -2.903290518437E-05))

        # PV2_0 through PV2_10
        assert_that(pv[1],
                    contains(-7.294883106769E-04, 1.00392029973,
                             1.983331686156E-03, 0.00000000000,
                             3.022667872225E-05, 4.509364811534E-05,
                             -5.385195875663E-06, -2.333221106141E-02,
                             -1.789051786060E-04, -2.348803123711E-02,
                             -2.465552723903E-05))


if __name__ == '__main__':
    unittest.main()

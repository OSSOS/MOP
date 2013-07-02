__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from hamcrest import assert_that

from test.matchers import almost_equal
from pymop.tools import wcs

SIGFIGS = 16


class WCSTest(unittest.TestCase):
    """
    Source for test case data:
    http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHTSG/821543p.head
    """

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

        ra, dec = wcs.xy2sky(x, y, crpix1, crpix2, crval1, crval2, cd, pv, nord)

        assert_that(ra, almost_equal(177.62041959006154, SIGFIGS))
        assert_that(dec, almost_equal(7.5256066570082263, SIGFIGS))

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

        x, y = wcs.sky2xy(ra, dec, crpix1, crpix2, crval1, crval2, dc, pv, nord)

        assert_that(x, almost_equal(15000.066582252624, SIGFIGS))
        assert_that(y, almost_equal(19999.992539886229, SIGFIGS))


if __name__ == '__main__':
    unittest.main()

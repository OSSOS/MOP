from astropy.coordinates import SkyCoord
from astropy.io import fits

__author__ = "David Rusk <drusk@uvic.ca>"

import unittest
from astropy import units
from mock import patch
#from hamcrest import assert_that, equal_to
from astropy import table

from ossos import storage, mpc


class ConeSearchTest(unittest.TestCase):

    def test_cone_search(self):
        mpc_line="     O13AE3Y* C2013 04 04.42583 14 09 16.989-11 14 50.60         23.18r      568 1615909p27 O13AE3Y Y  1715.1 2159.5 23.18 0.10 UUUU %"
        observation = mpc.Observation.from_string(mpc_line)

        result_table = storage.cone_search(observation.coordinate.lonangle.degrees,
                            observation.coordinate.latangle.degrees,
                            )
        self.assertIsInstance(result_table, table.Table)
        self.assertEqual(result_table['dataset_name'][0],1607614)


class DownloadImages(unittest.TestCase):

    def test_get_cutout(self):
        uri = "vos:jkavelaars/2001_QT297/FORS2.2013-06-15T08:18:06.831/FORS2.2013-06-15T08:18:06.831.fits"
        ra = 340.20001892 * units.degree
        dec = -6.82226166 * units.degree
        coo = SkyCoord(ra, dec)
        radius = 10.0 * units.arcminute
        hdulist = storage.ra_dec_cutout(uri, coo, radius)
        self.assertIsInstance(hdulist, fits.HDUList)
        self.assertTrue(len(hdulist) > 1)
        for hdu in hdulist[1:]:
            self.assertTrue(hdu.header.get("XOFFSET", False) is not False)

class ObjectCountTest(unittest.TestCase):
    @patch("ossos.storage.set_property")
    @patch("ossos.storage.get_property")
    def test_increment_object_counter(self, get_property, set_property):
        get_property.return_value = "09"

        node_uri = "vos:drusk/OSSOS/astromdir/test"
        epoch_field = "13AE"

        counter = storage.increment_object_counter(node_uri, epoch_field)

        expected_tag = storage.build_counter_tag(epoch_field)
        expected_count = "0A"

        get_property.assert_called_once_with(node_uri, expected_tag, ossos_base=True)
        assert_that(counter, equal_to(expected_count))
        set_property.assert_called_once_with(node_uri, expected_tag,
                                             expected_count, ossos_base=True)

    @patch("ossos.storage.set_property")
    @patch("ossos.storage.get_property")
    def test_increment_counter_doesnt_exist(self, get_property, set_property):
        get_property.return_value = None

        node_uri = "vos:drusk/OSSOS/astromdir/test"
        epoch_field = "13AE"

        counter = storage.increment_object_counter(node_uri, epoch_field)

        expected_tag = storage.build_counter_tag(epoch_field)
        expected_count = "01"

        get_property.assert_called_once_with(node_uri, expected_tag, ossos_base=True)
        assert_that(counter, equal_to(expected_count))
        set_property.assert_called_once_with(node_uri, expected_tag,
                                             expected_count, ossos_base=True)

    def test_build_dryrun_tag(self):
        epoch_field = "13AE"

        assert_that(storage.build_counter_tag(epoch_field, dry_run=True),
                    equal_to("13AE-object_count-DRYRUN"))

    @patch("ossos.storage.set_property")
    @patch("ossos.storage.get_property")
    def test_increment_object_counter_dryrun(self, get_property, set_property):
        get_property.return_value = "02"

        node_uri = "vos:drusk/OSSOS/astromdir/test"
        epoch_field = "13AE"

        counter = storage.increment_object_counter(node_uri, epoch_field,
                                                   dry_run=True)

        expected_tag = storage.build_counter_tag(epoch_field, dry_run=True)
        expected_count = "03"

        assert_that(counter, equal_to(expected_count))
        get_property.assert_called_once_with(
            node_uri,
            storage.build_counter_tag(epoch_field, dry_run=True),
            ossos_base=True)
        set_property.assert_called_once_with(node_uri,
                                             expected_tag,
                                             expected_count,
                                             ossos_base=True)


if __name__ == '__main__':
    unittest.main()

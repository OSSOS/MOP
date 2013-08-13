__author__ = "David Rusk <drusk@uvic.ca>"

import unittest

from mock import patch
from hamcrest import assert_that, equal_to

from ossos import storage


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

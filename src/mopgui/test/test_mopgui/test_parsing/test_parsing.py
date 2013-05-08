import unittest

from hamcrest import (assert_that, equal_to, has_length, has_entries,
                      same_instance)

from test import base_tests
from mopgui.parsing.parser import AstromParser


class ParserTest(base_tests.FileReadingTestCase):
    def setUp(self):
        self.parser = AstromParser()

        # The main test file
        filename = self.get_abs_path("data/1584431p15.measure3.cands.astrom")
        self.astrom_data = self.parser.parse(filename)

    def test_parse_observation_rawnames(self):
        assert_that(self.astrom_data.observations, has_length(3))
        assert_that(self.astrom_data.observations[0].rawname,
                    equal_to("1584431p15"))
        assert_that(self.astrom_data.observations[1].rawname,
                    equal_to("1584449p15"))
        assert_that(self.astrom_data.observations[2].rawname,
                    equal_to("1584453p15"))

    def test_parse_observation_basic_details(self):
        obs0 = self.astrom_data.observations[0]
        obs1 = self.astrom_data.observations[1]
        obs2 = self.astrom_data.observations[2]

        assert_that(obs0.expnum, equal_to("1584431"))
        assert_that(obs0.ftype, equal_to("p"))
        assert_that(obs0.ccdnum, equal_to("15"))

        assert_that(obs1.expnum, equal_to("1584449"))
        assert_that(obs1.ftype, equal_to("p"))
        assert_that(obs1.ccdnum, equal_to("15"))

        assert_that(obs2.expnum, equal_to("1584453"))
        assert_that(obs2.ftype, equal_to("p"))
        assert_that(obs2.ccdnum, equal_to("15"))

    def test_parse_observation_headers(self):
        obs0 = self.astrom_data.observations[0]
        obs1 = self.astrom_data.observations[1]
        obs2 = self.astrom_data.observations[2]

        assert_that(obs0.header, has_length(18))
        assert_that(obs0.header, has_entries(
            {"MOPversion": "1.20",
             "MJD_OBS_CENTER": "2012 10 21.40516",
             "EXPTIME": "320.14",
             "THRES": "2.70",
             "FWHM": "2.90",
             "MAXCOUNT": "30000.0",
             "CRVAL1": "26.92871",
             "CRVAL2": "29.01125",
             "EXPNUM": "1584431",
             "SCALE": "0.185",
             "CHIP": "16",
             "CRPIX1": "-3227.00",
             "CRPIX2": "-75.96",
             "NAX1": "2112",
             "NAX2": "4644",
             "DETECTOR": "MegaPrime",
             "PHADU": "1.60",
             "RDNOIS": "3.00"}
        ))

        assert_that(obs1.header, has_length(18))
        assert_that(obs2.header, has_length(18))

    def test_parse_sys_header(self):
        assert_that(self.astrom_data.sys_header, has_length(4))
        assert_that(self.astrom_data.sys_header, has_entries(
            {"RMIN": "0.5",
             "RMAX": "10.3",
             "ANGLE": "-19.9",
             "AWIDTH": "22.3"}
        ))

    def test_parse_sources(self):
        assert_that(self.astrom_data.sources, has_length(3))

        ## Test source 0
        source0 = self.astrom_data.sources[0]
        assert_that(source0, has_length(3))

        # Source 0 reading 0
        data00 = source0[0]
        assert_that(data00.x, equal_to("911.00"))
        assert_that(data00.y, equal_to("3967.12"))
        assert_that(data00.x0, equal_to("911.00"))
        assert_that(data00.y0, equal_to("3967.12"))
        assert_that(data00.ra, equal_to("26.6833367"))
        assert_that(data00.dec, equal_to("29.2203532"))

        # Source 0 reading 1
        data01 = source0[1]
        assert_that(data01.x, equal_to("944.25"))
        assert_that(data01.y, equal_to("3964.03"))
        assert_that(data01.x0, equal_to("938.93"))
        assert_that(data01.y0, equal_to("3965.78"))
        assert_that(data01.ra, equal_to("26.6816808"))
        assert_that(data01.dec, equal_to("29.2202748"))

        # Source 0 reading 2
        data02 = source0[2]
        assert_that(data02.x, equal_to("949.76"))
        assert_that(data02.y, equal_to("3963.12"))
        assert_that(data02.x0, equal_to("943.91"))
        assert_that(data02.y0, equal_to("3965.20"))
        assert_that(data02.ra, equal_to("26.6813840"))
        assert_that(data02.dec, equal_to("29.2202469"))

        ## Test source 1
        assert_that(self.astrom_data.sources[1], has_length(3))

        ## Test source 2
        source2 = self.astrom_data.sources[2]
        assert_that(source2, has_length(3))

        # Source 2 reading 2
        data22 = source2[2]
        assert_that(data22.x, equal_to("1800.48"))
        assert_that(data22.y, equal_to("1843.53"))
        assert_that(data22.x0, equal_to("1795.10"))
        assert_that(data22.y0, equal_to("1845.71"))
        assert_that(data22.ra, equal_to("26.6311063"))
        assert_that(data22.dec, equal_to("29.1102185"))

    def test_parse_source_readings_have_observations(self):
        obs0 = self.astrom_data.observations[0]
        obs1 = self.astrom_data.observations[1]
        obs2 = self.astrom_data.observations[2]

        source0 = self.astrom_data.sources[0]
        assert_that(source0[0].obs, same_instance(obs0))
        assert_that(source0[1].obs, same_instance(obs1))
        assert_that(source0[2].obs, same_instance(obs2))

        source1 = self.astrom_data.sources[1]
        assert_that(source1[0].obs, same_instance(obs0))
        assert_that(source1[1].obs, same_instance(obs1))
        assert_that(source1[2].obs, same_instance(obs2))

        source2 = self.astrom_data.sources[2]
        assert_that(source2[0].obs, same_instance(obs0))
        assert_that(source2[1].obs, same_instance(obs1))
        assert_that(source2[2].obs, same_instance(obs2))


if __name__ == '__main__':
    unittest.main()

__author__ = "David Rusk <drusk@uvic.ca>"

import tempfile
import unittest

from hamcrest import (assert_that, equal_to, has_length, has_entries,
                      same_instance, contains, close_to)

from tests.base_tests import FileReadingTestCase
from ossos import astrom
from ossos.astrom import (AstromParser, StreamingAstromWriter, Observation,
                             BaseAstromWriter, BulkAstromWriter, SourceReading)

TEST_FILE_1 = "data/1584431p15.measure3.cands.astrom"
TEST_FILE_2 = "data/1616681p22.measure3.cands.astrom"
FK_FILE = "data/fk1616682s00.measure3.cands.astrom"


class ParserTest(FileReadingTestCase):
    def setUp(self):
        self.parser = AstromParser()

    def parse(self, filename):
        return self.parser.parse(self.get_abs_path(filename))

    def test_parse_observation_rawnames(self):
        astrom_data = self.parse(TEST_FILE_1)

        assert_that(astrom_data.observations, has_length(3))
        assert_that(astrom_data.observations[0].rawname,
                    equal_to("1584431p15"))
        assert_that(astrom_data.observations[1].rawname,
                    equal_to("1584449p15"))
        assert_that(astrom_data.observations[2].rawname,
                    equal_to("1584453p15"))

    def test_parse_observation_basic_details(self):
        astrom_data = self.parse(TEST_FILE_1)

        obs0 = astrom_data.observations[0]
        obs1 = astrom_data.observations[1]
        obs2 = astrom_data.observations[2]

        assert_that(obs0.expnum, equal_to("1584431"))
        assert_that(obs0.ftype, equal_to("p"))
        assert_that(obs0.ccdnum, equal_to("15"))
        assert_that(obs0.is_fake(), equal_to(False))

        assert_that(obs1.expnum, equal_to("1584449"))
        assert_that(obs1.ftype, equal_to("p"))
        assert_that(obs1.ccdnum, equal_to("15"))
        assert_that(obs1.is_fake(), equal_to(False))

        assert_that(obs2.expnum, equal_to("1584453"))
        assert_that(obs2.ftype, equal_to("p"))
        assert_that(obs2.ccdnum, equal_to("15"))
        assert_that(obs2.is_fake(), equal_to(False))

    def test_parse_observation_headers(self):
        astrom_data = self.parse(TEST_FILE_1)

        obs0 = astrom_data.observations[0]
        obs1 = astrom_data.observations[1]
        obs2 = astrom_data.observations[2]

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
        astrom_data = self.parse(TEST_FILE_1)

        assert_that(astrom_data.sys_header, has_length(4))
        assert_that(astrom_data.sys_header, has_entries(
            {"RMIN": "0.5",
             "RMAX": "10.3",
             "ANGLE": "-19.9",
             "AWIDTH": "22.3"}
        ))

    def test_parse_sources(self):
        astrom_data = self.parse(TEST_FILE_1)

        assert_that(astrom_data.sources, has_length(3))

        ## Test source 0
        source0 = astrom_data.sources[0]
        assert_that(source0.num_readings(), equal_to(3))

        # Source 0 reading 0
        data00 = source0.get_reading(0)
        assert_that(data00.x, equal_to(911.00))
        assert_that(data00.y, equal_to(3967.12))
        assert_that(data00.x0, equal_to(911.00))
        assert_that(data00.y0, equal_to(3967.12))
        assert_that(data00.ra, equal_to(26.6833367))
        assert_that(data00.dec, equal_to(29.2203532))
        assert_that(data00.xref, equal_to(911.00))
        assert_that(data00.yref, equal_to(3967.12))

        # Source 0 reading 1
        data01 = source0.get_reading(1)
        assert_that(data01.x, equal_to(944.25))
        assert_that(data01.y, equal_to(3964.03))
        assert_that(data01.x0, equal_to(938.93))
        assert_that(data01.y0, equal_to(3965.78))
        assert_that(data01.ra, equal_to(26.6816808))
        assert_that(data01.dec, equal_to(29.2202748))
        assert_that(data01.xref, equal_to(911.00))
        assert_that(data01.yref, equal_to(3967.12))

        # Source 0 reading 2
        data02 = source0.get_reading(2)
        assert_that(data02.x, equal_to(949.76))
        assert_that(data02.y, equal_to(3963.12))
        assert_that(data02.x0, equal_to(943.91))
        assert_that(data02.y0, equal_to(3965.20))
        assert_that(data02.ra, equal_to(26.6813840))
        assert_that(data02.dec, equal_to(29.2202469))
        assert_that(data02.xref, equal_to(911.00))
        assert_that(data02.yref, equal_to(3967.12))

        ## Test source 1
        assert_that(astrom_data.sources[1].num_readings(), equal_to(3))

        ## Test source 2
        source2 = astrom_data.sources[2]
        assert_that(source2.num_readings(), equal_to(3))

        # Source 2 reading 2
        data22 = source2.get_reading(2)
        assert_that(data22.x, equal_to(1800.48))
        assert_that(data22.y, equal_to(1843.53))
        assert_that(data22.x0, equal_to(1795.10))
        assert_that(data22.y0, equal_to(1845.71))
        assert_that(data22.ra, equal_to(26.6311063))
        assert_that(data22.dec, equal_to(29.1102185))
        assert_that(data22.xref, equal_to(1698.04))
        assert_that(data22.yref, equal_to(1842.46))

    def test_parse_source_readings_have_observations(self):
        astrom_data = self.parse(TEST_FILE_1)

        obs0 = astrom_data.observations[0]
        obs1 = astrom_data.observations[1]
        obs2 = astrom_data.observations[2]

        source0 = astrom_data.sources[0]
        assert_that(source0.get_reading(0).obs, same_instance(obs0))
        assert_that(source0.get_reading(1).obs, same_instance(obs1))
        assert_that(source0.get_reading(2).obs, same_instance(obs2))

        source1 = astrom_data.sources[1]
        assert_that(source1.get_reading(0).obs, same_instance(obs0))
        assert_that(source1.get_reading(1).obs, same_instance(obs1))
        assert_that(source1.get_reading(2).obs, same_instance(obs2))

        source2 = astrom_data.sources[2]
        assert_that(source2.get_reading(0).obs, same_instance(obs0))
        assert_that(source2.get_reading(1).obs, same_instance(obs1))
        assert_that(source2.get_reading(2).obs, same_instance(obs2))

    def test_parse_file2_had_neg_crval2(self):
        astrom_data = self.parse(TEST_FILE_2)

        assert_that(astrom_data.sources, has_length(1))
        assert_that(astrom_data.observations, has_length(3))

        obs_names = [obs.rawname for obs in astrom_data.observations]
        assert_that(obs_names, contains("1616681p22", "1616692p22", "1616703p22"))

    def test_parse_source_reference_point(self):
        astrom_data = self.parse(TEST_FILE_2)
        source = astrom_data.get_sources()[0]

        reading0 = source.get_reading(0)
        reading1 = source.get_reading(1)
        reading2 = source.get_reading(2)

        delta = 0.00000001
        assert_that(reading0.reference_source_point[0], close_to(560.06, delta))
        assert_that(reading0.reference_source_point[1], close_to(406.51, delta))
        assert_that(reading1.reference_source_point[0], close_to(562.82, delta))
        assert_that(reading1.reference_source_point[1], close_to(406.68, delta))
        assert_that(reading2.reference_source_point[0], close_to(564.44, delta))
        assert_that(reading2.reference_source_point[1], close_to(406.03, delta))

    def test_reading_coordinate_offsets(self):
        astrom_data = self.parse(TEST_FILE_2)

        source = astrom_data.get_sources()[0]

        reading0 = source.get_reading(0)
        reading1 = source.get_reading(1)
        reading2 = source.get_reading(2)

        delta = 0.00000001
        assert_that(reading0.get_coordinate_offset(reading0)[0], close_to(0, delta))
        assert_that(reading0.get_coordinate_offset(reading0)[1], close_to(0, delta))
        assert_that(reading0.get_coordinate_offset(reading1)[0], close_to(-2.76, delta))
        assert_that(reading0.get_coordinate_offset(reading1)[1], close_to(-0.17, delta))
        assert_that(reading0.get_coordinate_offset(reading2)[0], close_to(-4.38, delta))
        assert_that(reading0.get_coordinate_offset(reading2)[1], close_to(0.48, delta))

    def test_parse_fake_file(self):
        astrom_data = self.parse(FK_FILE)

        assert_that(astrom_data.observations, has_length(3))
        assert_that(astrom_data.get_sources(), has_length(21))

        obs0 = astrom_data.observations[0]

        assert_that(obs0.rawname, equal_to("fk1616682s00"))
        assert_that(obs0.expnum, equal_to("1616682"))
        assert_that(obs0.ftype, equal_to("s"))
        assert_that(obs0.ccdnum, equal_to("00"))
        assert_that(obs0.is_fake(), equal_to(True))


class GeneralAstromWriterTest(FileReadingTestCase):
    def setUp(self):
        self.parser = AstromParser()
        self.outputfile = tempfile.NamedTemporaryFile(suffix=".astrom",
                                                      mode="a+b")

    def tearDown(self):
        self.outputfile.close()

    def read_output(self):
        self.outputfile.seek(0)
        return self.outputfile.read()

    def parse(self, filename=TEST_FILE_1):
        return AstromParser().parse(self.get_abs_path(filename))


class BaseAstromWriterTest(GeneralAstromWriterTest):
    def setUp(self):
        super(BaseAstromWriterTest, self).setUp()
        self.writer = BaseAstromWriter(self.outputfile)

    def test_write_observation_list(self):
        expected = ("# 1584431p15                                                                    \n"
                    "# 1584449p15                                                                    \n"
                    "# 1584453p15                                                                    \n"
        )

        observations = [Observation("1584431", "p", "15"),
                        Observation("1584449", "p", "15"),
                        Observation("1584453", "p", "15")]

        self.writer._write_observation_list(observations)

        assert_that(self.read_output(), equal_to(expected))

    def test_write_observation_headers(self):
        expected = ("## MOPversion                                                                   \n"
                    "#  1.20                                                                         \n"
                    "## MJD-OBS-CENTER  EXPTIME THRES FWHM  MAXCOUNT CRVAL1     CRVAL2     EXPNUM    \n"
                    "# 2012 10 21.40516  320.14  2.70  2.90  30000.0   26.92871   29.01125  1584431  \n"
                    "## SCALE CHIP CRPIX1    CRPIX2    NAX1  NAX2   DETECTOR           PHADU RDNOIS  \n"
                    "#  0.185  16  -3227.00    -75.96  2112  4644 MegaPrime            1.60  3.00    \n"
                    "## MOPversion                                                                   \n"
                    "#  1.20                                                                         \n"
                    "## MJD-OBS-CENTER  EXPTIME THRES FWHM  MAXCOUNT CRVAL1     CRVAL2     EXPNUM    \n"
                    "# 2012 10 21.48212  320.15  2.70  3.30  30000.0   26.92871   29.01128  1584449  \n"
                    "## SCALE CHIP CRPIX1    CRPIX2    NAX1  NAX2   DETECTOR           PHADU RDNOIS  \n"
                    "#  0.185  16  -3222.28    -76.24  2112  4644 MegaPrime            1.60  3.00    \n"
                    "## MOPversion                                                                   \n"
                    "#  1.20                                                                         \n"
                    "## MJD-OBS-CENTER  EXPTIME THRES FWHM  MAXCOUNT CRVAL1     CRVAL2     EXPNUM    \n"
                    "# 2012 10 21.49934  320.15  2.70  3.40  30000.0   26.92871   29.01128  1584453  \n"
                    "## SCALE CHIP CRPIX1    CRPIX2    NAX1  NAX2   DETECTOR           PHADU RDNOIS  \n"
                    "#  0.185  16  -3221.49    -76.45  2112  4644 MegaPrime            1.60  3.00    \n")

        astrom_data = self.parse(TEST_FILE_1)

        self.writer._write_observation_headers(astrom_data.observations)

        assert_that(self.read_output(), equal_to(expected))

    def test_write_sys_header(self):
        expected = ("##     RMIN    RMAX   ANGLE   AWIDTH                                            \n"
                    "#      0.5    10.3   -19.9    22.3                                              \n"
        )

        astrom_data = self.parse(TEST_FILE_1)

        self.writer._write_sys_header(astrom_data.sys_header)

        assert_that(self.read_output(), equal_to(expected))

    def test_write_sources(self):
        expected = ("\n"
                    "   911.00  3967.12   911.00  3967.12   26.6833367   29.2203532\n"
                    "   944.25  3964.03   938.93  3965.78   26.6816808   29.2202748\n"
                    "   949.76  3963.12   943.91  3965.20   26.6813840   29.2202469\n"
                    "\n"
                    "   925.73  3967.76   925.73  3967.76   26.6824630   29.2203830\n"
                    "   944.25  3964.03   938.93  3965.78   26.6816808   29.2202748\n"
                    "   949.76  3963.12   943.91  3965.20   26.6813840   29.2202469\n"
                    "\n"
                    "  1698.04  1842.46  1698.04  1842.46   26.6368529   29.1100700\n"
                    "  1780.20  1843.71  1775.28  1845.56   26.6322821   29.1102127\n"
                    "  1800.48  1843.53  1795.10  1845.71   26.6311063   29.1102185\n"
        )

        astrom_data = self.parse(TEST_FILE_1)

        self.writer._write_source_data(astrom_data.sources)

        assert_that(self.read_output(), equal_to(expected))


class BulkAstromWriterTest(GeneralAstromWriterTest):
    def setUp(self):
        super(BulkAstromWriterTest, self).setUp()
        self.writer = BulkAstromWriter(self.outputfile)

    def test_parse_then_rewrite(self):
        """
        Sanity check that we can parse data, then write that data back out
        identically.
        """
        astrom_data = self.parse(TEST_FILE_1)
        self.writer.write_astrom_data(astrom_data)

        actual = self.read_output()

        with open(self.get_abs_path(TEST_FILE_1), "rb") as fh:
            expected = fh.read()

        assert_that(actual, equal_to(expected))


class StreamingAstromWriterTest(GeneralAstromWriterTest):
    def setUp(self):
        super(StreamingAstromWriterTest, self).setUp()
        self.sys_header = {astrom.RMIN: 0.5, astrom.RMAX: 10.3,
                           astrom.ANGLE: -19.9, astrom.AWIDTH: 22.3}
        self.writer = StreamingAstromWriter(self.outputfile, self.sys_header)

    def test_write_source(self):
        with open(self.get_abs_path(TEST_FILE_1), "rb") as fh:
            expected_lines = fh.readlines()

        def get_expected(num_lines):
            return "".join(expected_lines[:num_lines])

        astrom_data = self.parse(TEST_FILE_1)

        source1 = astrom_data.sources[0]
        source2 = astrom_data.sources[1]
        source3 = astrom_data.sources[2]

        assert_that(self.read_output(), equal_to(""))
        self.writer.write_source(source1)
        assert_that(self.read_output(), equal_to(get_expected(28)))
        self.writer.write_source(source2)
        assert_that(self.read_output(), equal_to(get_expected(32)))
        self.writer.write_source(source3)
        assert_that(self.read_output(), equal_to(get_expected(37)))

    def test_continue_writing_to_existing_file(self):
        with open(self.get_abs_path(TEST_FILE_1), "rb") as fh:
            expected_lines = fh.readlines()

        def get_expected(num_lines):
            return "".join(expected_lines[:num_lines])

        astrom_data = self.parse(TEST_FILE_1)
        source1 = astrom_data.sources[0]
        source2 = astrom_data.sources[1]

        assert_that(self.read_output(), equal_to(""))
        self.writer.write_source(source1)
        assert_that(self.read_output(), equal_to(get_expected(28)))

        writer2 = StreamingAstromWriter(self.outputfile, self.sys_header)
        writer2.write_source(source2)
        assert_that(self.read_output(), equal_to(get_expected(32)))


class AstromDataTest(FileReadingTestCase):
    def test_get_reading_count(self):
        astrom_data = AstromParser().parse(self.get_abs_path(TEST_FILE_1))
        assert_that(astrom_data.get_reading_count(), equal_to(9))


class URIResolvingTest(unittest.TestCase):
    def test_resolve_image_uri(self):
        observation = Observation("1584431", "p", "15")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p.fits"
        assert_that(observation.get_image_uri(), equal_to(expected_uri))

    def test_resolve_apcor_uri(self):
        observation = Observation("1616681", "p", "22")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd22/1616681p22.apcor"
        assert_that(observation.get_apcor_uri(), equal_to(expected_uri))

    def test_resolve_apcor_uri_single_digit_ccd(self):
        """Just double checking we don't run into trouble with leading zeros"""
        observation = Observation("1616681", "p", "05")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616681/ccd05/1616681p05.apcor"
        assert_that(observation.get_apcor_uri(), equal_to(expected_uri))

    def test_resolve_fake_image_uri(self):
        observation = Observation("1616682", "s", "24", fk="fk")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616682/ccd24/fk1616682s24.fits"
        assert_that(observation.get_image_uri(), equal_to(expected_uri))

    def test_resolve_fake_apcor_uri(self):
        observation = Observation("1616682", "s", "24", fk="fk")
        expected_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1616682/ccd24/fk1616682s24.apcor"
        assert_that(observation.get_apcor_uri(), equal_to(expected_uri))

    def test_resolve_uris_from_reading(self):
        observation = Observation("1584431", "p", "15")
        # 0's for don't cares
        reading = SourceReading(0, 0, 0, 0, 0, 0, 0, 0, observation)

        expected_image_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/1584431p.fits"
        expected_apcor_uri = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/1584431/ccd15/1584431p15.apcor"

        assert_that(reading.get_image_uri(), equal_to(expected_image_uri))
        assert_that(reading.get_apcor_uri(), equal_to(expected_apcor_uri))


if __name__ == '__main__':
    unittest.main()

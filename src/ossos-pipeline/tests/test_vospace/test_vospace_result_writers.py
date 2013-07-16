__author__ = "David Rusk <drusk@uvic.ca>"

import os
import unittest

from hamcrest import assert_that, equal_to

from tests.base_tests import FileReadingTestCase
from ossos import storage, astrom
from ossos.gui.context import VOSpaceWorkingContext
from ossos.mpc import MPCWriter
from ossos.astrom import StreamingAstromWriter

BASE_TEST_DIR = "vos:drusk/OSSOS/tests/"
CURRENT_TEST_DIR = BASE_TEST_DIR + "writer_tests"

TEST_FILE_1 = "file1"


class AbstractVOSpaceWriterTest(FileReadingTestCase):
    def setUp(self):
        self.context = VOSpaceWorkingContext(CURRENT_TEST_DIR)
        self.filehandle = self.context.open(TEST_FILE_1)

    def read_vofile(self, filename):
        vofile = storage.vofile(self.context.get_full_path(filename),
                                os.O_RDONLY)
        contents = vofile.read()
        vofile.close()
        return contents

    def tearDown(self):
        if self.filehandle is None:
            return

        os.remove(self.filehandle.get_local_filename())

        self._clean_vospace()

    def _clean_vospace(self):
        for filename in self.context.listdir():
            storage.delete_uri(self.context.get_full_path(filename))


class VOSpaceMPCWriterTest(AbstractVOSpaceWriterTest):
    def setUp(self):
        super(VOSpaceMPCWriterTest, self).setUp()

        self.writer = MPCWriter(self.filehandle, auto_flush=False)

    def test_write_results(self):
        self.writer.write_mpc_line("12345",
                                   "A234567",
                                   "*",
                                   "M",
                                   "N",
                                   "2012 10 21.405160",
                                   "26.683336700", # 01 46 44.001
                                   "29.220353200", # +29 13 13.27
                                   "123.5",
                                   "A",
                                   "523")

        expected = "12345A234567*MN2012 10 21.40516001 46 44.001+29 13 13.27         123.5A      523\n"

        assert_that(self.filehandle.read(), equal_to(""))

        self.writer.flush()

        assert_that(self.filehandle.read(), equal_to(expected))

        assert_that(self.read_vofile(TEST_FILE_1), equal_to(expected))


class VOSpaceAstromWriterTest(AbstractVOSpaceWriterTest):
    def setUp(self):
        super(VOSpaceAstromWriterTest, self).setUp()

        self.astrom_file = "data/1584431p15.measure3.cands.astrom"
        self.astrom_data = astrom.AstromParser().parse(
            self.get_abs_path(self.astrom_file))

        self.writer = StreamingAstromWriter(self.filehandle, self.astrom_data.sys_header)

    def test_write_results(self):
        with open(self.get_abs_path(self.astrom_file), "rb") as fh:
            expected_lines = fh.readlines()

        def get_expected(num_lines):
            return "".join(expected_lines[:num_lines])

        source1 = self.astrom_data.sources[0]

        self.writer.write_source(source1)
        expected = get_expected(28)

        assert_that(self.filehandle.read(), equal_to(expected))

        self.writer.close()

        assert_that(self.read_vofile(TEST_FILE_1), equal_to(expected))


if __name__ == '__main__':
    unittest.main()

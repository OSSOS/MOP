from unittest import TestCase
from ossos import mop_file
from ossos import match

class TestMOPHeader(TestCase):

    def test_parser_astrom(self):
        filename = "data/fk1616682s00.measure3.cands.astrom"
        mopfile = mop_file.MOPFile(filename=filename, subfmt='jmp')
        self.assertIsInstance(mopfile, mop_file.MOPFile)
        self.assertAlmostEqual(mopfile.data["R.A._fk1616693s00"][3], 214.3292916)
        self.assertEqual(mopfile.header.file_ids[0], 'fk1616682s00')
        self.assertEqual(mopfile.header.keywords['ANGLE'][0], '20.0')

    def test_parser_comb(self):
        filename = "data/15BS+1+1_p39.cands.comb"
        mopfile = mop_file.MOPFile(filename=filename, subfmt='jmp')
        self.assertIsInstance(mopfile, mop_file.MOPFile)
        self.assertAlmostEqual(mopfile.data["FLUX_1832046p39"][1], 694.72)
        self.assertEqual(mopfile.header.file_ids[0], '1832036p39')
        self.assertEqual(mopfile.header.keywords['ANGLE'][0], '-23.0')

    def test_match(self):
        real_detections = mop_file.MOPFile(filename="data/15BS+1+1_p37.measure3.reals.astrom")
        cand_detections = mop_file.MOPFile(filename="data/15BS+1+1_p37.cands.comb")
        match_mopfile = match.match_mopfiles(cand_detections, real_detections)
        self.assertEqual(match_mopfile.data['real'][0], 0)
        self.assertEqual(match_mopfile.data['real'][1], -1)

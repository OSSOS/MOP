import astropy

__author__ = 'jjk'
import unittest
from ossos import ssos
from astropy.time import Time

class SSOSTest(unittest.TestCase):

    def test_query_with_inputlist(self):
        mpc_lines=("     HL7j2    C2013 04 03.62926 17 12 01.16 +04 13 33.3          24.1 R      568",
                   "     HL7j2    C2013 04 04.58296 17 11 59.80 +04 14 05.5          24.0 R      568",
                   "     HL7j2    C2013 05 03.52252 17 10 38.28 +04 28 00.9          23.4 R      568",
                   "     HL7j2    C2013 05 08.56725 17 10 17.39 +04 29 47.8          23.4 R      568")


        params = ssos.ParamDictBuilder(mpc_lines).params
        print params['obs']
        self.assertEquals(params['obs'].split('\n')[3].strip('\n'),
                          mpc_lines[3])

        query = ssos.Query(mpc_lines,
                           search_start_date=Time('2007-01-01', scale='utc'),
                           search_end_date=Time('2008-08-07', scale='utc'))
        query.param_dict_biulder.verbose=True
        table = query.get_table()

        print table.colnames



if __name__ == '__main__':
    unittest.main()

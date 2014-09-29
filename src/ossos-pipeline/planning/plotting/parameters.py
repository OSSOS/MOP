__author__ = 'Michele Bannister   git:@mtbannister'

import os

MPCORB_FILE = os.path.join(os.getenv('HOME', '/Users/michele/'), 'MPCORB-Distant.dat')
L7MODEL = '/Users/michele/Dropbox/OSSOS/L7model-3.0-9.0'  # 'vos:OSSOS/CFEPS/L7SyntheticModel-v09.txt'
REAL_KBO_AST_DIR = '/Users/michele/Dropbox/OSSOS/measure3/ossin/'
MOST_RECENT_OSSOS_RELEASE = '/Users/michele/Dropbox/OSSOS/Release_summaries/ossos23sept2014.txt'
PLOT_FIELD_EPOCH = 'Jun14.00'  # Jun14.00 ==> '0' days since the New Moon on Jun14

PLOT_USNO_STARS = True
PLOT_MEGACAM_ARCHIVE_FIELDS = False  # # TODO Make this work when True
PLOT_SYNTHETIC_KBOS = False
PLOT_SYNTHETIC_KBO_TRAILS = False
PLOT_REAL_KBOS = True and os.access(REAL_KBO_AST_DIR, os.F_OK)
PLOT_FIELD_LAYOUT = True

PLOT_MPCORB = True and os.access(MPCORB_FILE, os.F_OK)

# ORIGINAL: DON't USE, does not match the location of the data as pointed & taken!
# Ablocks={'13AE': {"RA": "14:32:30.29","DEC":"-13:54:01.4"},
# '13AO': {"RA": "16:17:04.41","DEC":"-13:14:45.8"}}
BLOCKS = {
    '13AE': {"RA": "14:15:28.89", "DEC": "-12:32:28.4"},
    # E+0+0: image 1616681, ccd21 on April 9. E block discovery triplets are April 4,9,few 19
    '13AO': {"RA": "15:58:01.35", "DEC": "-12:19:54.2"},  # O+0+0: image 1625346, ccd21 on May 8. O block are May 7,8.
    '13BL': {'RA': "00:54:00.00", "DEC": "+03:50:00.00"},  # 13B blocks are at their opposition locations
    '13BH': {'RA': "01:30:00.00", "DEC": "+13:00:00.00"},  # due to bad weather, discovery wasn't until Dec/Jan
    '15AM': {'RA': "15:30:00.00", "DEC": "-12:20:00.0"}  # at its 2014 initial observation
}

DISCOVERY_NEW_MOON = 'Oct13'  # applies to the 13B blocks

NEWMOONS = {'Feb13': "2013/02/10 10:00:00",
            'Mar13': "2013/03/11 10:00:00",
            'Apr13': "2013/04/10 10:00:00",
            'May13': "2013/05/09 10:00:00",
            'Jun13': "2013/06/08 10:00:00",
            'Jul13': "2013/07/08 10:00:00",
            'Aug13': "2013/08/06 10:00:00",
            'Sep13': '2013/09/05 10:00:00',
            'Oct13': '2013/10/04 10:00:00',
            'Nov13': '2013/11/03 10:00:00',
            'Dec13': '2013/12/02 10:00:00',
            'Jan14': '2014/01/01 10:00:00',
            'Feb14': '2014/01/31 10:00:00',
            'Mar14': '2014/03/28 10:00:00',
            'Apr14': '2014/04/01 10:00:00',
            'May14': '2014/05/28 10:00:00',
            'Jun14': '2014/06/26 10:00:00',
            'Jul14': '2014/07/26 10:00:00',
            'Aug14': "2014/08/25 10:00:00",
            'Sep14': '2014/09/24 10:00:00',
            'Oct14': '2014/10/23 10:00:00',
            'Nov14': '2014/11/22 10:00:00',
            'Dec14': '2014/12/22 10:00:00',
}


class tno(object):
    def __init__(self):
        self.classification = None
        self.name = None
        self.mag = None
        self.mag_stdev = None
        self.dist = None
        self.dist_e = None
        self.nobs = None
        self.arclen = None
        self.a = None
        self.a_e = None
        self.e = None
        self.e_e = None
        self.i = None
        self.i_e = None
        self.peri = None
        self.diameter = None

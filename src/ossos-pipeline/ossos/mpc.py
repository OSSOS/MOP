__author__ = 'jjk, mtb55'

import mp_ephem
from mp_ephem.ephem import *

MPCReader = EphemerisReader
Observation = mp_ephem.Observation


mp_ephem.DEFAULT_OBSERVERS = ['M. T. Bannister',
                     'J. J. Kavelaars',
                     'B. J. Gladman',
                     'J.-M. Petit',
                     'T. Burdullis'
                     ]
mp_ephem.DEFAULT_MEASURERS = ['S. D. J. Gwyn',
                     'Y.-T. Chen.'
                     ]
mp_ephem.DEFAULT_TELESCOPE = "CFHT 3.6m + CCD"
mp_ephem.DEFAULT_ASTROMETRIC_NETWORK = "UCAC4"



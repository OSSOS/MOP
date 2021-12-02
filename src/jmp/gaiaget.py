#! /usr/bin/env python3
"""
J.-M. Petit (CNRS / UTINAM) 2021-11-27
Query I/345/gaia2

gaiaget.py [-h|--help] [-m max] [-l|--low mag] ra dec width height epoch

  -m <n>: max number of lines in output
  -l|--low <mag>: lowest magnitude to report. Default = 15

  ra    : RA of cenral location, in decimal degree
  dec   : DEC of central position, in decimal degree
  width : width of box, in degree. Max value is 1 degree
  height: height of box, in degree. Max value is 1 degree
  epoch : epoch at which to propagate the positions, ISOT format [YYYY-MM-DDTHH:MN:SS.sss]
"""

import os, sys
import getopt
try:
    import numpy as np
except:
    sys.stderr.write("(error) needs module numpy in PYTHONPATH\n")
    sys.exit(1)

try:
    import astropy.units as u
    from astropy.coordinates import SkyCoord
    from astropy.time import Time
except:
    sys.stderr.write("(error) needs module astropy in PYTHONPATH\n")
    sys.exit(1)

try:
    from astroquery.gaia import Gaia
except:
    sys.stderr.write("(error) needs module astroquery in PYTHONPATH\n")
    sys.exit(1)

def GetGaiaPositions(ra, dec, w, h, epoch, limit=100):
    """
Query Gaia DR2 catalog and returns positions, proper motion and g_mag of stars
in box of s width x height around central position (ra, dec).
Positions are propagated to time epoch (decimal calendar year.).
Reference frame for position is 'ICRS'.

    :param ra: Right Ascension in decimal degree
    :param dec: Declination in decimal degree
    :param w: width of search box, in decimal degree
    :param h: height of search box, in decimal degree
    :param limit: maximum number of rows to return (-1 for unlimited)

    :return: rr: table of positions, g_mag and id
    """
    Gaia.ROW_LIMIT = limit
    coord = SkyCoord(ra=ra, dec=dec, unit=(u.degree, u.degree), frame='icrs')
    width = u.Quantity(w, u.deg)
    height = u.Quantity(h, u.deg)
    #r = Gaia.query_object_async(coordinate=coord, width=width, height=height)
    #rr = r.copy()
    #rr.keep_columns(['designation','ref_epoch','ra','dec','pmra','pmdec','phot_g_mean_mag'])
    rr = Gaia.query_object_async(coordinate=coord, width=width, height=height, columns=['designation','ref_epoch','ra','dec','pmra','pmdec','phot_g_mean_mag'], verbose=False)
    # Here, we must propagate the position to epoch
    if True:
        for i in range(len(rr['ra'])):
            dt = epoch - rr['ref_epoch'][i]
            if not rr['pmra'].mask[i]:
                dec = rr['dec'][i]
                cd = np.cos(dec/180.*3.14159265389793828)
                rr['ra'][i] += rr['pmra'][i]*dt/(3600000.0*cd)
            if not rr['pmdec'].mask[i]:
                rr['dec'][i] += rr['pmdec'][i]*dt/3600000.0
    else:
        print('WARNING: gaiaget.py is currently not propagating positions to epoch {:11.6f},'.format(epoch))
        print('Positions are given for epoch {:11.6f}.'.format(rr['ref_epoch'][0]))
        
    return rr

if __name__ == "__main__":
    __limit = -1
    __lowmag = 15

    __options = ('help', 'low=')
    try:
        __opts, __args = getopt.getopt(sys.argv[1:], 'hl:m:', __options)
    except:
        help("__main__")
        sys.exit(1)

    for __o, __a in __opts:
        if __o in ("-h", "--help"):
            help("__main__")
            sys.exit(1)

        elif __o in ("-l", "--low"):
            try:
                __lowmag = float(__a)
            except:
                sys.stderr.write("(error) wrong magnitude format\n")

        elif __o == "-m":
            try:
                __limit = int(__a)
            except:
                sys.stderr.write("(error) wrong limit/max format\n")

    ra, dec, w, h = list(map(float, __args[:-1]))
    w = min(w, 1.)
    h = min(h, 1.)
    epoch = Time(__args[-1], format='isot', scale='utc')

    rr = GetGaiaPositions(ra, dec, w, h, epoch.decimalyear, __limit)
    #for k in rr.colnames:
    #    print(k, type(rr[k][0]))

    filename = 'gaia.ccmap_{:010.6f}_{:010.6f}_{:5.3f}_{:5.3f}_{:s}'.format(ra, dec, w, h, epoch.isot.replace(':', '-'))
    fo = open(filename, 'w')
    for i in range(len(rr['ra'])):
        if (rr['phot_g_mean_mag'][i] >= __lowmag):
            fo.write('{:10.6f} {:10.6f} {:5.2f} {:s}\n'.format(rr['ra'][i], rr['dec'][i], rr['phot_g_mean_mag'][i], rr['designation'][i].replace(' ', '_')))
    fo.close()

    try:
        os.stat('gaia.ccmap', follow_symlinks=False)
        os.remove('gaia.ccmap')
    except OSError:
        pass
    finally:
        os.symlink(filename, 'gaia.ccmap')

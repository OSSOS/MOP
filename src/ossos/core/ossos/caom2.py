from ossos import mpc, storage, orbfit
from ossos import wcs
import sys
import numpy
from astropy.table import Table, Column


def ephem_search(mpc_filename, search_date="2014 07 24.0"):
    """
    builds a TSV file in the format of SSOIS by querying for possible observations in CADC/CAOM2.

    This is a fall back program, should only be useful when SSOIS is behind.
    """
    columns = ('Image',
               'Ext',
               'X',
                'Y',
                'MJD',
                'Filter',
                'Exptime',
                'Object_RA',
                'Object_Dec',
                'Image_target',
                'Telescope/Instrument',
                'MetaData',
                'Datalink')

    ephem_table = Table(names=columns,
                         dtypes=('S10', 'i4', 'f8', 'f8',
                                 'f8', 'S10', 'f8', 'f8', 'f8', 'S20', 'S20', 'S20', 'S50'))

    ephem_table.pprint()

    o = orbfit.Orbfit(mpc. MPCReader(mpc_filename).mpc_observations)
    o.predict(search_date)
    fields = storage.cone_search(o.coordinate.ra.degrees, o.coordinate.dec.degrees, dra=0.3, ddec=0.3,
                                 calibration_level=1)
    mjdates = numpy.unique(fields['mjdate'])

    collectionIDs = []
    for mjdate in mjdates:
        jd = 2400000.5 + mjdate
        o.predict(jd)
        for field in storage.cone_search(o.coordinate.ra.degrees, o.coordinate.dec.degrees,
                                         dra=30./3600.0, ddec=30./3600.0,
                                         mjdate=mjdate, calibration_level=1):
            collectionIDs.append(field['collectionID'])

    expnums = numpy.unique(numpy.array(collectionIDs))

    for expnum in expnums:
        header = storage.get_astheader(expnum, 22)
        o.predict(header['MJDATE']+2400000.5)
        print(o.time.iso, o.coordinate.ra.degrees, o.coordinate.dec.degrees)
        for ccd in range(36):
            header = storage.get_astheader(expnum, ccd)
            w = wcs.WCS(header)
            (x, y) = w.sky2xy(o.coordinate.ra.degrees, o.coordinate.dec.degrees)
            print(ccd, x, y)
            if 0 < x < header['NAXIS1'] and 0 < y < header['NAXIS2']:
                ephem_table.add_row([expnum, ccd+1, x, y,
                                     header['MJDATE'], header['FILTER'], header['EXPTIME'],
                                     o.coordinate.ra.degrees, o.coordinate.dec.degrees,
                                     header['OBJECT'],
                                     'CFHT/MegaCam',
                                     None,
                                     "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHT/{}p[{}]".format(expnum, ccd)])
                break

    ephem_table.pprint()
    ephem_table.write('backdoor.tsv', format='ascii', delimiter='\t')


if __name__ == '__main__':
    ephem_search(sys.argv[0])

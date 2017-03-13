"""Mark the stationary sources in a given source catalog by matching with other source catalogs"""
import sys

from ossos import storage
from ossos import util
from astropy.io import fits
import numpy
import argparse
import logging
import os

task="stationary"
dependency = None

def add_column(hdu, column_name, column_format, data):
    """

    @param hdu:  The FITS Binary table to add the column to
    @param column_name: The name of the Column to add
    @param column_format: The data format for the column
    @param data: The data for the column
    @return: fits.BinTableHDU
    """
    orig_cols = hdu.data.columns
    new_cols = fits.ColDefs([fits.Column(name=column_name, format=column_format, array=data),])
    new_hdu = fits.BinTableHDU.from_columns(orig_cols + new_cols, name=hdu.name)
    return new_hdu


def run(expnum, ccd, prefix, version, dry_run, force):
    """
    Retrieve the catalog from VOSspace, find the matching expnum/ccd combos and match against those.

    @param expnum: exposure number to retrieve for match
    @param ccd: chip to retrieve for matching
    @return:
    """
    message = storage.SUCCESS

    if storage.get_status(task, prefix, expnum, version=version, ccd=ccd) and not force:
        logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, version, ccd))
        return

    with storage.LoggingManager(task, prefix, expnum, ccd, version, dry_run):
        try:
            if dependency is not None and not storage.get_status(dependency, prefix, expnum, "p", ccd=ccd):
                raise IOError("{} not yet run for {}".format(dependency, expnum))

            # get catalog from the vospace storage area
            logging.info("Getting fits image from VOSpace")

            logging.info("Running match on %s %d" % (expnum, ccd))
            filename = match(expnum, ccd)

            if dry_run:
                return

            # place the results into VOSpace
            dest = storage.dbimages_uri(expnum, ccd, prefix=prefix, version=version, ext='.cat.fits')
            source = filename
            count = 0
            with open(source, 'r'):
                while True:
                    count += 1
                    try:
                        logging.info("Attempt {} to copy {} -> {}".format(count, source, dest))
                        storage.copy(source, dest)
                        break
                    except Exception as ex:
                        if count > 10:
                            raise ex
            logging.info(message)
        except Exception as e:
            message = str(e)
            logging.error(message)

        storage.set_status(task, prefix, expnum, version, ccd=ccd, status=message)


def match(expnum, ccd):

    match_list = storage.footprint_search(expnum, ccd)

    if len(match_list) == 0:
        raise LookupError("No cataloges to match {}p{:02d}.cat.fits against.".format(expnum, ccd))

    this_catalog = fits.open(storage.get_file(expnum, ccd, ext='.cat.fits'))
    # reshape the position vectors from the catalogues for use in match_lists
    p1 = numpy.transpose((this_catalog['OBJECTS'].data['X_WORLD'],
                          this_catalog['OBJECTS'].data['Y_WORLD']))
    matches = numpy.zeros(len(this_catalog['OBJECTS'].data['X_WORLD']))

    for match_set in match_list:
        logging.info("trying to match against catalog {}p{:02d}.cat.fits".format(match_set[0], match_set[1]))
        try:
            match_catalog = fits.open(storage.get_file(match_set[0], match_set[1], ext='.cat.fits'))
        except OSError as ioe:
            logging.debug(str(ioe))
            continue
        # reshape the position vectors from the catalogues for use in match_lists
        p2 = numpy.transpose((match_catalog['OBJECTS'].data['X_WORLD'],
                              match_catalog['OBJECTS'].data['Y_WORLD']))
        idx1, idx2 = util.match_lists(p1, p2, tolerance=1.0/3600.0)
        matches[idx2.data[~idx2.mask]] += 1

    this_catalog['OBJECTS'] = add_column(this_catalog['OBJECTS'], column_name='MATCHES', column_format='I', data=matches)
    this_catalog.writeto('junk.cat.fits')
    return 'junk.cat.fits'


def main():
    parser = argparse.ArgumentParser(
        description='Create a matches column in a source catalog to determine if a source is a stationary object.')

    parser.add_argument('--ccd', '-c',
                        action='store',
                        type=int,
                        dest='ccd',
                        default=None,
                        help='which ccd to process, default is all')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:cfis/solar_system/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="expnum(s) to process")
    parser.add_argument("--dry-run",
                        action="store_true",
                        help="DRY RUN, don't copy results to VOSpace, implies --force")
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--force", default=False,
                        action="store_true")
    parser.add_argument("--debug", "-d",
                        action="store_true")

    cmd_line = " ".join(sys.argv)
    args = parser.parse_args()

    util.set_logger(args)
    logging.info("Started {}".format(cmd_line))

    storage.DBIMAGES = args.dbimages
    prefix = ''
    version = 'p'

    exit_code = 0
    for expnum in args.expnum:
        if args.ccd is None:
           if int(expnum) < 1785619:
               # Last exposures with 36 CCD Megaprime
               ccdlist = range(0,36)
           else:
               # First exposrues with 40 CCD Megaprime
               ccdlist = range(0, 40)
        else:
           ccdlist = [args.ccd]
        for ccd in ccdlist:
            run(expnum, ccd, prefix, version, args.dry_run, args.force)
    return exit_code

if __name__ == '__main__':
    sys.exit(main())
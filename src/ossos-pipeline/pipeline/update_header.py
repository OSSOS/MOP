#!python
"""replace arg1 header with arg2s."""

import argparse
import logging
import os
import sys

from ossos import storage


def run_update_header(image_hdulist, header_hdulist):

    for hdu_idx in range(1, len(image_hdulist)):
        new_header = header_hdulist[hdu_idx]
        ## pull some data structure keywords out of the astrometric headers
        for key in ['NAXIS', 'XTENSION', 'PCOUNT', 'GCOUNT',
                    'NAXIS1', 'NAXIS2', 'BITPIX', 'BZERO', 'BSCALE', 'CADCPROC']:
            if new_header.get(key, None) is not None:
                del(new_header[key])
        image_hdulist[hdu_idx].header.update(new_header)

    return image_hdulist


def main():
    """Do the script."""
    parser = argparse.ArgumentParser(
        description='replace image header')
    parser.add_argument('--extname',
                        help='name of extension to in header')
    parser.add_argument('expnum', type=str,
                        help='exposure to update')
    parser.add_argument('-r', '--replace',
                        action='store_true',
                        help='store modified image back to VOSpace?')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('--debug', action='store_true')
    parser.add_argument('--force', action='store_true', help="Re-run even if previous success recorded")
    parser.add_argument('--dbimages', help="VOSpace DATA storage area.", default="vos:OSSOS/dbimages")

    args = parser.parse_args()

    storage.DBIMAGES = args.dbimages

    level = logging.CRITICAL
    message_format = "%(message)s"
    if args.verbose:
        level = logging.INFO
    if args.debug:
        level = logging.DEBUG
        message_format = "%(module)s %(funcName)s %(lineno)s %(message)s"
    logging.basicConfig(level=level, format=message_format)

    message = storage.SUCCESS
    expnum = args.expnum

    exit_status = 0
    try:
        # skip if already succeeded and not in force mode
        if storage.get_status(expnum, 36, 'update_header') and not args.force:
            logging.info("Already updated, skipping")
            sys.exit(0)
    
        image_hdulist = storage.get_image(args.expnum, return_file=False)
        ast_hdulist = storage.get_astheader(expnum, ccd=None)

        run_update_header(image_hdulist, ast_hdulist)
        image_filename = os.path.basename(storage.get_uri(expnum))
        image_hdulist.writeto(image_filename)
        if args.replace:
            dest = storage.dbimages_uri(expnum)
            storage.copy(image_filename, dest)
            storage.set_status(expnum, 36, 'update_header', message)
    except Exception as e:
        message = str(e)
        if args.replace:
            storage.set_status(expnum, 36, 'update_header', message)
        exit_status = message
        logging.error(message)

    return exit_status

if __name__ == '__main__':
    sys.exit(main())

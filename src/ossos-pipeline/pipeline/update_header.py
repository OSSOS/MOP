#!python
"""replace arg1 header with arg2s."""

import argparse
import logging
import os
import sys

from astropy.io import fits

from ossos import storage

hlines = None


def run_update_header(image, header_filename):

    hdu_list = fits.open(image,
                         do_not_scale_image_data=True,
                         mode='update')
    for hdu in hdu_list[1:]:
        extname = hdu.header.get('EXTNAME', None)
        logging.info("Got extno %s from header." % extname)
        new_header = get_header_object(header_filename,
                                       extname)
        for key in ['BZERO', 'BSCALE', 'CADCPROC']:
            v = hdu.header.get(key, None)
            if v is None:
                del(new_header[key])
                continue
            new_header[key] = v
        hdu.header = new_header

    hdu_list.flush()

    return hdu_list.close()


def get_header_object(header_filename, extname=None):
    """Given astGwyn header file build a Header object.

    astGwyn produces a single text file that contains all the
    headers, here we look through this header for the correct
    chunk for the image of interest.

    extno: int, the name of the desired extension

    """
    global hlines
    hlines = hlines or (
        open(header_filename).readlines())

    while len(hlines) > 0:
        sbuff = ''
        line = "{0:80}".format(hlines.pop(0).strip()[0:79])
        while line[0:3] != 'END' and len(hlines) > 0:
            sbuff += line
            line = "{0:80}".format(hlines.pop(0).strip()[0:79])

        ### we've reached a header END keyword or the end of the header
        if line[0:3] != 'END':
            raise ValueError('malformed header')

        ### pad out the buffer to make a valid header
        while len(sbuff) % 2880 != 0:
            sbuff += "{0:80}".format(' ')

        header = fits.Header.fromstring(sbuff)
        
        if extname is None or header.get('EXTNAME', None) == extname:
            return header

    raise ValueError('requested EXTNAME not found')


def main():
    """Do the script."""
    parser = argparse.ArgumentParser(
        description='replace image header')
    parser.add_argument('--extname',
                        help='name of extension to in header')
    parser.add_argument('expnum', type=str,
                        help='exposure to update')
    parser.add_argument('header', type=str,
                        nargs='?',
                        help='filename of replacement header')
    parser.add_argument('-r', '--replace',
                        action='store_true',
                        help='store modified image back to VOSpace?')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('--debug', action='store_true')
    parser.add_argument('--force', action='store_true', help="Re-run even if previous success recorded")

    args = parser.parse_args()

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
    
        header = (args.header is not None and ((os.access(args.header, os.W_OK) and args.header) or (
            storage.get_file(args.header, ext='head')))) or (storage.get_file(args.expnum, ext='head'))

        image = (os.access(args.expnum, os.W_OK) and args.expnum) or (storage.get_image(args.expnum))

        logging.info(
            "Swapping header for %s for contents in %s \n" % (
            image, header))

        expnum = fits.open(image)[0].header['EXPNUM'] or args.expnum

        # skip if already succeeded and not in force mode
        if storage.get_status(expnum, 36, 'update_header') and not args.force:
            logging.info("Skipping {} as already done.".format(expnum))
            sys.exit(0)

        run_update_header(image, header)
        if args.replace:
            dest = storage.dbimages_uri(expnum)
            storage.copy(image, dest)
            storage.set_status(expnum, 36, 'update_header', message)
    except Exception as e:
        if args.replace:
            message = str(e)
            storage.set_status(expnum, 36, 'update_header', message)
        exit_status = message
        logging.error(str(e))

    return exit_status

if __name__ == '__main__':
    sys.exit(main())

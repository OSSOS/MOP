#!/usr/bin/env python
'''replace arg1 header with arg2s.'''

from astropy.io import fits
import argparse
import logging
from ossos import storage
import os

hlines = None

def run_update_header(image, header_filename, extname=None):

    hdu_list = fits.open(image,
                         do_not_scale_image_data=True,
                         mode='update')
    if extname is None:
        for hdu in hdu_list[1:]:
            extname = hdu.header.get('EXTNAME',None)
            logging.info("Got extno %s from header." % (extname))
            hdu.header = get_header_object(header_filename,
                                           extname)
            hdu.header.update('BZERO', 32768, 'manually added')
            hdu.header.update('BSCALE', 1, 'manually added')
    else:
        logging.info("Replacing primary header with %s." % (extname))
        hdu_list[0].header = get_header_object(header_filename, extname)

    hdu_list.flush()

    return hdu_list.close()


def get_header_object(header_filename,extname=None):
    """Given astGwyn header file build a Header object.

    astGwyn produces a single text file that contains all the
    headers, here we look through this header for the correct
    chunk for the image of interest.

    extno: int, the name of the desired extension

    """
    global hlines
    hlines = hlines or (
        open(header_filename).readlines() )

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
        
        if extname is None or header.get('EXTNAME',None) == extname:
            return header

    raise ValueError('requested EXTNAME not found')

        

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='replace image header')
    parser.add_argument('--extname',
                        help='name of extension to in header')
    parser.add_argument('expnum',type=str,
                      help='exposure to update')
    parser.add_argument('header',type=str,
                        nargs='?',
                        help='filename of replacement header')
    parser.add_argument('-r','--replace',
                      action='store_true',
                      help='store modified image back to VOSpace?')
    parser.add_argument('-v','--verbose', action='store_true')
    
    args = parser.parse_args()

    if args.verbose:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    message = storage.SUCCESS
    try:
        
        image = (os.access(args.expnum,os.W_OK) and args.expnum ) or (
            storage.get_image(args.expnum) )
    
        header = (args.header is not None and ((
            os.access(args.header, os.W_OK) and args.header ) or (
            storage.get_image(args.header, ext='head')))) or ( 
            storage.get_image(args.expnum, ext='head'))

        logging.info(
            "Swapping header for %s for contents in %s \n" % (
            image, header) )

        run_update_header(image, header)
    
        if args.replace:
            expnum = args.expnum or fits.open(image)[0].header['EXPNUM']
            dest = storage.dbimages_uri(expnum)
            storage.copy(image, dest)
        storage.set_status(args.expnum, ccd="", 'update_header', message)
        sys.exit(0)
    except Exception as e:
        logging.error("Error replacing header for %s" % ( args.expnum))
        logging.error(str(e))
        message = str(e)
        storage.set_status(args.expnum, ccd="", 'update_header', message)
        sys.exit(2)
    

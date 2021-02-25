#!python
from multiprocessing import Pool
from ossos import mop_file, storage
import os
from astropy.io import fits
import numpy
from vos import Client
import sys
from ossos import junk_keywords
from ossos import util
import argparse
import logging

client = Client()

RA = 'RA_J2000'
DEC = 'DE_J2000'
del_keyword_list = junk_keywords.bad_keywords_list

def main(expnum, ccd):


    header = storage.get_astheader(expnum, ccd)
    datasec = storage.datasec_to_list(header.get('DATASEC', '[80:2080,30,4160]'))
    try:
        fwhm = "{:5.2f}".format(storage.get_fwhm(expnum, ccd))
    except:
        fwhm = 'unknown'
    for keyword in del_keyword_list:
        try:
            del(header[keyword])
        except:
            pass
    header['FWHM'] = (fwhm, 'FWHM in pixels')
    header['EXTNAME'] = 'header'
    primary_hdu = fits.PrimaryHDU(header=header)
    hdu_list = fits.HDUList([primary_hdu, ])
    for ext in ['jmp', 'matt']:
        extension = 'obj.'+ext
        name = "{}p{:02d}.{}".format(expnum, ccd, extension)
        try:
            os.unlink(name)
            os.unlink(name+".fits")
        except:
            pass
        logging.info("Retrieving {}".format(name))
        obj_file = mop_file.Parser(expnum, ccd, extension)
        obj_file.parse()
        
        t = numpy.all([datasec[0] < obj_file.data['X'], obj_file.data['X'] < datasec[1],
                       datasec[2] < obj_file.data['Y'], obj_file.data['Y'] < datasec[3]], axis=0)
        logging.info("Source remaining after datasec cut: {} of {}".format(len(obj_file.data[t]['X']), len(t)))
        table_hdu = fits.table_to_hdu(obj_file.data[t])
        table_hdu.header['CATALOG'] = name
        table_hdu.header['EXTNAME'] = ext
        hdu_list.append(table_hdu)
        del table_hdu
        del obj_file
        os.unlink(name)

    name = "{}p{:02d}.{}".format(expnum, ccd, 'obj.fits')
    if os.access(name, os.F_OK):
        os.unlink(name)
    hdu_list.writeto(name)
    uri = storage.dbimages_uri(expnum, ccd, 'p', ext=".obj.fits")
    logging.info(name+" -> "+uri)
    count = 0
    while True:
        print(("Copy attempt {}".format(count)))
        try:
            storage.copy(name, uri)
            os.unlink(name)
            break
        except Exception as ex:
            if count > 10:
                raise ex
            count += 1

if __name__ == '__main__':


    parser = argparse.ArgumentParser(
        description='Run FITS catalog builder chunk of the OSSOS pipeline')

    parser.add_argument('--ccd', '-c',
                        action='store',
                        type=int,
                        dest='ccd',
                        default=None,
                        help='which ccd to process, default is all')
    parser.add_argument("--dbimages",
                        action="store",
                        default="vos:OSSOS/dbimages",
                        help='vospace dbimages containerNode')
    parser.add_argument("expnum",
                        type=int,
                        nargs='+',
                        help="expnum(s) to process")
    parser.add_argument("--dry-run",
                        action="store_true",
                        help="DRY RUN, don't copy results to VOSpace, implies --force")

    parser.add_argument("--fk", action="store_true", help="Run fk images")

    parser.add_argument("--type", "-t", choices=['o', 'p', 's'],
                        help="which type of image: o-RAW, p-ELIXIR, s-SCRAMBLE", default='p')
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

    prefix = (args.fk and 'fk') or ''
    task  = util.task()
    dependency = 'step1'

    storage.DBIMAGES = args.dbimages

    for expnum in args.expnum:
        if args.ccd is None:
           if int(expnum) < 1785619:
               # Last exposures with 36 CCD Megaprime                                                                                                                                     
               ccdlist = list(range(0,36))
           else:
               # First exposrues with 40 CCD Megaprime                                                                                                                                    
               ccdlist = list(range(0, 40))
        else:
           ccdlist = [args.ccd]

        for ccd in ccdlist:
            if storage.get_status(task, prefix, expnum, version=args.type, ccd=ccd) and not args.force:
                logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, args.type, ccd))
                continue
            storage.set_logger(task,
                               prefix, expnum, ccd, args.type, args.dry_run)
            message = 'success'
            try:
                if not storage.get_status(dependency, prefix, expnum, "p", ccd=ccd):
                    raise IOError("{} not yet run for {}".format(dependency, expnum))
                main(expnum, ccd) # , args.type, args.dry_run, prefix=prefix)
            except Exception as e:
                message = str(e)

            logging.error(message)
            if not args.dry_run:
                storage.set_status(task, prefix, expnum, version=args.type, ccd=ccd, status=message)

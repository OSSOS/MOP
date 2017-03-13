"""Build the source detection list from an image.

This source detection system uses sextactor and produces PSF based measurement values."""
import subprocess

from ossos import storage
from ossos import util
from astropy.io import fits
import os, sys, argparse
import logging
task = 'build_cat'
dependency = None

def run(expnum, ccd, version, prefix, dry_run, force):

    message = storage.SUCCESS

    if storage.get_status(task, prefix, expnum, version=version, ccd=ccd) and not force:
        logging.info("{} completed successfully for {} {} {} {}".format(task, prefix, expnum, version, ccd))
        return

    with storage.LoggingManager(task, prefix, expnum, ccd, version, dry_run):
        try:
            if dependency is not None and not storage.get_status(dependency, prefix, expnum, "p", ccd=ccd):
                raise IOError("{} not yet run for {}".format(dependency, expnum))

            image_filename = storage.get_image(expnum, ccd)
            flat_filename = storage.get_flatfield(expnum, ccd)
            phot_zp = fits.open(image_filename)[0].header.get('PHOTZP', 0)

            catalog_uri = storage.get_uri(expnum, ccd, ext=".cat.fits")
            fits_cat_name = os.path.basename(catalog_uri)
            ldac_cat_name = os.path.splitext(image_filename)[0]+".ldac"
            storage.mkdir(os.path.dirname(catalog_uri))
            logging.info("Building PSF input catalog")
            sex_config = os.getenv('SEX_CONFIG', '')
            logging.info("Using config: {}".format(os.path.join(sex_config, 'pre_psfex.sex')))
            cmd = ['/usr/local/bin/sex', image_filename,
                   '-c', os.path.join(sex_config, 'pre_psfex.sex'),
                   '-CATALOG_NAME', ldac_cat_name,
                   '-WEIGHT_IMAGE', flat_filename,
                   '-MAG_ZEROPOINT', str(phot_zp)]
            logging.info(" ".join(cmd))
            logging.info(subprocess.check_output(cmd, stderr=subprocess.STDOUT))

            cmd = ['psfex', ldac_cat_name,
                   '-c', os.path.join(sex_config, 'default.psfex')]
            logging.info(" ".join(cmd))
            logging.info(subprocess.check_output(cmd,
                                                 stderr=subprocess.STDOUT))

            psf_uri = storage.dbimages_uri(expnum, ccd, ext='.psf')
            psf_name = os.path.basename(psf_uri)
            cmd = ['sex',
                   '-c', os.path.join(sex_config, 'ml.sex'),
                   '-WEIGHT_IMAGE', flat_filename,
                   '-CATALOG_NAME', fits_cat_name,
                   '-PSF_NAME', psf_name,
                   '-MAG_ZEROPOINT', str(phot_zp),
                   image_filename]
            logging.info(subprocess.check_output(cmd,
                                                 stderr=subprocess.STDOUT))

            if dry_run:
                return

            storage.copy(fits_cat_name, catalog_uri)
            storage.copy(psf_name, psf_uri)
            logging.info(message)

        except Exception as e:
            logging.error(str(e))
            message = str(e)

    storage.set_status(task, prefix, expnum, version, ccd, message)

    return


def main():

    parser = argparse.ArgumentParser(
        description='Run biuld_cat chunk of the CFIS pipeline')

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

    prefix = ''
    version = 'p'

    storage.DBIMAGES = args.dbimages

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
            run(expnum, ccd, version,  prefix, args.dry_run, args.force)
    return exit_code

if __name__ == '__main__':
    sys.exit(main())

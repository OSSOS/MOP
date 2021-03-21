
# !python
"""Retrieval of cutouts of the FITS images associated with the OSSOS detections.

Takes a directory of .ast file (in dbase format) as input

An Example URL for cutouts
http://www.canfar.phys.uvic.ca/vospace/auth/synctrans?TARGET=vos://cadc.nrc.ca~
vospace/OSSOS/dbimages/1625356/1625356p.fits&DIRECTION=pullFromVoSpace&PROTOCOL=
ivo://ivoa.net/vospace/core%23httpget&view=cutout&cutout=CIRCLE+ICRS+242.1318+-1
2.4747+0.05
"""

import argparse
import logging
import os
import sys

from astropy import units
from astropy.units import Quantity

from ossos import (mpc, storage, parameters)

storage.FITS_EXT = ".fits"

def cutout(obj, obj_dir, radius):

    cutout_listing = storage.listdir(obj_dir, force=True)
    for obs in obj.mpc_observations:
        if obs.null_observation:
            logging.debug('skipping: {}'.format(obs))
            continue
        if obs.date > parameters.SURVEY_START:
            # can't make postage stamps of earlier linkages
            # can't parse for an obs.comment's exposure number if no
            # obs.comment exists
            try:
                parts = storage.frame2expnum(obs.comment.frame)
            except Exception as ex:
                logging.warning(f"Skipping: {obs}")
                logging.debug(f"Failed to map comment.frame to expnum: {ex}")
                continue
            uri = storage.get_uri(parts['expnum'], version=parts['version'])
            sky_coord = obs.coordinate
            # Using the WCS rather than the X/Y
            # (X/Y can be unreliable over the whole survey)
            postage_stamp_filename = f"{obj.provisional_name}_" \
                                     f"{obs.date.mjd:11.5f}_" \
                                     f"{obs.coordinate.ra.degree:09.5f}_" \
                                     f"{obs.coordinate.dec.degeee:09.5f}.fits"

            if postage_stamp_filename in cutout_listing:
                # skipping existing cutouts
                continue 

            # ast_header = storage._get_sghead(parts['expnum'])
            while True:
              try:
                hdulist = storage.ra_dec_cutout(uri, sky_coord, radius, update_wcs=True)

                with open(postage_stamp_filename, 'w') as tmp_file:
                    hdulist.writeto(tmp_file, overwrite=True, output_verify='fix+ignore')
                    storage.copy(postage_stamp_filename, obj_dir + "/" + postage_stamp_filename)
                os.unlink \
                        (postage_stamp_filename)  # easier not to have them hanging around
              except OSError as e:  # occasionally the node is not found: report and move on for later cleanup
                logging.error("OSError: -> " +str(e))
              except Exception as e:
                logging.error("Exception: -> " +str(e))
                continue
              break


def main():
    parser = argparse.ArgumentParser(
        description='Parse a directory of TNO .ast files and create links in the postage stamp directory '
                    'that allow retrieval of cutouts of the FITS images associated with the OSSOS detections. '
                    'Cutouts are defined on the WCS RA/DEC of the object position.')

    parser.add_argument("version",
                        help="The OSSOS data release version these stamps should be assigned to. e.g. v8")
    parser.add_argument("astdir", help="Directory containing the input .ast files", action="store", default="ast/")
    parser.add_argument("--ossin",
                        action="store",
                        default="vos:OSSOS/0_OSSOSreleases/OSSOS",
                        help="The vospace containerNode that clones ossin dbaseclone"
                             "holding the .ast files of astrometry/photometry measurements.")
    parser.add_argument("--blocks", "-b",
                        action="store",
                        default=['o3e', 'o3o', 'o3l', 'o4h', 'o5p', 'o5m', 'o5s', 'o5t', 'o5c', 'o5d'],
                        choices=["o3e", "o3o", "Col3N", 'o3l', 'o4h', 'o5m', 'o5p', 'o5s', 'o5t', 'o5c', 'o5d'],
                        help="Prefixes of object designations to include.")
    parser.add_argument("--radius", '-r',
                        # FIXME: figure out how to assign this a unit.degree before storage
                        action='store',
                        default=36 * units.arcsec,  # about 200 px square
                        help='Radius (arcsec) of circle of cutout postage stamp.')
    parser.add_argument("--debug", "-d",
                        action="store_true")
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--recheck",
                        default=None,
                        action="store",
                        help="A tuple of TNO IDs to rerun")


    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.DEBUG)
    elif args.verbose:
        logging.basicConfig(level=logging.INFO)
    else:
        logging.basicConfig(level=logging.ERROR)


    astdir = args.astdir
    flist = os.listdir(astdir)
    if args.recheck:
        flist = [args.recheck + '.ast']

    for fn in flist:
        if not fn.endswith('.ast'):
            continue
        for block in args.blocks:
            if fn.startswith(block):
                obj_dir = '{}/{}/{}'.format(storage.POSTAGE_STAMPS, args.version, fn.partition('.')
                                                [0]) # obj.provisional_name
                logging.info \
                    ("Processing astrometric files in {}".format(obj_dir))
                storage.mkdir(obj_dir)
                obj = mpc.MPCReader(astdir + fn)
                # assert storage.exists(obj_dir, force=True)
                sys.stderr.write('{} beginning...'.format(obj.provisional_name))
                # if int(obj.provisional_name[3:]) == 49:
                assert isinstance(args.radius, Quantity)
                cutout(obj, obj_dir, args.radius)
                sys.stderr.write \
                    ('{} complete.\n\n'.format(obj.provisional_name))


if __name__ == '__main__':
    main()

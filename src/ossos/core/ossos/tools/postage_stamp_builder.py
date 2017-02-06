from __future__ import absolute_import

#!python
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

from astropy import units
from astropy.units import Quantity
from astropy.io import fits

from ossos import (mpc, storage, parameters)


def cutout(obj, obj_dir, radius):
    print(len([n for n in obj.mpc_observations if not n.null_observation]))
    for obs in obj.mpc_observations:
        print('starting analysis of {}'.format(str(obs)))
        if obs.null_observation:
            print('skipping')
            continue
        if obs.date > parameters.SURVEY_START:  # can't make postage stamps of earlier linkages
            # can't parse for an obs.comment's exposure number if no obs.comment exists
            try:
                expnum = obs.comment.frame.split('p')[0].strip(' ')  # only want calibrated images
            except AttributeError, e:
                print('No comment in this MPC line!')
                continue
            if not expnum.isdigit():
                print('expnum {} parsed from comment line invalid. Check comment parsing.\n{}'.format(
                    expnum, str(obs.comment))
                )
                continue
            uri = storage.get_uri(expnum)
            sky_coord = obs.coordinate  # Using the WCS rather than the X/Y (X/Y can be unreliable over the whole survey)
            print('Trying {} on {} on {}...'.format(obj.provisional_name, obs.date, expnum))
            try:
                hdulist = storage.ra_dec_cutout(uri, sky_coord, radius)
                # if not 'ASTLEVEL' in hdulist[1].header:   # FIXME: activate once all headers are retro-fitted with ASTLEVEL
                #     logging.info('Cutout invalid for use. Skipping inclusion.\n')
                #     continue
                postage_stamp_filename = "{}_{:11.5f}_{:09.5f}_{:+09.5f}.fits".format(obj.provisional_name,
                                                                                      obs.date.mjd,
                                                                                      obs.coordinate.ra.degree,
                                                                                      obs.coordinate.dec.degree)
                print("{}".format(postage_stamp_filename))

                with open(postage_stamp_filename, 'w') as tmp_file:
                    hdulist.writeto(tmp_file, clobber=True)
                    storage.copy(postage_stamp_filename, obj_dir + "/" + postage_stamp_filename)
                os.unlink(postage_stamp_filename)  # easier not to have them hanging around
            except OSError, e:  # occasionally the node is not found: report and move on for later cleanup
                print e
                continue


def main():
    parser = argparse.ArgumentParser(
        description='Parse a directory of TNO .ast files and create links in the postage stamp directory '
                    'that allow retrieval of cutouts of the FITS images associated with the OSSOS detections. '
                    'Cutouts are defined on the WCS RA/DEC of the object position.')

    parser.add_argument("version",
                        help="The OSSOS data release version these stamps should be assigned to. e.g. v8")
    parser.add_argument("--ossin",
                        action="store",
                        default="vos:OSSOS/0_OSSOSreleases/OSSOS",
                        help="The vospace containerNode that clones ossin dbaseclone"
                             "holding the .ast files of astrometry/photometry measurements.")
    parser.add_argument("--blocks", "-b",
                        action="store",
                        default=['o3e', 'o3o', 'o3l', 'o4h', 'o5p', 'o5m', 'o5s', 'o5t'],
                        choices=["o3e", "o3o", "Col3N", 'o3l', 'o4h', 'o5m', 'o5p', 'o5s', 'o5t', 'o5c', 'o5d'],
                        help="Prefixes of object designations to include.")
    parser.add_argument("--radius", '-r',
                        # FIXME: figure out how to assign this a unit.degree before storage
                        action='store',
                        default=0.01 * units.degree,  # about 200 px square
                        help='Radius (degree) of circle of cutout postage stamp.')
    parser.add_argument("--debug", "-d",
                        action="store_true")
    parser.add_argument("--verbose", "-v",
                        action="store_true")
    parser.add_argument("--recheck",
                        default=None,
                        action="store",
                        help="A tuple of TNO IDs to rerun")


    args = parser.parse_args()
    print args

    if args.debug:
        logging.basicConfig(level=logging.DEBUG)
    elif args.verbose:
        logging.basicConfig(level=logging.INFO)


    astdir = parameters.L7_HOME + 'OSSOS' + args.version + '/ast/'
    print astdir
    flist = os.listdir(astdir)
    if args.recheck:
        flist = [args.recheck + '.ast']
        print flist

    for fn in flist:
        for block in args.blocks:
            if fn.startswith(block):
                obj_dir = '{}/{}/{}'.format(storage.POSTAGE_STAMPS, args.version, fn.partition('.')[0]) # obj.provisional_name
                print obj_dir
                if args.recheck is None:  # otherwise if rechecking, it'll have already been started
                    if not storage.exists(obj_dir, force=True):
                        storage.mkdir(obj_dir)
                    else:
                        print(fn)
                        continue   # good if the object has already had its stamps cut
                obj = mpc.MPCReader(astdir + fn)
                # assert storage.exists(obj_dir, force=True)
                print('{} beginning...\n'.format(obj.provisional_name))
                # if int(obj.provisional_name[3:]) == 49:
                assert isinstance(args.radius, Quantity)
                cutout(obj, obj_dir, args.radius)
                print('{} complete.\n'.format(obj.provisional_name))


if __name__ == '__main__':
    main()

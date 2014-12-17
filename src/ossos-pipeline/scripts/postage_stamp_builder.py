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

import requests

from ossos import mpc
from ossos import storage


BASEURL = "http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/vospace/auth/synctrans"


def cutout(obj, obj_dir, args):
    for obs in obj.mpc_observations:
        if obs.null_observation:
            continue
        expnum = obs.comment.frame.split('p')[0]  # only want calibrated images
        # Using the WCS rather than the X/Y, as the X/Y can be unreliable on a long-term basis
        cutout = "CIRCLE ICRS {} {} {}".format(obs.coordinate.ra.degrees,
                                               obs.coordinate.dec.degrees,
                                               args.radius)
        # FIXME: should be able to use line below, but bug in VOSpace requires direct-access workaround, for now.
        # postage_stamp = storage.get_image(expnum, cutout=cutout)

        target = storage.get_uri(expnum)
        direction = "pullFromVoSpace"
        protocol = "ivo://ivoa.net/vospace/core#httpget"
        view = "cutout"
        params = {"TARGET": target,
                  "PROTOCOL": protocol,
                  "DIRECTION": direction,
                  "cutout": cutout,
                  "view": view}
        try:
            r = requests.get(BASEURL, params=params, auth=(args.username, args.password))
            r.raise_for_status()  # confirm the connection worked as hoped
            postage_stamp_filename = "{}_{:11.5f}_{:09.5f}_{:+09.5f}.fits".format(obj.provisional_name,
                                                                                  obs.date.mjd,
                                                                                  obs.coordinate.ra.degrees,
                                                                                  obs.coordinate.dec.degrees)
            logging.info("{}".format(postage_stamp_filename))
            with open(postage_stamp_filename, 'w') as tmp_file:
                tmp_file.write(r.content)
                # storage.copy(postage_stamp_filename, obj_dir)
                # os.remove(postage_stamp_filename)   # easier not to have them hanging around
        except requests.exceptions.HTTPError, e:
            logging.error("{}".format(str(e)))

        break

    return


def main():
    parser = argparse.ArgumentParser(
        description='Parse a directory of TNO .ast files and create links in the postage stamp directory '
                    'that allow retrieval of cutouts of the FITS images associated with the OSSOS detections. '
                    'Cutouts are defined on the WCS RA/DEC of the object position.')

    parser.add_argument("version",
                        nargs='+',
                        help="The OSSOS data release version these stamps should be assigned to.")
    parser.add_argument("username",
                        nargs='+',
                        help="VOSpace account username.")
    parser.add_argument("password",
                        nargs='+',
                        help="VOSpace account password.")
    parser.add_argument("--ossin",
                        action="store",
                        default="vos:OSSOS/dbaseclone/ast/",
                        help="The vospace containerNode that clones ossin's dbaseclone"
                             "holding the .ast files of astrometry/photometry measurements.")
    parser.add_argument("--blocks", "-b",
                        action="store",
                        default=["o3e", "o3o"],
                        choices=["o3e", "o3o", "O13BL", "Col3N"],
                        help="Prefixes of object designations to include.")
    parser.add_argument("--radius", '-r',
                        action='store',
                        default=0.02,
                        help='Radius (degrees) of circle of cutout postage stamp.')
    parser.add_argument("--debug", "-d",
                        action="store_true")
    parser.add_argument("--verbose", "-v",
                        action="store_true")

    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.DEBUG)
    elif args.verbose:
        logging.basicConfig(level=logging.INFO)

    for fn in storage.listdir(args.ossin)[0:1]:
        obj = mpc.MPCReader(args.ossin + fn)  # let MPCReader's logic determine the provisional name
        for block in args.blocks:
            if obj.provisional_name.startswith(block):
                obj_dir = '{}/{}/{}'.format(storage.POSTAGE_STAMPS, args.version[0], obj.provisional_name)
                try:
                    storage.exists(obj_dir, force=True)
                except IOError:
                    storage.mkdir(obj_dir)
                cutout(obj, obj_dir, args)


if __name__ == '__main__':
    main()

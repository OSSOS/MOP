#!python
"""Create links in the postage stamp directory that allow retrieval of the FITS images associated with the OSSOS detections.

Takes a directory of .ast file (in dbase format) as input

An Example URL for cutouts
http://www.canfar.phys.uvic.ca/vospace/auth/synctrans?TARGET=vos://cadc.nrc.ca~
vospace/OSSOS/dbimages/1625356/1625356p.fits&DIRECTION=pullFromVoSpace&PROTOCOL=
ivo://ivoa.net/vospace/core%23httpget&view=cutout&cutout=CIRCLE+ICRS+242.1318+-1
2.4747+0.05
"""
import sys
import re
import argparse

from ossos import mpc
from ossos import storage


def link_cutout(obj, obj_dir, args):
    for obs in obj.mpc_observations:
        if obs.null_observation:
            continue

        try:
            match = re.search('(\d+)\D(\d+)', obs.comment.frame)
            if match is None:
                print obs
                continue
            expnum = match.group(1)
            ccd = int(match.group(2))
            X = float(obs.comment.x)
            Y = float(obs.comment.y)
            x1 = int(max(1, X - args.width))
            x2 = int(min(2112, X + args.width))
            y1 = int(max(1, Y - args.width))
            y2 = int(min(4644, Y + args.width))

            cutout = '[{}][{}:{},{}:{}]'.format(ccd, x1, x2, y1, y2)
            full_image = storage.get_image(expnum, ccd=ccd, cutout=cutout)
            postage_stamp = "{}/{}_{:11.5f}_{:09.5f}_{:+09.5f}.fits".format(
                obj_dir,
                obj.provisional_name,
                obs.date.mjd,
                obs.coordinate.ra.degrees,
                obs.coordinate.dec.degrees)
            print "{} -> {}".format(postage_stamp, full_image)
            storage.vospace.link(full_image, postage_stamp)
        except Exception as e:
            sys.stderr.write(str(e))

    return


def main():
    parser = argparse.ArgumentParser(
        description='Parse a directory of TNO .ast files and create links in the postage stamp directory '
                    'that allow retrieval of cutouts of the FITS images associated with the OSSOS detections.')

    parser.add_argument("version",
                        nargs='+',
                        help="The OSSOS data release version these stamps should be assigned to.")
    parser.add_argument("--ossin",
                        action="store",
                        default="vos:OSSOS/dbaseclone/ast/",
                        help="The vospace containerNode that clones ossin's dbaseclone"
                             "holding the .ast files of astrometry/photometry measurements.")
    parser.add_argument("--blocks", "-b",
                        action="store",
                        default=["o3e", "o3o"],
                        choices=["o3e", "o3o", "O13BL", "Col3N"],
                        help="Prefixes of object OSSOS designations to include.")
    parser.add_argument("--width", '-w',
                        action='store',
                        default=256 / 2.0,
                        help='Dimension of cutout postage stamp.')

    args = parser.parse_args()

    for fn in storage.listdir(args.ossin)[0:1]:
        obj = mpc.MPCReader(args.ossin + fn)  # let MPCReader's logic determine the provisional name
        for block in args.blocks:
            if obj.provisional_name.startswith(block):
                obj_dir = '{}/{}/{}'.format(storage.POSTAGE_STAMPS, args.version, obj.provisional_name)
                if not storage.exists(obj_dir):
                    storage.mkdir(obj_dir)
                link_cutout(obj, obj_dir, args)


if __name__ == '__main__':
    main()

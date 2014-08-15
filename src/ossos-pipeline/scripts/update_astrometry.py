#!python
import re
import numpy
import sys
from ossos.downloads.cutouts import ImageCutoutDownloader

__author__ = 'jjk'
"""
Update the astrometry and photometry of an mpc observation based on the header contents of the observation.
 """


from ossos import mpc, storage, wcs, astrom
import argparse


def remeasure(mpc_obs):
    """
    re-measure the astrometry and photometry of the given mpc line
    """
    # TODO  Actually implement this.
    if mpc_obs.null_observation:
        return mpc_obs
    assert isinstance(mpc_obs.comment, mpc.MPCComment)
    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)',mpc_obs.comment.frame)
    header = storage.get_astheader(parts.group('expnum'),int(parts.group('ccd')))
    this_wcs = wcs.WCS(header)
    x = float(mpc_obs.comment.x)
    y = float(mpc_obs.comment.y)
    (ra,dec) = this_wcs.xy2sky(x,y)
    mpc_obs.coordinate = (ra,dec)
    recompute_mag(mpc_obs)
    return mpc_obs



def recompute_mag(mpc_obs):
    """
    Get the mag of the object given the mpc.Observation
    """
    # TODO this really shouldn't need to build a 'reading' to get the cutout...
    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)', mpc_obs.comment.frame)
    if parts is None:
        return None
    expnum = parts.group('expnum')
    ccd = parts.group('ccd')
    ftype = parts.group('type')

    observation =  astrom.Observation(expnum,ftype,ccd)
    observation.header = storage.get_mopheader(int(expnum),int(ccd))
    reading = astrom.SourceReading(float(mpc_obs.comment.x),
                                   float(mpc_obs.comment.y),
                                   float(mpc_obs.comment.x),
                                   float(mpc_obs.comment.y),
                                   mpc_obs.coordinate.ra.degrees,
                                   mpc_obs.coordinate.dec.degrees,
                                   float(mpc_obs.comment.x),
                                   float(mpc_obs.comment.y),
                                   observation,
                                   ssos=True,
                                   from_input_file=True,null_observation=False,discovery=mpc_obs.discovery)


    image_slice_downloader = ImageCutoutDownloader(slice_rows=100, slice_cols=100)
    cutout = image_slice_downloader.download_cutout(reading, needs_apcor=True)
    try:
        (x, y, mag, merr) = cutout.get_observed_magnitude()
        mpc_obs.mag = mag
        mpc_obs.mag_err = merr
    except:
        mpc_obs.mag = None
        mpc_obs.mag_err = None

    return



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('mpc_file',help="An MPC file to update.")
    parser.add_argument('cor_file',help="Corrected MPC file")

    args = parser.parse_args()
    out_mags = []
    out_merrs = []
    in_mags = []
    in_merrs = []
    fptr = storage.open_vos_or_local(args.mpc_file)
    optr = storage.open_vos_or_local(args.cor_file, mode='w')
    lines = fptr.read().split('\n')
    for line in lines:
        if not len(line) > 0:
            continue
        mpc_in = mpc.Observation.from_string(line)
        mpc_obs = remeasure(mpc_in)
        optr.write(mpc_obs.to_string()+"\n")
        if not mpc_obs.comment.photometry_note[0] == "Z" and str(mpc_obs.note1) not in ["I", "H"]:
            out_mags.append(mpc_obs.mag)
            in_mags.append(mpc_in.mag)
            in_merrs.append(mpc_in.mag_err)
            out_merrs.append(mpc_obs.mag_err)
    optr.close()
    fptr.close()
    sys.stdout.write(args.mpc_file)
    sys.stdout.write("{:6.2f} {:6.2f} ".format(numpy.average(in_mags, weights=in_merrs), numpy.std(in_mags)))
    sys.stdout.write("{:6.2f} {:6.2f} ".format(numpy.average(out_mags, weights=out_merrs), numpy.std(out_mags)))
    sys.stdout.write("\n")


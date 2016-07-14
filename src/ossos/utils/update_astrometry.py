#!python
"""
Update the astrometric and photometric measurements of an mpc observation based on the header contents of the
observation.
"""
from copy import deepcopy
from astropy import units
import os
import math
import numpy

__author__ = 'jjk'

import logging
import re

from ossos import mpc, storage, wcs, astrom
from ossos import orbfit
import argparse

# Maximum allowed change in angle during re-measure
TOLERANCE = 2.00 * units.arcsec


def remeasure(mpc_in, recentroided=False):
    """
    re-measure the astrometry and photometry of the given mpc line
    """
    if mpc_in.null_observation:
        return mpc_in
    mpc_obs = deepcopy(mpc_in)
    logging.debug("rm start: {}".format(mpc_obs.to_string()))

    if not isinstance(mpc_obs.comment, mpc.OSSOSComment):
        logging.error( "Failed to convert comment line")
        return mpc_in

    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)', mpc_obs.comment.frame)
    if not parts:
        logging.error( "Failed to parse expnum from frame info in comment line")
        return mpc_in

    start_coordinate = mpc_in.coordinate
    #assert isinstance(start_coordinate, ICRSCoordinates)

    try:
        header = storage._get_sghead(parts.group('expnum'))[int(parts.group('ccd')) + 1]
    except IOError as ioerr:
        logging.error(str(ioerr))
        logging.error("Failed to get astrometric header for: {}".format(mpc_obs))
        return mpc_in

    this_wcs = wcs.WCS(header)
    try:
        #(x , y ) = this_wcs.sky2xy(mpc_obs.coordinate.ra.degree, mpc_obs.coordinate.dec.degree, usepv=True) 
        x = mpc_obs.comment.x
        y = mpc_obs.comment.y
        #logging.error("{} {}".format( x, float(mpc_obs.comment.x)))
        #logging.error("{} {}".format( y, float(mpc_obs.comment.y)))
        if mpc_in.discovery and int(parts.group('ccd')) < 18 or int(parts.group('ccd')) in [36, 37]:
            logging.warn("This is a discovery line so flipping/flopping the x/y position recorded in comment as that is likely take from the flip/flopped image.")
            x = header['NAXIS1'] - x + 1
            y = header['NAXIS2'] - y + 1
    except:
        logging.warn("Failed to get the X Y coordinate from comment line.")
        return mpc_in

    (ra, dec) = this_wcs.xy2sky(x, y, usepv=True)
    mpc_obs.coordinate = (ra, dec)
    sep = mpc_in.coordinate.separation(mpc_obs.coordinate)

    if sep > TOLERANCE and mpc_in.discovery :
        logging.warn("sep: {} --> The RA/DEC determined using the flip/flopped coordinates is not close enough to original value reverting to original values.".format(sep))
        # Try un-flipping.
        x = float(mpc_in.comment.x)
        y = float(mpc_in.comment.y)
        (ra, dec) = this_wcs.xy2sky(x, y, usepv=True)
        #print "--> x/y coordinates ({},{}) and recomputing ra/dec ({},{})".format(x, y, ra, dec)
        mpc_obs.coordinate = (ra, dec)
        sep = mpc_in.coordinate.separation(mpc_obs.coordinate)

    if sep > TOLERANCE and not recentroided:
        # use the old header RA/DEC to predict the X/Y and then use that X/Y to get new RA/DEC
        logging.warn("sep: {} --> large offset when using comment line X/Y to compute RA/DEC, using RA/DEC and original WCS to compute X/Y and replacing X/Y reported in astrometry file.".format(sep))
        header2 = storage.get_astheader(parts.group('expnum'), int(parts.group('ccd')))
        # header2 = storage.get_image(parts.group('expnum'),
        #                            int(parts.group('ccd')),
        #                            return_file=False,
        #                            flip_image=False)[0].header
        image_wcs = wcs.WCS(header2)
        (x, y) = image_wcs.sky2xy(mpc_in.coordinate.ra.degree, mpc_in.coordinate.dec.degree, usepv=False)
        (ra, dec) = this_wcs.xy2sky(x, y, usepv=True)
        logging.warn("Coordinate changed: ({:5.2f},{:5.2f}) --> ({:5.2f},{:5.2f})".format(mpc_obs.comment.x,
                                                                                          mpc_obs.comment.y,
                                                                                          x, y))
        mpc_obs.coordinate = (ra, dec)
        mpc_obs.comment.x = x
        mpc_obs.comment.y = y


    try:
        merr = float(mpc_obs.comment.mag_uncertainty)
        fwhm = float(storage.get_fwhm(parts.group('expnum'), int(parts.group('ccd')))) * header['PIXSCAL1']
        centroid_err = merr * fwhm
        logging.debug("Centroid uncertainty:  {} {} => {}".format(merr, fwhm, centroid_err))
    except Exception as err:
        logging.error(str(err))
        logging.error("Failed to compute centroid for observation:\n"
                      "{}\nUsing default of 0.2".format(mpc_obs.to_string()))
        centroid_err = 0.2

    mpc_obs.comment.astrometric_level = header.get('ASTLEVEL', "0")

    try:
        asterr = float(header['ASTERR'])
        residuals = (asterr ** 2 + centroid_err ** 2) ** 0.5
        logging.debug("Residuals: {} {} => {}".format(asterr, centroid_err, residuals))
    except Exception as err:
        logging.error(str(err))
        logging.error("Failed while trying to compute plate uncertainty for\n{}".format(mpc_obs.to_stirng()))
        logging.error('Using default of 0.25')
        residuals = 0.25

    mpc_obs.comment.plate_uncertainty = residuals

    logging.debug("sending back: {}".format(mpc_obs.to_string()))

    return mpc_obs


def recompute_mag(mpc_in):
    """
    Get the mag of the object given the mpc.Observation
    """
    # TODO this really shouldn't need to build a 'reading' to get the cutout...

    from ossos.downloads.cutouts import downloader
    dlm = downloader.ImageCutoutDownloader()

    mpc_obs = deepcopy(mpc_in)
    assert isinstance(mpc_obs, mpc.Observation)
    if mpc_obs.null_observation:
        return mpc_obs
    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)', mpc_obs.comment.frame)
    if parts is None:
        return mpc_obs
    expnum = parts.group('expnum')
    ccd = parts.group('ccd')
    file_type = parts.group('type')

    observation = astrom.Observation(expnum, file_type, ccd)
    assert isinstance(observation, astrom.Observation)
    #ast_header = storage._get_sghead(int(expnum), 'p')[int(ccd)+1]

    # The ZP for the current astrometric lines is the pipeline one.  The new ZP is in the astheader file.
    #new_zp = ast_header.get('PHOTZP')

    # The .zeropoint.used value is likely the one used for the original photometry.
    old_zp = storage.get_zeropoint(int(expnum), int(ccd))
    reading = astrom.SourceReading(float(mpc_obs.comment.x), float(mpc_obs.comment.y), float(mpc_obs.comment.x),
                                   float(mpc_obs.comment.y), mpc_obs.coordinate.ra.degree,
                                   mpc_obs.coordinate.dec.degree, float(mpc_obs.comment.x), float(mpc_obs.comment.y),
                                   observation, ssos=True, from_input_file=True, null_observation=False,
                                   discovery=mpc_obs.discovery)
    cutout = dlm.download_cutout(reading, needs_apcor=True)
    ccdnum = int(parts.group('ccd'))
    try:
        if mpc_in.discovery and ccdnum < 18 or ccdnum in [36, 37]:
            logging.warn("This is a discovery line so flipping/flopping the x/y position "
                         "recorded in comment as that is likely taken from a flip/flopped image, and we now work"
                         "in unflip/flop frame" )
            # Get the full header of the image so we can flip/flop the coordinates
            header = storage._get_sghead(parts.group('expnum'))[ccdnum + 1]
            # Get the X/Y position of the source from the MPC line comment.
            x = mpc_obs.comment.x
            y = mpc_obs.comment.y
            # Do the coordinate flip/flop
            x = header['NAXIS1'] - x + 1
            y = header['NAXIS2'] - y + 1
            # Now x/y are in the full frame of the MEF image which is stored in cutout.
            # so we transform to the pixel coordinates of the cutout.
            (x_p, y_p, hdu_list_idx)  = cutout.get_pixel_coordinates((x, y), ccdnum)
            # Now turn those in RA/DEC coordinates.
            mpc_obs.coordinate = cutout.pix2world(x_p, y_p, hdu_list_idx)
            if mpc_in.coordinate.separation(mpc_obs.coordinate) > TOLERANCE:
                logger.warning("Flipping coordinates don't match RA/DEC. reverting to oringal values.")
                # Try reverting back to the orignal X/Y pixel locations from the input astrometry line.
                (x_p, y_p, hdu_list_idx)  = cutout.get_pixel_coordinates((mpc_obs.comment.x, mpc_in.comment.y), ccdnum)
                mpc_obs.coordinate = cutout.pix2world(x_p, y_p, hdu_list_idx)
                if mpc_in.coordinate.separation(mpc_obs.coordinate) > TOLERANCE:
                    # Something is not good.
                    raise ValueError("Can't determine x/y to RA/DEC mapping on discovery images.")
            else:
                logger.warn("Updating X/Y to use the not flipped coordinates frame.")
            # OK, we have the correct pixel coordinates for the cutout we are looking, store the orignal observerd coord
            cutout.update_pixel_location((x_p, y_p), hdu_list_idx)
    except Exception as ex:
        logging.warn(ex)
        logging.warn("Failed to get the X Y coordinate from comment line.")
        return mpc_in

    # cutout.zmag = new_zp

    if math.fabs(cutout.zmag - old_zp) > 0.3:
        logging.warn("Large change in zeropoint detected: {}  -> {}".format(old_zp, cutout.zmag))

    try:
        PHOT = cutout.get_observed_magnitude()
        x = PHOT['XCENTER']
        y = PHOT['YCENTER']
        mag = PHOT['MAG']
        merr = PHOT['MERR']
        cutout.update_pixel_location((x, y), hdu_index=cutout.extno)
        x, y = cutout.observed_source_point
    except Exception as ex:
        logging.error("ERROR: {}".format(str(ex)))
        logging.warn("Failed to do photometry.")
        return mpc_obs
    if mpc_obs.comment.mag is not None and math.fabs(mpc_obs.comment.mag - mag) > 3.5 * mpc_obs.comment.mag_uncertainty:
        logging.warn("recomputed magnitude shift large: {} --> {}".format(mpc_obs.mag, mag[0]))
    if math.sqrt((x.value - mpc_obs.comment.x) ** 2 + (y.value - mpc_obs.comment.y) ** 2) > 1.9:
        logging.warn("Centroid shifted ({},{}) -> ({},{})".format(mpc_obs.comment.x,
                                                                  mpc_obs.comment.y,
                                                                  x.value,
                                                                  y.value))
    # Don't use the new X/Y for Hand measured entries.
    if str(mpc_obs.note1) != "H":
        mpc_obs.comment.x = x.value
        mpc_obs.comment.y = y.value

    mpc_obs.comment.mag = mag
    mpc_obs.comment.mag_uncertainty = merr
    if mpc_obs.mag is not None:
        mpc_obs.mag = mag

    return mpc_obs


def main(mpc_file, cor_file, skip_mags=False):
    """

    :param mpc_file: A file containing the astrometric lines to be updated.
    :param cor_file: The base name for the updated astrometry and diagnostic files.
    :param skip_mags: Should we skip recomputing the magnitude of sources?
    :return: :raise ValueError: If actions on the mpc_obs indicate this is not a valid OSSOS observations
    """
    observations = mpc.MPCReader(mpc_file).mpc_observations

    original_obs = []
    modified_obs = []
    logging.info("ASTROMETRY FILE: {} --> {}".format(mpc_file, cor_file))
    for mpc_in in observations:
        if not isinstance(mpc_in.comment, mpc.OSSOSComment):
            continue
        logging.info("="*220)
        logging.info("   orig: {}".format(mpc_in.to_string()))
        mpc_obs = remeasure(mpc_in)
        logging.info("new wcs: {}".format(mpc_obs.to_string()))

        if skip_mags and not mpc_obs.comment.photometry_note[0] == "Z":
            mpc_mag = remeasure(recompute_mag(mpc_obs), recentroided=True)
        else:
            mpc_mag = mpc_obs

        sep = mpc_in.coordinate.separation(mpc_mag.coordinate)
        if sep > TOLERANCE:
            logging.error("Large offset: {} arc-sec".format(sep))
            logging.error("orig: {}".format(mpc_in.to_string()))
            logging.error(" new: {}".format(mpc_mag.to_string()))
            new_comment = raw_input("COMMENT: ")
            mpc_mag.comment.comment = mpc_mag.comment.comment + " " + new_comment
        logging.info("new cen: {}".format(mpc_mag.to_string()))
        original_obs.append(mpc_in)
        modified_obs.append(mpc_mag)
        logging.info("="*220)

    optr = file(cor_file+".tlf", 'w')
    for idx in range(len(modified_obs)):
        inp = original_obs[idx]
        out = modified_obs[idx]
        if inp != out:
            optr.write(out.to_tnodb()+"\n")
    optr.close()

    compare_orbits(original_obs, modified_obs, cor_file)
    logging.info("="*220)

    return True


def compare_orbits(original_obs, modified_obs, cor_file):
    """Compare the orbit fit given the oringal and modified astrometry."""

    origin = orbfit.Orbfit(original_obs)
    modified = orbfit.Orbfit(modified_obs)

    orbpt = file(cor_file+".orb", 'w')

    # Dump summaries of the orbits
    orbpt.write("#"*80+"\n")
    orbpt.write("# ORIGINAL ORBIT\n")
    orbpt.write(origin.summarize()+"\n")
    orbpt.write("#"*80+"\n")
    orbpt.write("# MODIFIED ORBIT\n")
    orbpt.write(modified.summarize()+"\n")
    orbpt.write("#"*80+"\n")

    # Create a report of the change in orbit parameter uncertainty
    for element in ['a', 'e', 'inc', 'om', 'Node', 'T']:
        oval = getattr(origin, element).value
        doval = getattr(origin, "d"+element).value
        mval = getattr(modified, element).value
        dmval = getattr(modified, "d"+element).value
        precision = max(int(-1*math.floor(math.log10(dmval))), int(-1*math.floor(math.log10(doval)))) + 1
        precision = max(0, precision)
        vdigits = 12
        ddigits = 6
        vpadding = " "*int(vdigits-precision)
        dpadding = " "*int(ddigits-precision)

        orbpt.write("{element:>5s}: "
                    "{oval[0]:>{vdigits}.{vdigits}}.{oval[1]:<{precision}.{precision}} {vpadding} +/- "
                    "{doval[0]:>{ddigits}.{ddigits}}.{doval[1]:<{precision}.{precision}} {dpadding} ==> "
                    "{mval[0]:>{vdigits}.{vdigits}}.{mval[1]:<{precision}.{precision}} {vpadding} +/- "
                    "{dmval[0]:>{ddigits}.{ddigits}}.{dmval[1]:<{precision}.{precision}}\n".format(
            element=element,
            dpadding=dpadding,
            vpadding=vpadding,
            vdigits=vdigits,
            ddigits=ddigits,
            oval="{:12.12f}".format(oval).split("."),
            doval="{:12.12f}".format(doval).split("."),
            mval="{:12.12f}".format(mval).split("."),
            dmval="{:12.12f}".format(dmval).split("."),
            precision=precision)
        )

        delta = math.fabs(oval - mval)
        if delta > 3.5 * doval:
            logging.warn("large delta for element {}: {} --> {}".format(element, oval, mval))


    # Compute the stdev of the residuals and report the change given the new observations
    orbpt.write("*"*80+"\n")
    orbpt.write("Change in orbital parameters \n")
    sep = "Change in scatter between initial and recalibrated obseravtions. \n"
    for orb in [origin, modified]:
        orbpt.write(sep)
        sep = "\n ==> becomes ==> \n"
        residuals = orb.residuals
        dra = []
        ddec = []
        mags = {}
        for observation in orb.observations:
            if not observation.null_observation:
                dra.append(observation.ra_residual)
                ddec.append(observation.dec_residual)
                filter = observation.band
                if filter is not None:
                    if filter not in mags:
                        mags[filter] = []
                    try:
                        mags[filter].append(float(observation.mag))
                    except:
                        pass
                if observation.comment.plate_uncertainty * 5.0 < \
                        ((observation.ra_residual ** 2 + observation.dec_residual ** 2) ** 0.5):
                    logging.warn("LARGE RESIDUAL ON: {}".format(observation.to_string()))
                    logging.warn("Fit residual  unreasonably large.")
        dra = numpy.array(dra)
        ddec = numpy.array(ddec)
        print mags
        merr_str = ""
        for filter in mags:
            mag = numpy.percentile(numpy.array(mags[filter]), (50))
            print mag
            mags[filter] = numpy.percentile(numpy.array(mags[filter]), (5,95))
            print mags[filter]
            merr = (mags[filter][1] - mags[filter][0])/6.0
            merr_str += " {}: {:8.2f} +/- {:8.2f}".format(filter, mag, merr)
        orbpt.write("ra_std:{:8.4} dec_std:{:8.4} mag: {}".format(dra.std(), ddec.std(), merr_str))
    orbpt.write("\n")
    orbpt.close()


if __name__ == '__main__':
    description = """This program takes as input a set of OSSOS measurements and adjusts the astrometric and photometric
    entries to be consistent with the current best estimate for the astrometric and photometric calibrations.
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('ast_file', help="An MPC file to update.")
    parser.add_argument('--result_base_name', help="base name for remeasurement results (defaults to basename of input)",
                        default=None)
    parser.add_argument('--skip-mags', action="store_false", help="Recompute magnitudes.", default=True)
    parser.add_argument('--debug', action='store_true')

    args = parser.parse_args()

    level = logging.INFO
    if args.debug:
        level = logging.DEBUG

    import coloredlogs
    logger = logging.getLogger('update_astrom')
    print level, logging.DEBUG
    coloredlogs.install(level=level)

    if args.result_base_name is None:
        base_name = os.path.splitext(os.path.basename(args.ast_file))[0]
    else:
        base_name = args.result_base_name
    main(args.ast_file, base_name, args.skip_mags)


#!python
"""
Update the astrometric and photometric measurements of an mpc observation based on the header contents of the
observation.
"""
from copy import deepcopy
import time
from astropy import units
import os
import math
import numpy
import logging
import re
import mp_ephem
from ossos import storage, wcs, astrom
from ossos import orbfit
import argparse

__author__ = 'jjk'

# Maximum allowed change in angle during re-measure
TOLERANCE = 2.00 * units.arcsec


def _flipped_ccd(ccd):
    """
    Is this CCD likely flipped?
    
    The MegaCam imager on CFHT has E/N flipped for some of their CCDs.
    
    @param ccd:
    @return:
    """
    return ccd < 18 or ccd in [36, 37]


def remeasure(mpc_in, reset_pixel_coordinates=True):
    """
    Compute the RA/DEC of the line based on the X/Y in the comment and the WCS of the associated image.

    Comment of supplied astrometric line (mpc_in) must be in OSSOSComment format.

    @param mpc_in: An line of astrometric measurement to recompute the RA/DEC from the X/Y in the comment.
    @type mpc_in: mp_ephem.Observation
    @param reset_pixel_coordinates: try and determine correct X/Y is X/Y doesn't map to correct RA/DEC value
    @type reset_pixel_coordinates: bool
    @type reset_pixecl_coordinates: bool

    """
    if mpc_in.null_observation:
        return mpc_in
    mpc_obs = deepcopy(mpc_in)
    logging.debug("rm start: {}".format(mpc_obs.to_string()))

    if not isinstance(mpc_obs.comment, mp_ephem.ephem.OSSOSComment):
        logging.error("Failed to convert comment line")
        return mpc_in

    parts = re.search('(?P<expnum>\d{7})(?P<type>\S)(?P<ccd>\d\d)', str(mpc_obs.comment.frame))
    if not parts:
        logging.error("Failed to parse expnum from frame info in comment line")
        return mpc_in
    ccd = int(parts.group('ccd'))
    expnum = int(parts.group('expnum'))
    exp_type = parts.group('type')

    try:
        header = _connection_error_wrapper(storage._get_sghead, expnum)[ccd+1]
    except IOError as ioerr:
        logging.error(str(ioerr))
        logging.error("Failed to get astrometric header for: {}".format(mpc_obs))
        return mpc_in

    this_wcs = wcs.WCS(header)

    coordinate = this_wcs.xy2sky(mpc_obs.comment.x, mpc_obs.comment.y, usepv=True)
    mpc_obs.coordinate = coordinate[0].to('degree').value, coordinate[1].to('degree').value
    sep = mpc_in.coordinate.separation(mpc_obs.coordinate)

    if sep > TOLERANCE*20 and mpc_in.discovery and _flipped_ccd(ccd):
        logging.warn("Large ({}) offset using X/Y in comment line to compute RA/DEC".format(sep))
        if reset_pixel_coordinates:
            logging.info("flipping/flopping the discvoery x/y position recorded.")
            x = header['NAXIS1'] - mpc_obs.comment.x + 1
            y = header['NAXIS2'] - mpc_obs.comment.y + 1
            new_coordinate = this_wcs.xy2sky(x, y, usepv=True)
            new_sep = mpc_in.coordinate.separation(new_coordinate)
            if new_sep < TOLERANCE*2:
                mpc_obs.coordinate = new_coordinate
                mpc_obs.comment.x = x
                mpc_obs.comment.y = y
                sep = new_sep

    if sep > TOLERANCE:
        # use the old header RA/DEC to predict the X/Y and then use that X/Y to get new RA/DEC
        logging.warn("sep: {} --> large offset when using comment line X/Y to compute RA/DEC")
        if reset_pixel_coordinates:
           logging.warn("Using RA/DEC and original WCS to compute X/Y and replacing X/Y in comment.".format(sep))
           header2 = _connection_error_wrapper(storage.get_astheader, expnum, ccd)
           image_wcs = wcs.WCS(header2)
           (x, y) = image_wcs.sky2xy(mpc_in.coordinate.ra.degree, mpc_in.coordinate.dec.degree, usepv=False)
           mpc_obs.coordinate = this_wcs.xy2sky(x, y, usepv=True)
           mpc_obs.comment.x = x
           mpc_obs.comment.y = y
           logging.info("Coordinate changed: ({:5.2f},{:5.2f}) --> ({:5.2f},{:5.2f})".format(mpc_obs.comment.x,
                                                                                             mpc_obs.comment.y,
                                                                                             x, y))

    if mpc_obs.comment.mag_uncertainty is not None:
        try:
            merr = float(mpc_obs.comment.mag_uncertainty)
            fwhm = float(_connection_error_wrapper(storage.get_fwhm, expnum, ccd))
            centroid_err = merr * fwhm * header['PIXSCAL1']
            logging.debug("Centroid uncertainty:  {} {} => {}".format(merr, fwhm, centroid_err))
        except Exception as err:
            logging.error(str(err))
            logging.error("Failed to compute centroid_err for observation:\n"
                          "{}\nUsing default of 0.2".format(mpc_obs.to_string()))
            centroid_err = 0.2
    else:
        centroid_err = 0.2

    mpc_obs.comment.astrometric_level = header.get('ASTLEVEL', "0")

    try:
        asterr = float(header['ASTERR'])
        residuals = (asterr ** 2 + centroid_err ** 2) ** 0.5
        logging.debug("Residuals: {} {} => {}".format(asterr, centroid_err, residuals))
    except Exception as err:
        logging.error(str(err))
        logging.error("Failed while trying to compute plate uncertainty for\n{}".format(mpc_obs.to_string()))
        logging.error('Using default of 0.25')
        residuals = 0.25

    mpc_obs.comment.plate_uncertainty = residuals

    logging.debug("sending back: {}".format(mpc_obs.to_string()))

    return mpc_obs


def _connection_error_wrapper(func, *args, **kwargs):
    """
    Wrap a call to func in a try/except that repeats on ConnectionError
    @param func:
    @param args:
    @param kwargs:
    @return:
    """

    counter = 0
    while counter < 5:
        try:
            result = func(*args, **kwargs)
            return result
        except Exception as ex:
            time.sleep(5)
            counter += 1
            logging.warning(str(ex))


def recompute_mag(mpc_in, skip_centroids=False):
    """
    Get the mag of the object given the mp_ephem.ephem.Observation
    """
    # TODO this really shouldn't need to build a 'reading' to get the cutout...

    from ossos.downloads.cutouts import downloader
    dlm = downloader.ImageCutoutDownloader()

    mpc_obs = deepcopy(mpc_in)
    assert isinstance(mpc_obs, mp_ephem.ephem.Observation)
    assert isinstance(mpc_obs.comment, mp_ephem.ephem.OSSOSComment)

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

    ast_header = _connection_error_wrapper(storage._get_sghead, int(expnum))[int(ccd)+1]

    filter_value = None
    for keyword in ['FILTER', 'FILT1 NAME']:
       filter_value = ast_header.get(keyword, None)
       if filter_value is not None:
         if filter_value.startswith('gri'):
             filter_value = 'w'
         else:
             filter_value = filter_value[0]
         break
    # The ZP for the current astrometric lines is the pipeline one.  The new ZP is in the astheader file.
    new_zp = ast_header.get('PHOTZP')

    # The .zeropoint.used value is likely the one used for the original photometry.
    old_zp = _connection_error_wrapper(storage.get_zeropoint, int(expnum), int(ccd))

    reading = astrom.SourceReading(float(mpc_obs.comment.x), float(mpc_obs.comment.y), float(mpc_obs.comment.x),
                                   float(mpc_obs.comment.y), mpc_obs.coordinate.ra.degree,
                                   mpc_obs.coordinate.dec.degree, float(mpc_obs.comment.x), float(mpc_obs.comment.y),
                                   observation, ssos=True, from_input_file=True, null_observation=False,
                                   discovery=mpc_obs.discovery)

    cutout = _connection_error_wrapper(dlm.download_cutout, reading, needs_apcor=True)
    cutout._zmag = new_zp

    if math.fabs(cutout.zmag - old_zp) > 0.3:
        logging.warn("Large change in zeropoint detected: {}  -> {}".format(old_zp, cutout.zmag))

    try:
        PHOT = cutout.get_observed_magnitude(centroid=not skip_centroids and mpc_obs.note1 != "H")
        x = PHOT['XCENTER']
        y = PHOT['YCENTER']
        mag = PHOT['MAG']
        merr = PHOT['MERR']
        cutout.update_pixel_location((x, y), hdu_index=cutout.extno)
        x, y = cutout.observed_source_point
    except Exception as ex:
        logging.error("ERROR: {}".format(str(ex)))
        return mpc_obs

    try:
        if mpc_obs.comment.mag_uncertainty is not None and mpc_obs.comment.mag is not None and math.fabs(mpc_obs.comment.mag - mag) > 3.5 * mpc_obs.comment.mag_uncertainty:
           logging.warn("recomputed magnitude shift large: {} --> {}".format(mpc_obs.mag, mag[0]))
        if math.sqrt((x.value - mpc_obs.comment.x) ** 2 + (y.value - mpc_obs.comment.y) ** 2) > 1.9:
            logging.warn("Centroid shifted ({},{}) -> ({},{})".format(mpc_obs.comment.x,
                                                                      mpc_obs.comment.y,
                                                                      x.value,
                                                                      y.value))
    except Exception as ex:
        logging.error(str(ex))

    # Don't use the new X/Y for Hand measured entries.  (although call to get_observed_magnitude should have changed)
    if str(mpc_obs.note1) != "H" and not skip_centroids:
        mpc_obs.comment.x = x.value
        mpc_obs.comment.y = y.value

    try:
        mag = float(mag)
    except:
        return mpc_obs

    if math.isnan(mag):
        return mpc_obs

    if mag > 10:
        mpc_obs._band = filter_value
        mpc_obs.comment.mag = mag
        mpc_obs.comment.mag_uncertainty = merr

    # Update the mpc record magnitude if previous value existed here.
    if (mpc_obs.mag is not None or (mpc_obs.mag is None and mpc_in.comment.photometry_note[0] == "Z")) and mag > 10:
        mpc_obs.mag = mag

    return mpc_obs


def run(mpc_file, cor_file, 
        skip_discovery=True, skip_mags=False, 
        skip_centroids=False, compare_orbits=False):
    """

    :param mpc_file: A file containing the astrometric lines to be updated.
    :param cor_file: The base name for the updated astrometry and diagnostic files.
    :param skip_mags: Should we skip recomputing the magnitude of sources?
    :return: :raise ValueError: If actions on the mpc_obs indicate this is not a valid OSSOS observations
    """
    observations = mp_ephem.EphemerisReader().read(mpc_file)
    logging.debug("Read in Observations: {}".format(observations))
    original_obs = []
    modified_obs = []
    logging.info("ASTROMETRY FILE: {} --> {}.tlf".format(mpc_file, cor_file))
    for mpc_in in observations:
      try:
        if not isinstance(mpc_in.comment, mp_ephem.ephem.OSSOSComment):
            logging.info(type(mpc_in.comment))
            logging.info("Skipping: {}".format(mpc_in.to_string()))
            continue
        if ((skip_discovery and mpc_in.discovery) or
                (not skip_discovery and not mpc_in.discovery)):
            logging.info("Discovery mis-match")
            logging.info("Skipping: {}".format(mpc_in.to_string()))
            continue
        logging.info("="*220)
        logging.info("   orig: {}".format(mpc_in.to_string()))
        if mpc_in.comment.astrometric_level == 4:
            logging.info("Already at maximum AstLevel, skipping.")
            continue
        if mpc_in.null_observation:
            logging.info("Skipping NULL observation.")
            continue
        mpc_obs = remeasure(mpc_in)
        logging.info("new wcs: {}".format(mpc_obs.to_string()))

        if not skip_mags:  
            # and not mpc_obs.comment.photometry_note[0] == "Z":
            mpc_mag = remeasure(recompute_mag(mpc_obs,
                                              skip_centroids=skip_centroids),
                                reset_pixel_coordinates=not skip_centroids)
        else:
            mpc_mag = mpc_obs

        sep = mpc_in.coordinate.separation(mpc_mag.coordinate)
        if sep > TOLERANCE:
            logging.error("Large offset: {} arc-sec".format(sep))
            logging.error("orig: {}".format(mpc_in.to_string()))
            logging.error(" new: {}".format(mpc_mag.to_string()))
            new_comment = "BIG SHIFT HERE"
            mpc_mag.comment.comment = mpc_mag.comment.comment + " " + new_comment
        logging.info("new cen: {}".format(mpc_mag.to_string()))
        original_obs.append(mpc_in)
        modified_obs.append(mpc_mag)
        logging.info("="*220)
      except:
        logging.error("Skipping: {}".format(mpc_in))

    optr = open(cor_file + ".tlf", 'w')
    for idx in range(len(modified_obs)):
        inp = original_obs[idx]
        out = modified_obs[idx]
        if inp != out:
            optr.write(out.to_tnodb()+"\n")
    optr.close()

    if not compare_orbits:
        return True
    try:
       compare_orbits(original_obs, modified_obs, cor_file)
    except Exception as ex:
       logging.error("Orbit comparison failed: {}".format(ex))
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
        merr_str = ""
        for filter in mags:
            mag = numpy.percentile(numpy.array(mags[filter]), (50))
            mags[filter] = numpy.percentile(numpy.array(mags[filter]), (5,95))
            merr = (mags[filter][1] - mags[filter][0])/6.0
            merr_str += " {}: {:8.2f} +/- {:8.2f}".format(filter, mag, merr)
        orbpt.write("ra_std:{:8.4} dec_std:{:8.4} mag: {}".format(dra.std(), ddec.std(), merr_str))
    orbpt.write("\n")
    orbpt.close()


def main():
    description = """This program takes as input a set of OSSOS measurements and adjusts the astrometric and photometric
    entries to be consistent with the current best estimate for the astrometric and photometric calibrations.
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('ast_file', help="An MPC file to update.")
    parser.add_argument('--discovery', help="Only process the discovery images.", action='store_true', default=False)
    parser.add_argument('--result_base_name', help="base name for remeasurement results (defaults to basename of input)",
                        default=None)
    parser.add_argument('--skip-mags', action="store_true", help="Recompute magnitudes.", default=False)
    parser.add_argument('--skip-centroids', action="store_true", help="Recompute centroids.", default=False)
    parser.add_argument('--compare-orbits', action='store_true', help="Compute/Compare pre and post remeasure orbits?", default=False)
    parser.add_argument('--debug', action='store_true')

    args = parser.parse_args()

    level = logging.INFO
    if args.debug:
        level = logging.DEBUG

    import coloredlogs
    logger = logging.getLogger('update_astrom')
    coloredlogs.install(level=level)

    if args.result_base_name is None:
        base_name = os.path.splitext(os.path.basename(args.ast_file))[0]
    else:
        base_name = args.result_base_name
    run(args.ast_file, base_name, 
        skip_discovery=not args.discovery, 
        skip_mags=args.skip_mags, 
        skip_centroids=args.skip_centroids, 
        compare_orbits=args.compare_orbits)


if __name__ == '__main__':
    sys.exit(main())

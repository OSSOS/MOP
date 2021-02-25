# !/usr/bin/env python

__author__ = "Michele Bannister <micheleb@uvic.ca>"

import sys
import argparse
import shlex
from subprocess import Popen
import datetime

import pyfits

import sqlalchemy as sa
from ossos import storage
import web.field_obs.queries

"""
Initial table populating for the 'ossuary' database.
Currently assumes your tables are already instantiated in the database that you're connecting to.
25 April 2013  NRC-Herzberg
"""


def wanted_keys(header):
    # strip header into consistency with desired format for ossuary 'images' table
    retdir = {}

    for key, val in list(header.items()):
        if key in ['FILENAME', 'EXPTIME', 'QRUNID', 'AIRMASS']:
            retdir[key.lower()] = val

    retdir['image_id'] = header['EXPNUM']
    retdir['cfht_field'] = header['OBJECT']
    retdir['mjd_start'] = header['MJDATE']
    retdir['mjd_end'] = header['MJDEND']
    # have to combine the date info for the end of the exposure
    retdir['obs_start'] = header['DATE-OBS'] + ' ' + header['UTIME']
    retdir['obs_end'] = header['DATE']
    retdir['exptime'] = datetime.timedelta(seconds=header['EXPTIME'])
    retdir['moon_up'] = header['MOONUP']
    retdir['moon_phase'] = header['MOONPHAS']
    retdir['moon_angle'] = header['MOONANGL']
    retdir['crval_ra'] = header['CRVAL1']
    retdir['crval_dec'] = header['CRVAL2']
    # retdir['crpix_ra'] = header['CRPIX1']
    # retdir['crpix_dec'] = header['CRPIX2']

    return retdir


# For reference: header keywords we care about:
# OBJECT  # the field it thinks it's looking at. eg 'O+0-1   '
# FILENAME # '1616934o' / Base filename at acquisition: header has a .head suffix
# DATE    # '2013-04-10T13:47:33' / UTC Date of file creation
# DATE-OBS= '2013-04-10'         / Date at start of observation (UTC)
# UTIME   = '13:42:40.03'        / Time at start of observation (UTC)
# EXPNUM  # eg. 1616934 / CFHT odometer number
# EXPTIME #             287.078 / Measured integration time (seconds)
# QRUNID  = '13AQ05  ' # when the data are taken: use for identifying runs in the same lunation
# MJDATE  =        56392.5712966 / Modified Julian Date at start of observation
# MJDEND  =        56392.5751651 / Modified Julian Date at end of observation
# OBJRA   = '16:00:05.62'        / Target right ascension CHECK these are what Stephen updates
# OBJDEC  = '-13:28:46.9'        / Target declination
# AIRMASS =                1.213 / Airmass at start of observation
# MOONANGL=               141.50 / Angle from object to moon at start in degrees
# MOONPHAS=                 0.01 / Moon phase @ 0 HST, 0..1 new>full, -1..0 >new
# MOONUP  = 'False   '  / Moon up? True or False
# and Stephen informs me that the updated WCS can be found in:
# CRVAL1  # / WCS Ref value (RA in decimal degrees)
# CRVAL2  # / WCS Ref value (DEC in decimal degrees)
# and these (he adds them)
# CRPIX1
# CRPIX2
# CD[1..2]_[1..2]
# PV[1..2]_[1..10]


def verify_ossos_image(header):
    # confirm that the image is processed and satisfies necessary parameters
    # do not add to database any of these objects that were observed for followup on OSSOS time
    verboten = ['2004 KV18', '2008 LC18', '2009 MS9', 'PlutoHazard']
    for obj in verboten:
        assert header['OBJECT'] != obj, \
            'Observation of %s rather than OSSOS target. Skipping.' % obj
        # did we receive the requested integration?
    assert (abs(header['EXPTIME'] - header['EXPREQ']) < 5.), \
        'Requested %d s exposure, took %d exposure' % (header['EXPTIME'], header['EXPREQ'])
    # integration should be within ~10 sec of: normal field: 287 s, wallpaper: 30 s, long nail: 387 s
    # from 2015 onward: normal field 300 s, long nail 400 s.
    # u-band: 320 s, deep 15BM fields in 2014B: 500 s
    assert (280. < float(header['EXPTIME']) < 297.) \
           or (25. < float(header['EXPTIME']) < 35.) \
           or (380. < float(header['EXPTIME']) < 397.) \
           or (315. < float(header['EXPTIME']) < 325.) \
           or (495. < float(header['EXPTIME']) < 505.) \
           or (295. < float(header['EXPTIME']) < 305.), \
        'Exposure %s s, not in OSSOS range.' % header['EXPTIME']

    assert (header['FILTER'] in ['r.MP9601', 'r.MP9602']), 'Filter not OSSOS. Instead %s' % header['FILTER']
    assert field_in_survey_footprint(header), 'Field not in survey footprint.'
    # does calibrator exist?
    # check against qrunid in header to confirm it used the right dark flat.

    return


def field_in_survey_footprint(header):
    # as the fields precess according to Keplerian shear, required footprint is time-dependent.
    # but it doesn't move that far in four years.
    retval = True

    return retval


def get_iq_and_zeropoint(image, header_extract):
    # Options: it can be in the image tag, it can be in the file if that's been created.
    fwhm = storage.get_tag(image, 'fwhm_22')  # 22 is the standard chip: mid lower
    if fwhm is None:  # not in tag, maybe in the file: does that exist?
        sys.stdout.write('...no fwhm vtag. Instead trying file. ')
        try:
            fwhm = storage.get_fwhm(image, ccd=22)
        except:  # no file yet then either
            fwhm = None
            sys.stdout.write('...fwhm not yet measured. ')

    if fwhm is not None:
        iq = float(fwhm) * 0.1850  # plate scale is 0.1850 arcsec/pixel
        header_extract['iq_ossos'] = iq
    else:
        header_extract['iq_ossos'] = None  # This will need to be updated in the future, then.
    sys.stdout.write('Storing IQ: %s\n' % str(header_extract['iq_ossos']))

    zeropt = storage.get_tag(image, 'zeropoint_22')
    if zeropt is None:
        sys.stdout.write('...no zeropoint vtag. Instead trying file. ')
        try:
            zeropt = storage.get_zeropoint(image, ccd=22)
        except:  # no file yet then either
            zeropt = None
            sys.stdout.write('...zeropoint not yet measured. ')

    header_extract['zeropt'] = zeropt
    sys.stdout.write('Storing zeropoint: %s\n' % str(header_extract['zeropt']))

    return header_extract


def parse_sgwn_comments():
    retval = {}
    lines = storage.vofile('vos:sgwyn/tkBAD').read().splitlines()
    for line in lines:
        ln = line.partition(' ')
        retval[ln[0]] = ln[2]

    return retval


def get_snr(image, header_extract):
    snr = storage.get_tag(image, 'snr_13')
    header_extract['snr'] = snr

    return header_extract


def retrieve_processed_images(ims):
    ss = sa.select([ims.images.c.image_id, ims.images.c.iq_ossos], order_by=ims.images.c.image_id)
    query = ims.conn.execute(ss)
    retval = []
    retval2 = []
    for s in query:
        if isinstance(s[0], int):
            retval.append(s[0])
            retval2.append(s[1])
            # retval = [s[0] for s in query if isinstance(s[0], long)]
    # retval2 = [s[1] for s in query if isinstance(s[0], long)]

    return retval, retval2


def iq_unmeasured_images(ims):
    ss = sa.select([ims.images.c.image_id, ims.images.c.iq_ossos, ims.images.c.snr], order_by=ims.images.c.image_id)
    ss.append_whereclause(ims.images.c.iq_ossos == None)
    query = ims.conn.execute(ss)
    retval = [s[0] for s in query if isinstance(s[0], int)]

    return retval


def snr_unmeasured_images(ims):
    ss = sa.select([ims.images.c.image_id, ims.images.c.snr], order_by=ims.images.c.image_id)
    ss.append_whereclause(ims.images.c.snr == None)  # RERUN THIS AGAIN WITH SNR==0.74 TO CATCH THE FAILURES
    query = ims.conn.execute(ss)
    retval = [s[0] for s in query if isinstance(s[0], int)]

    return retval


def parse_unprocessed_images(dbimages, processed_images):
    # Ensure the unprocessed list doesn't include the calibrators/moving folders
    retval = [im for im in dbimages if (im.isdigit() and int(im) not in processed_images)]
    retval.sort()

    return retval


def get_header(image):
    retval = None
    header_text = storage.get_astheader(image, 0)  # zeroth ccd: first see if the preproc'd version is there
    if header_text is None:
        header_text = storage.get_astheader(image, 0, version='o')  # 'o' for an object acquisition
    if header_text is not None:  # checked twice, it must be there now if it's there at all...
        retval = wanted_keys(header_text)

    return retval, header_text


def put_image_in_database(image, ims):
    # first check if the key is already present, if so remove and re-add the entry
    # (don't know what field has been updated.)
    ss = sa.select([ims.images.c.image_id])
    ss.append_whereclause(ims.images.c.image_id == image['image_id'])
    query = ims.conn.execute(ss)

    if len([s[0] for s in query]) > 0:
        ss = sa.delete(ims.images.c.image_id == image['image_id'])
        query = ims.conn.execute(ss)

    ins = ims.images.insert(values=image)
    ims.conn.execute(ins)

    return


def update_values(ims, image_id, iq_zeropt=True, comment=False, snr=False, commdict=None):
    """
    Update a row in ossuary with
    :param ims: an ImageQuery, contains image table and a connector
    :param image_id: the primary key of the row to be updated
    :param iq_zeropt: Keyword set if iq and zeropoint are to be checked for updating
    :param comment: Keyword set if image is to have a comment of Stephen's added
    :param commdict: The dictionary parsed from Stephen's file of comments
    :return: No return, just updates ossuary.
    """
    updating_params = {}
    if iq_zeropt:
        updating_params = get_iq_and_zeropoint(image_id, {})
    if comment:
        updating_params = {'comment': commdict[str(image_id)]}
    if snr:
        updating_params = get_snr(image_id, {})
    ss = ims.images.update(ims.images.c.image_id == image_id)
    ims.conn.execute(ss, updating_params)

    return


def generate_MegaCam_previews(image_id):
    # modified from script by Chris Willott
    fitsfile = storage.vofile(storage.DBIMAGES + '/' + image_id)
    prev1file = "%s_preview_256.jpg" % (image_id)
    prev2file = "%s_preview_1024.jpg" % (image_id)
    prev3file = "%s_preview_zoom_1024.jpg" % (image_id)

    prim_hdr = pyfits.open(fitsfile, "update")
    numextens = prim_hdr[0].header['NEXTEND']

    cmd3 = "ds9 -mosaicimage wcs  %s   -geometry 258x505 -rotate 180 -scale squared -scale mode zscale " \
           "-scale scope global -scale datasec yes  -invert -mode no -view colorbar no  -zoom to fit  " \
           "-saveimage jpeg  %s -quit" % (fitsfile, prev1file)
    cmd4 = "ds9 -mosaicimage wcs  %s  -geometry 1026x1273 -rotate 180 -scale squared -scale mode zscale " \
           "-scale scope global -scale datasec yes -invert -view colorbar no  -zoom to fit " \
           "-saveimage jpeg  %s -quit" % (fitsfile, prev2file)

    # Sometimes MegaCam images have fewer than 23 chips available.
    if numextens >= 23:
        cmd5 = "ds9 -fits  %s[23] -pan -9 1780 image -geometry 1026x1273  -scale squared -scale mode zscale " \
               "-scale scope global -scale datasec yes -invert -mode no -view colorbar no -zoom 1  " \
               "-saveimage jpeg  %s -quit" % (fitsfile, prev3file)
    elif numextens >= 14:
        cmd5 = "ds9 -fits  %s[14] -pan -9 1780 image -geometry 1026x1273  -rotate 180 -scale squared " \
               "-scale mode zscale -scale scope global -scale datasec yes -invert -mode no -view colorbar no " \
               "-zoom 1 -saveimage jpeg  %s -quit" % (fitsfile, prev3file)
    else:
        cmd5 = "ds9 -fits  %s[1] -pan -9 1780 image -geometry 1026x1273  -rotate 180 -scale squared " \
               "-scale mode zscale -scale scope global -scale datasec yes -invert -mode no -view colorbar no " \
               "-zoom 1 -saveimage jpeg  %s -quit" % (fitsfile, prev3file)

    # Start ds9 processes but kill ds9 if takes longer than 60 seconds
    for cmdnum in [cmd3, cmd4, cmd5]:
        args = shlex.split(cmdnum)
        p = Popen(args)
        pid = p.pid
        # need to add a timeout after 60 sec if image isn't opened by ds9 in that time

    return


def main():
    """
    Update the ossuary Postgres db with images observed for OSSOS.
    iq: Go through and check all ossuary's images for new existence of IQs/zeropoints.
    comment: Go through all ossuary and
    Then updates ossuary with new images that are at any stage of processing.
    Constructs full image entries, including header and info in the vtags, and inserts to ossuary.

    TODO: a CLUSTER after data is inserted - maybe once a week, depending how much there is
    CLUSTER images;   need to sqlalchemy this one
    """
    parser = argparse.ArgumentParser()

    parser.add_argument("-iq", "--iq", action="store_true",
                        help="Check existing images in ossuary that do not yet have "
                             "IQ/zeropoint information; update where possible.")
    parser.add_argument("-comment", action="store_true",
                        help="Add comments on images provided by S. Gwyn to database.")
    parser.add_argument("-snr", action="store_true",
                        help="Update existing images in ossuary for SNR info where that exists in a vtag.")
    args = parser.parse_args()

    images = web.field_obs.queries.ImagesQuery()
    processed_images, iqs = retrieve_processed_images(images)  # straight list of primary keys
    commdict = parse_sgwn_comments()

    if args.iq:
        unmeasured_iqs = iq_unmeasured_images(images)
        sys.stdout.write('%d images in ossuary; updating %d with new IQ/zeropoint information.\n' %
                         (len(processed_images), len(unmeasured_iqs)))
        for n, image in enumerate(unmeasured_iqs):  # it's in the db, so has already passed the other checks
            update_values(images, image)
            sys.stdout.write('%s %d/%d...ossuary updated.\n' % (image, n + 1, len(unmeasured_iqs)))

    if args.snr:
        unmeasured = snr_unmeasured_images(images)
        sys.stdout.write('%d images in ossuary; updating %d with new SNR information.\n' %
                         (len(processed_images), len(unmeasured)))
        for n, image in enumerate(unmeasured):  # it's in the db, so has already passed the other checks
            update_values(images, image, iq_zeropt=False, snr=True)
            sys.stdout.write('%s %d/%d...ossuary updated.\n' % (image, n + 1, len(unmeasured)))

    if args.comment:
        sys.stdout.write('%d images in ossuary; updating with new comment information.\n' %
                         len(processed_images))
        for image in list(commdict.keys()):
            if int(image) in processed_images:
                update_values(images, image, iq_zeropt=False, comment=True, commdict=commdict)
                sys.stdout.write('%s has comment...\n' % image)

    unprocessed_images = parse_unprocessed_images(storage.list_dbimages(), processed_images)
    sys.stdout.write('%d images in ossuary; updating with %d new in VOspace.\n' %
                     (len(processed_images), len(unprocessed_images)))

    for n, image in enumerate(unprocessed_images):
        sys.stdout.write('%s %d/%d ' % (image, n + 1, len(unprocessed_images)))
        try:
            subheader, fullheader = get_header(image)
            if subheader is not None:
                sys.stdout.write('Header obtained. ')
                verify_ossos_image(fullheader)
                header = get_iq_and_zeropoint(image, subheader)
                header = get_snr(image, header)
                if image in list(commdict.keys()):
                    header['comment'] = commdict[image]
                put_image_in_database(header, images)
                sys.stdout.write('...added to ossuary...\n')
                # generate_MegaCam_previews(image)
                # sys.stdout.write(' .gif preview saved.\n')
            else:
                sys.stdout.write('Header is not available: skipping.\n')
        except Exception as e:
            sys.stdout.write('... %s\n' % e)


if __name__ == "__main__":
    main()

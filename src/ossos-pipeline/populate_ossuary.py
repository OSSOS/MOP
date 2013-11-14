# Initial table populating for the 'ossuary' database.
# 
# MTB 25 April 2013  NRC-Herzberg

import sys, datetime
import sqlalchemy as sa
from astropy.io import fits
from cStringIO import StringIO
from web.field_obs.queries import ImagesQuery
from ossos import storage

"""
Currently assumes your tables are already instantiated in the database that you're connecting to.
"""


def wanted_keys(header):
    # strip header into consistency with desired format for ossuary 'images' table
    retdir = {}

    for key, val in header.items():
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
    assert (280. < float(header['EXPTIME']) < 297.) \
        or (25. < float(header['EXPTIME']) < 35.) \
        or (380. < float(header['EXPTIME']) < 397.), \
        'Exposure %s s, not in OSSOS range.' % header['EXPTIME']

    assert (header['FILTER'] == 'r.MP9601'), 'Filter not r. Instead %s' % header['FILTER']
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
        except: # no file yet then either
            zeropt = None
            sys.stdout.write('...zeropoint not yet measured. ')

    header_extract['zeropt'] = zeropt
    sys.stdout.write('Storing zeropoint: %s\n' % str(header_extract['zeropt']))

    return header_extract


def retrieve_processed_images(ims):
    ss = sa.select([ims.images.c.image_id, ims.images.c.iq_ossos], order_by=ims.images.c.image_id)
    query = ims.conn.execute(ss)
    retval = []
    retval2 = []
    for s in query:
        if isinstance(s[0], long):
            retval.append(s[0])
            retval2.append(s[1])
    # retval = [s[0] for s in query if isinstance(s[0], long)]
    # retval2 = [s[1] for s in query if isinstance(s[0], long)]

    return retval, retval2


def parse_unprocessed_images(dbimages):
    # Ensure the unprocessed list doesn't include the calibrators/moving folders
    retval = [im for im in dbimages if (im.isdigit() and long(im) not in processed_images)]
    retval.sort()

    return retval


def get_header(data_web_service_url, image):
    # pulling back the header: fast if you know the direct source
    # also this means it's definitely the original header that's being checked
    # 'o' for an object acquisition
#    hdr = "%s/%so.fits.fz?cutout=[0]" % (data_web_service_url, image)
#    header_text = fits.open(StringIO(storage.vospace.open(hdr, view='data').read()))[0].header
    header_text = storage.get_astheader(image, 0) # zeroth ccd
    if header_text is not None:  # sometimes this happens
        retval = wanted_keys(header_text)
    else:
        retval = None

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


############################ BEGIN MAIN #######################################
data_web_service_url = storage.DATA_WEB_SERVICE + "CFHT"
images = ImagesQuery()
processed_images, iqs = retrieve_processed_images(images)  # straight list of primary keys
dbimages = storage.list_dbimages()
unprocessed_images = parse_unprocessed_images(dbimages)

# Need to add updating of existing records when fwhm etc info are added
# unprocessed_images = []
# for i, im in enumerate(processed_images):
# 	if iqs[i] is None:
# 		unprocessed_images.append(im)

sys.stdout.write('%d images in ossuary; updating with %d new in VOspace.\n' %
                 (len(processed_images), len(unprocessed_images)))

# Construct a full image entry, including header and info in the vtags
for n, image in enumerate(unprocessed_images):  # earlier ones are PlutoHazard etc
    sys.stdout.write('%s %d/%d ' % (image, n + 1, len(unprocessed_images)))
    try:
        subheader, fullheader = get_header(data_web_service_url, image)
        if subheader is not None:
            sys.stdout.write('Header obtained. ')
            verify_ossos_image(fullheader)
            header = get_iq_and_zeropoint(image, subheader)
            put_image_in_database(header, images)
            sys.stdout.write('...added to ossuary.\n')
        else:
            sys.stdout.write('Header is not available: skipping.\n')
    except Exception, e:
        sys.stdout.write('... %s\n' % e)



# can do a CLUSTER after data is inserted - maybe once a week, depending how much there is

#CLUSTER images;  # need to sqlalchemy this one


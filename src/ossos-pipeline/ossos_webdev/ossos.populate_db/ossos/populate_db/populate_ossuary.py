# Initial table populating for the 'ossuary' database.
# 
# MTB 25 April 2013  NRC-Herzberg

import sys, datetime
import sqlalchemy as sa
from astropy.io import fits
from cStringIO import StringIO
from ossos.field_obs.queries import ImagesQuery
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
	# and these (he must add them)
	# CRPIX1
	# CRPIX2
	# CD[1..2]_[1..2]
	# PV[1..2]_[1..10]


def verify_ossos_image(header):
	# confirm that the image is processed and satisfies necessary parameters
	if ((abs(header['EXPTIME'] - header['EXPREQ']) < 5.)  # approx what integration we asked for
		and (280. < float(header['EXPTIME']) < 297.) # shouldn't differ more than ~10 sec.
		and (header['FILTER'] =='r.MP9601')    # it must be an r filter
		and header['FILENAME'].endswith('o')  # 'o' for an object acquisition
		and field_in_survey_footprint(header)):

		# does calibrator exist?
		# check against qrunid in header to confirm it used the right dark flat.

			print 'valid'

			return True
	else:
		print 'That is weird.'
		return False 


def field_in_survey_footprint(header):
	# as the fields precess according to Keplerian shear, required footprint is time-dependent.


	return True


def get_iq_and_zeropoint(image, header_extract):
	try:  # 22 is the standard chip: mid lower
		fwhm = float(storage.get_tag(image, 'fwhm_22'))
		if fwhm:
			iq = fwhm*0.1850  # plate scale is 0.1850 arcsec/pixel
			header_extract['iq_ossos'] = iq
	except Exception, e:
		raise e
	try:
		zeropt = float(storage.get_tag(image, 'zeropoint_22'))
		if zeropt:
			header_extract['zeropt'] = float(zeropt)
	except Exception, e:
		raise e

	return header_extract


def retrieve_processed_images(ims):
	ss = sa.select([ims.images.c.image_id], order_by=ims.images.c.image_id)
	query = ims.conn.execute(ss)
	retval = [s[0] for s in query if isinstance(s[0], long)]

	return retval


def parse_unprocessed_images(dbimages):
	# Ensure the unprocessed list doesn't include the calibrators/moving folders
	retval = [im for im in dbimages if (im.isdigit() and long(im) not in processed_images)]
	retval.sort()

	return retval


def get_header(data_web_service_url, image):
	# pulling back the header: fast if you know the direct source
	hdr = "%s/%so.fits.fz?cutout=[0]" % (data_web_service_url, image) 
	header_text = fits.open(StringIO(storage.vospace.open(hdr, view='data').read()))[0].header
	retval = wanted_keys(header_text)

	return retval 


def put_image_in_database(image, ims):
	ins = ims.images.insert(values=image)
	ims.conn.execute(ins)

	return



############################ BEGIN MAIN #######################################
data_web_service_url = storage.DATA_WEB_SERVICE+"CFHT"
images = ImagesQuery()
processed_images = retrieve_processed_images(images)  # straight list of primary keys
dbimages = storage.list_dbimages()
unprocessed_images = parse_unprocessed_images(dbimages)
sys.stdout.write('%d images in ossuary; updating with %d new in VOspace.\n' % 
	(len(processed_images), len(unprocessed_images)))

# Construct a full image entry, including header and info in the vtags
for n, image in enumerate(unprocessed_images):
	# if image == '1624879':  # GOOD TEST IMAGE
	try:
		sub = get_header(data_web_service_url, image)
		# if verify_ossos_image(header):
		header_extract = get_iq_and_zeropoint(image, sub)
		# 	put_image_in_database(header_extract, images)
		print header_extract
		sys.stdout.write('%s %d/%d\n' % (image, n+1, len(unprocessed_images)))

	except Exception, e:
		sys.stdout.write('%s %s\n' % (image, e))


# can do a CLUSTER after data is inserted - maybe once a week, depending how much there is

#CLUSTER images;  # need to sqlalchemy this one


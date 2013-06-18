# Initial table populating for the 'ossuary' database.
# 
# MTB 25 April 2013  NRC-Herzberg

import vos, datetime
from astropy.io import fits
from cStringIO import StringIO
from queries import retrieve_processed_images, put_image_in_database


def vospace_connector():
	# images are in http://www.canfar.phys.uvic.ca/vosui/#/OSSOS/dbimages/
	# eg. (symbolic link, need to be connected/certificate etc)
	# http://www.canfar.phys.uvic.ca/vosui/#/OSSOS/dbimages/1607615/1607615o.head 
	vospace = vos.Client()#cadc_short_cut=True)  # shortcut doesn't work due to symbolic links
	stem = 'vos:OSSOS/dbimages/'

	# pull back each directory in vos:OSSOS/dbimages/ and extract the .head file in it
	# could use listDir?
	vos_images = [n[0] for n in vospace.getInfoList(stem) if n[0].isdigit()]
	vos_images.sort()

	return vospace, stem, vos_images


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


def get_iq_and_zeropoint(vospace, hfile, image, header_extract):
	# go to vospace and retrieve iq and zeropoint.used
	try:  # image quality is in fwhm separate file - 22 the standard chip: mid lower
		fwhm_file = hfile+'/ccd22/'+image+'p22.fwhm'
		fwhm = float(vospace.open(fwhm_file,view='data').read())
		iq = fwhm*0.1850  # plate scale is 0.1850 arcsec/pixel
		header_extract['iq_ossos'] = iq
	except Exception, e:
		raise e
	try:
		zeropt_file = hfile+'/ccd22/'+image+'p22.zeropoint.used'  # the standard chip
		zeropt = vospace.open(zeropt_file,view='data').read()
		header_extract['zeropt'] = float(zeropt)
	except Exception, e:
		raise e

	return header_extract



############################ BEGIN MAIN #######################################

processed_images = retrieve_processed_images()  # straight list of primary keys
vospace, stem, unprocessed_images = vospace_connector()
print len(unprocessed_images), 'folders in vospace to check.'

for n, image in enumerate(unprocessed_images):
	if long(image) not in processed_images:  
		try: 
			hfile = stem + image + '/'
			header = fits.open(StringIO(vospace.open(hfile+image+'o.head',view='data').read()))[0].header
			header_extract = wanted_keys(header)
			print image, n+1,'/',len(unprocessed_images) #, header_extract 
			if verify_ossos_image(header):
				header_extract = get_iq_and_zeropoint(vospace, hfile, image, header_extract)
				put_image_in_database(header_extract)

		except Exception, e:
			print image, e


# can do a CLUSTER after data is inserted - maybe once a week, depending how much there is

#CLUSTER images;  # need to sqlalchemy this one


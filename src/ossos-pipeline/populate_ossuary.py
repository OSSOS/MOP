# Initial table populating for the 'ossuary' database.
# 
# MTB 25 April 2013  NRC-Herzberg

import vos
from astropy.io import fits
from cStringIO import StringIO
from sqlalchemy import *

# Development testing database on local machine provided by Postgres.App
# logs via logging
engine = create_engine('postgresql://localhost/ossuary', echo=True)  

# images are in http://www.canfar.phys.uvic.ca/vosui/#/OSSOS/dbimages/
# eg. (symbolic link, need to be connected/certificate etc)
# http://www.canfar.phys.uvic.ca/vosui/#/OSSOS/dbimages/1607615/1607615o.head 

# Header keywords we care about: 
# CMMTSEQ # eg. 'North[1/1] - Ref + [0.0 90.0]'
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

# plus: confirm when adding data on images that:
# EXPREQ ~= EXPTIME 
# and that FILTER = 'r.MP9601' 
# and that the last character of FILENAME is 'o' for an object acquisition
# and that the obs ra/dec are within the survey footprint.

# need to pull back each directory in vos:OSSOS/dbimages/ and extract the .head file in it
vls vos:OSSOS/dbimages/

vospace_url = 'vos:OSSOS/dbimages/1616499/1616499o.head'


# can do a CLUSTER after data is inserted - maybe once a week, depending how much there is
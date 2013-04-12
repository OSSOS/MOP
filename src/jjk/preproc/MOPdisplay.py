#!/usr/bin/python

### Display a paritcular image[RA/DEC] area


from optparse import OptionParser
import MOPdbaccess, MOPfits
import logging
import sys,os
logging.basicConfig()
logger=logging.getLogger(sys.argv[0])




parser=OptionParser()

parser.add_option("--file","-f",type="string",help="file to display in ds9 display")
parser.add_option("--keep","-k",action="store_true",help="Don't delete already displayed stuff")
parser.add_option("--objectID","-o",type="int",help="Object ID to display images of")
parser.add_option("--sourceID","-s",type="int",help="Source ID to display image of")
parser.add_option("--verbose","-v",action="store_true",help="Tell you what I'm doing.")
parser.add_option("--debug","-d",action="store_true",help="Tell you how I'm doing.")
parser.add_option("--width","-w",type="int",help="set the width.",default=150)
parser.add_option("--table","-t",type="string",help="set the width.",default="objects_list")


(opt,args)=parser.parse_args()

if opt.verbose: logger.setLevel(logging.INFO)
if opt.debug: logger.setLevel(logging.DEBUG)

def moreData(ra,dec,box):
	"""Search the CFHT archive for more images of this location"""

	import cfhtCutout
	cdata={'ra_deg': ra, 'dec_deg': dec, 'radius_deg': 0.2}
	inter=cfhtCutout.find_images(cdata,0.2)

def xpacheck():
	"""Check if xpa is running"""
	import os
	f=os.popen('xpaaccess ds9')
	l=f.readline()
	f.close()
	if l.strip()!='yes':
   		logger.debug("\t Can't get ds9 access, xpaccess said: %s" % ( l.strip()))
		return (False)
	return(True)

def newFrame():
	"""Create a ds9 Frame"""
	os.system('xpaset -p ds9 frame new')
	
def scale():
	"""set the scale of the current frame to inverted zscale"""
    	os.system('xpaset -p ds9 scale zscale on')
    	os.system('xpaset -p ds9 cmap invert yes')
	os.system('xpaset -p ds9 scale datasec no')
	return

def align():
	"""set the alignment to wcs"""
	os.system('xpaset -p ds9 wcs align yes')
	return

def deleteFrames(frame='all'):
	"""delete all frames"""
	os.system('xpaset -p ds9 frame delete %s' % ( str(frame)))
	return

def mark(x,y,label=None):
	"""Mark a circle on the current image"""
	if label is not None: 
	    os.system("xpaset -p ds9 regions color red ")
	    cmd="echo 'image; text %d %d # text={%s}' | xpaset ds9 regions " % ( x,y,label) 
	else:
	    os.system("xpaset -p ds9 regions color blue")
	    cmd="echo 'image; circle %d %d 10 ' | xpaset ds9 regions " % (x,y)
	os.system(cmd)
	return
		 

def display(url):
	"""Display a file in ds9"""
	import os
	oscmd="curl --silent -g --fail --max-time 1800 -E ${HOME}/.ssl/cadcproxy.pem '%s'" % (url)
	logger.debug(oscmd)
    	os.system(oscmd+' | xpaset ds9 fits')
	return
	
if not xpacheck():
	if os.system("ds9 &"):
		sys.exit("\tCan't see or start ds9")
	while not xpacheck():
		logger.info("\tWaiting for ds9")

if opt.file:
	newFrame()
	scale()
	display("file:%s" % ( opt.file ) )
	sys.exit()
	
sql="""SELECT expnum,ccd,x_image,y_image FROM source s """

if opt.objectID and opt.table=='objects_list' :
	sql+="""JOIN objects_list oi ON s.sourceID=oi.sourceID WHERE objectID=%d""" % ( int(opt.objectID))
if opt.objectID and opt.table=='objects_moving' :
	sql+="""JOIN objects_moving oi ON s.sourceID=oi.partnerID WHERE objectID=%d""" % ( int(opt.objectID))

if opt.sourceID:
	sql+=""" WHERE s.sourceID=%d""" % ( int(opt.sourceID))

db=MOPdbaccess.connect('cfeps','cfhls',dbSystem='MYSQL')
logger.debug(sql)
c=db.cursor()
c.execute(sql)
rows=c.fetchall()
db.close()


import MOPfits

if not opt.keep:
    deleteFrames()
for row in rows:
	logger.debug(str(row))
	width=opt.width
	x=row[2]
	y=row[3]
	#if ( row[1]<18 ) :
	#   x=2112-x
	#   y=4644-y
	xmin=max(1,int(x)-width)
	xmax=min(2112,int(x)+width)
	ymin=max(1,int(y)-width)
	ymax=min(4644,int(y)+width)
	cutout="[%d:%d,%d:%d]" % ( xmin,xmax,ymin,ymax)
	newFrame()
	url = "http://test.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/authProxy/getData?archive=CFHT&cutout=[%d]%s&file_id=%s" % ( row[1]+1, cutout, str(row[0])+'p')
	print url
	display(url)
	mark(x-xmin+1,y-ymin+1)
	mark(40,-10,str(row[0]))
        align()
	scale()

sys.exit(0)

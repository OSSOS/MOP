#!python
from ossos import mpc
from ossos import storage
import sys
import requests
from os import path
import netrc

c = storage.vospace

VOSPACE_HOST = 'www.canfar.phys.uvic.ca'
BASEURL = "http://"+VOSPACE_HOST+"/vospace/auth/synctrans"

obj = path.splitext(path.basename(sys.argv[1]))[0]

lines = open("/Users/jjk/Dropbox/dbaseclone/ast/{}.ast".format(obj)).readlines()

o = []

try:
    c.mkdir("vos:OSSOS/postage_stamps/13AE/FITS/{}".format(obj))    
except IOError as e:
    if e.errno == 17:
        pass
    else:
        raise e


for line in lines:
   o = mpc.Observation.from_string(line)
   if o.null_observation:
	continue
   expnum = o.comment.frame.split('p')[0]
   cutout = "CIRCLE ICRS {} {} 0.02".format(o.coordinate.ra.degree,
                                            o.coordinate.dec.degree)
   target = "vos://cadc.nrc.ca~vospace/OSSOS/dbimages/{}/{}p.fits".format(expnum, expnum)
   direction = "pullFromVoSpace"
   protocol = "ivo://ivoa.net/vospace/core#httpget"
   view = "cutout"
   params = {"TARGET": target,
             "PROTOCOL": protocol,
             "DIRECTION": direction,
             "cutout": cutout,
             "view": view}

   (user, rhelm, password) = netrc.netrc().authenticators(VOSPACE_HOST)
   r = requests.get(BASEURL, params=params, auth=(user, password))
   filename = "{}_{:11.5f}_{:09.5f}_{:+09.5f}.fits".format(obj, o.date.mjd, o.coordinate.ra.degree, o.coordinate.dec.degree)
   f = open(filename,'w')
   f.write(r.content)
   f.close()
   print filename
   c.copy(filename,"vos:OSSOS/postage_stamps/13AE/FITS/{}/{}".format(obj, filename))


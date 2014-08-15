#!python
"""Create links in the postage stamp directory that allow retrieval of the FITS images associated with the OSSOS detections.

Takes a .ast file (in dbase format) as input

An Example URL for cutouts
http://www.canfar.phys.uvic.ca/vospace/auth/synctrans?TARGET=vos://cadc.nrc.ca~
vospace/OSSOS/dbimages/1625356/1625356p.fits&DIRECTION=pullFromVoSpace&PROTOCOL=
ivo://ivoa.net/vospace/core%23httpget&view=cutout&cutout=CIRCLE+ICRS+242.1318+-1
2.4747+0.05
"""
from ossos import mpc
from ossos import storage
import sys
import re
import os

dbimages = "http://www.canfar.phys.uvic.ca/data/pub/vospace/OSSOS/dbimages"

postage_stamp_directory = "vos:OSSOS/postage_stamps"
block_name = "13AE/FITS"
width = 256/2.0

c = storage.vospace

next_comment = None
obj_name = os.path.basename(sys.argv[1]).split(".")[0]
c.mkdir('{}/{}/{}'.format(postage_stamp_directory, block_name, obj_name))
for line in open(sys.argv[1]):
    try: 
        mpc_observation = mpc.Observation.from_string(line)
        if mpc_observation.null_observation:
            print line
            continue
        match = re.search('(\d+)\D(\d+)', mpc_observation.comment.frame)
        if match is None:
            print line
            continue
        expnum = match.group(1)
        ccd = int(match.group(2))
        X = float(mpc_observation.comment.x)
        Y = float(mpc_observation.comment.y)
        x1 = int(max(1,X - width))
        x2 = int(min(2112,X + width))
        y1 = int(max(1,Y - width))
        y2 = int(min(4644, Y + width))
        cutout = '[{}][{}:{},{}:{}]'.format(ccd, x1, x2, y1, y2)
        target = "{}/{}/{}p.fits?cutout={}".format(dbimages,
                                                   expnum,
                                                   expnum,
                                                   cutout)
        source = "{}/{}/{}/{}_{}.fits".format(postage_stamp_directory,
                                   block_name,
                                   obj_name, 
                                   obj_name,
                                   str(mpc_observation.date).replace(" ","_"))
        print "{} -> {}".format(source , target)
        c.link(target, source)
    except Exception as e:
        sys.stderr.write(str(e))


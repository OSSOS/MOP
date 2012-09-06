#!/usr/bin/env python 

import Tkinter
import PIL
import Image
import ImageTk
import pyfits
import numarray
import sys


def zscale(data,contrast,min=100,max=60000):
	"""Scale the data cube into the range 0-255"""

	## pic 100 random elements along each dimension
	## use zscale (see the IRAF display man page or
	## http://iraf.net/article.php/20051205162333315


	import random
	x=[]
	for i in random.sample(xrange(data.shape[0]),50):
	    for j in random.sample(xrange(data.shape[1]),50):
	 	x.append(data[i,j])
	
	yl=numarray.sort(numarray.clip(x,min,max))
	n=len(yl)
        ym=sum(yl)/float(n)

        xl=numarray.array(range(n))
 	xm=sum(xl)/float(n)	


	ss_xx=sum((xl-xm)*(xl-xm))
	ss_yy=sum((yl-ym)*(yl-ym))
        ss_xy=sum((xl-xm)*(yl-ym))
	b=ss_xy/ss_xx
	a=ym-b*xm

	z1=yl[n/2] + (b/contrast)*(1-n/2)
	z2=yl[n/2] + (b/contrast)*(n-n/2)

	## Now put the data inbetween Z1 and Z2
	high=data-z1
	z2=z2-z1
	high=numarray.clip(high,0,z2)

	## and change that to 0-255
	high= 256-256*high/z2

	### send back the scalled data
	return high

def flip(root,image):
        root.create_image(0,0,image=image,anchor="nw")
        return

def recen(self,event):
	return
	

import optparse

parser=optparse.OptionParser()
parser.add_option("--contrast",type="float",default=0.35,help="Image Contrast Level")
parser.add_option("--max",default=50000,type="float",help="Max Pixel Value")
parser.add_option("--min",default=-1000,type="float",help="Max Pixel Value")

(opt,files)=parser.parse_args(sys.argv[1:])

root=Tkinter.Tk()
w=Tkinter.Canvas(root,scrollregion=(0,0,800,600),confine=False,width=800,height=600)
#w.event("<Button>",recen)
w.pack()

im=[]
for filename in files:
    f=pyfits.open(filename)
    dat=f[1].data[-1:0:-4,0:-1:4]
    f.close()
    s=dat.shape
    d=zscale(dat,0.4,opt.min,opt.max)
    im.append(Image.frombuffer("L",(s[1],s[0]),d.astype("Int8")))

#imo = im[0] - im[1]
imo=Image.blend(im[0],im[1],-1.0)
ima=(ImageTk.PhotoImage(imo))

w.create_image(0,0,image=ima,anchor="nw")

root.mainloop()

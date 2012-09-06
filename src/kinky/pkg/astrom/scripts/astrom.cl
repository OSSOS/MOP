procedure astrom.cl(solncoef,pimgfile,pbox)

#  This program uses the computed plate solution to translate
#  pixel coordinates to RA and DEC.  You must display the
#  appropriate image before invoking this task.

string solncoef {"", prompt="file with plate solution?"}
string pimgfile {"", prompt="Image name to do centroid on?"}
int    pbox     {"", prompt="box size for centroiding?"}

begin
    string now, coeffile, command, pointfile
    string imgname, centerfile, lab0, lab1, lab2
    real x1, y1, xcen, ycen
    int wcs, box

#	cache("stsdas")
#	cache("stsdas.analysis")
	cache("gasp")
	cache("stsdas.gasp.xyeq")

	coeffile=solncoef
  	box = pbox
	pointfile=mktemp("tmp$pt")
	pnts:
           print("\n 'q'=quit, 'h'=here, otherwise centroid on this point ")
	   now= fscan(imcur, x1, y1, wcs, command)
	   if (command == "q") bye
           print(x1, y1, > pointfile)
#
# BRETT added the "iraf" variable to the following list, Mar 7/95. I think
# that this is necessary becuase the order of the parameters has changed.

	 if (command == "h") 
	 {
	   print(" \n *** NON-CENTROIDED ***********************************")
 	   xyeq(no,"",coeffile,pointfile,"iraf",1,2,0,ra_hours=yes)
           tvmark(1, pointfile, mark="circle",radii=5, label=yes, color=206)
           delete(pointfile, verify-)
	   print(" ******************************************************")
         }
	 else
	 {
           delete(pointfile, verify-)

#	   print("Image name to do centroid on?")
#	   scan(imgname)
	   print(" ")
	   imgname= pimgfile

	   centerfile = imgname//".cen"
	   imcntr(imgname,x1,y1,cboxsize=box, >> centerfile)
#	   print(x1,y1)
	   list = centerfile
	   now = fscan(list,lab0,lab1,xcen,lab2,ycen)
           delete(centerfile, verify-)
#	   print("xcen, ycen  ",xcen,ycen)
	   pointfile=mktemp("tmp$pt")
           print(xcen, ycen, > pointfile)
 	   xyeq(no,"",coeffile,pointfile,"iraf",1,2,0,ra_hours=yes)
           tvmark(1, pointfile, mark="circle",radii=9, label=yes, color=205)
           delete(pointfile, verify-)
         }
	 ;

	goto pnts
end

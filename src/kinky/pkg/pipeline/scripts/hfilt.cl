procedure hfilt(pimgfile)

#  This script display the first of 3 images hunted by the code
#  of hans, and allows the user to filter out via mouse commands on
#  the ximtool portions of the image with many false candidates.

string pimgfile {"", prompt="Image to filter (_result_ident)?"}

begin
    string imgname, centerfile, lab0, outfile
    string now, coeffile, command, pointfile, sl
    int cnts, n1, n2, n3, dum, nfound
    real x1, y1, xcen, ycen, xkeep,ykeep, length,ratio
    real sh1,sh2,xmin,xmax,ymin,ymax
    int wcs

	imgname= pimgfile

	coeffile = imgname + '_result_identif'
	list = coeffile
	pointfile=mktemp("tmp$pt")
	nfound = 0
	dum = 0
        while(fscan(list,lab0) != EOF) {
            dum = dum + 1
#	    print(" dum ",dum)
            if (dum == 14) goto getxy
        }
      getxy:
        if (fscan(list,x1,y1,cnts,n1,n2,n3,sh1,sh2,xcen,ycen) != EOF) {
	    nfound = nfound + 1
#            print(x1, y1)
            print(x1, y1, >> pointfile)
        }
	dum = 0
        while(fscan(list,lab0) != EOF) {
            dum = dum + 1
            if (dum == 3) goto getxy
        }

	print(" ")
	print(" Read ",nfound," coordinates; displaying image")
	set stdimage = "imt4096"
	display(imgname,1)
	tvmark(1,pointfile,mark='circle',radii='15',color=205)
        delete(pointfile, verify-)

	outfile = imgname + '.exclude'
	delete(outfile,verify-)
           
#-------------------------------------------------------------------
	pnts:
           print("********************************")
           print("* 'q'=quit,                    *")
           print("* 'n'=no more regions          *")
           print("* otherwise first corner point *")
           print("********************************")
           print("* >>IMCURSOR<< *")
           print("****************")
	   now= fscan(imcur, x1, y1, wcs, command)
	   xkeep = x1
	   ykeep = y1
	   if (command == "q") bye
	   if (command == "n") goto filter
	   pointfile=mktemp("tmp$pt")
           print(x1  ,   y1, > pointfile)
#           print(x1-1,   y1, >> pointfile)
#           print(x1+1,   y1, >> pointfile)
#           print(x1-1, y1+1, >> pointfile)
#           print(x1-1, y1+1, >> pointfile)
#           print(x1-1, y1+1, >> pointfile)
#           print(x1-1, y1-1, >> pointfile)
#           print(x1-1, y1-1, >> pointfile)
#           print(x1-1, y1-1, >> pointfile)
	   tvmark(1,pointfile,mark='point',pointsize=9,color=205)
           delete(pointfile, verify-)
           print("********************************")
           print("* 'q'=quit,                    *")
           print("* otherwise 2nd corner point   *")
           print("********************************")
           print("* >>IMCURSOR<< *")
           print("****************")
	   now= fscan(imcur, x1, y1, wcs, command)
	   if (command == "q") bye
	   pointfile=mktemp("tmp$pt")
           print(x1  ,   y1, > pointfile)
	   tvmark(1,pointfile,mark='point',pointsize=9,color=205)
           delete(pointfile, verify-)
           xmin = xkeep
           xmax = x1
           if (x1 < xmin) { 
                  xmin = x1 
                  xmax = xkeep 
           }
           ymin = ykeep
           ymax = y1
           if (y1 < ymin) { 
                  ymin = y1 
                  ymax = ykeep 
           }
	   xcen = (xmin + xmax)/2
	   ycen = (ymin + ymax)/2
	   length = xmax-xmin
	   ratio = (ymax-ymin)/length
	   sl = str(length) + ' ' + str(ratio)
#	   print(' xcen  ycen ',xcen,ycen)
#	   print(' length   ratio ',length,'  ',ratio)
#	   print(' sl ',sl)
	   pointfile=mktemp("tmp$pt")
           print(xcen, ycen, > pointfile)
	   tvmark(1,pointfile,mark='rectangle',lengths=sl,color=206)
	   length = xmax-xmin+1
	   sl = str(length) + ' ' + str(ratio)
	   tvmark(1,pointfile,mark='rectangle',lengths=sl,color=206)
	   length = xmax-xmin+2
	   sl = str(length) + ' ' + str(ratio)
	   tvmark(1,pointfile,mark='rectangle',lengths=sl,color=206)
           delete(pointfile, verify-)

	   print(xmin,xmax,ymin,ymax, >> outfile)

       goto pnts
#-----------------------------------------------------------------------
end

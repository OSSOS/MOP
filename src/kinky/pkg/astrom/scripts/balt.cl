#
#   balt.cl  
#
#  (B)rett's (al)ignment (t)ool
#

procedure bal(snumimg)

        int  snumimg  {3, prompt=" Number of images to align?" }
        int  sbox     {13, prompt=" Box size for centroiding? (pix) "}
begin 
	int    numimg

	string img1,img2,img3,img4
	string img[9]

	real   thisra,thisdec,thismag,thisid,scale

        real   x1,y1,x2,y2,x3,y3,x4,y4,xnow,ynow
        int    wcs
        string now, command, tvfile, shiftfile, centerfile, pointfile
	string rawnames

        string lab0,lab1,lab2
	real   xcen,ycen,box

	print(" ")
	print(" ") 
	print(" ********************************") 
	print(" * (B)rett's (al)ignment (t)ool *")
	print(" ********************************") 
	print(" ") 

        numimg = snumimg
	box = sbox

	if(numimg > 4) print(" PANIC...ONLY WORKS FOR 2--4 images FOR NOW")

!ls *.fits
        print(" ")
        print(" Enter name of reference image ")
        scan(img1)
###        print(" Displaying reference image")
        display(img1,1)
        print(" Enter name of image 2")
        scan(img2)
###        print(" Displaying image 2")
        display(img2,2)
	if (numimg>2) {
          print(" Enter name of image 3")
          scan(img3)
          display(img3,3)
	}
	if (numimg>3) {
          print(" Enter name of image 4")
          scan(img4)
          display(img4,4)
	}

        display(img1,1)
        print(" Find an easily locatable bright star that is ")
        print(" visible on all images and MARK IT ON FRAME 1")
        print('******************')
	print('* Q - quit       *'
	print('* any other-mark *'
        print('*  >>IMCURSOR<<  *')
        print('******************')
        now= fscan(imcur, x1, y1, wcs, command)
        if (command == "Q") bye
        tvfile=mktemp("tmp$tvfile")
	print(x1,y1, > tvfile)

        centerfile = img1//".baltcen"
        imcntr(img1,x1,y1,cboxsize=box, >> centerfile)
        list = centerfile
        now = fscan(list,lab0,lab1,xcen,lab2,ycen)
        print("star centroid : ",xcen,ycen)
#       mark for reference to not accidentally re-chosen later
        pointfile=mktemp("tmp$pt")
        print(xcen, ycen, > pointfile)
        tvmark(1,pointfile,mark='circle',radii='18,22',color=205)
	x1 = xcen
	y1 = ycen
        delete(pointfile, verify-)
        delete(centerfile, verify-)
        delete(tvfile, verify-)

	disp(img2,2)
        print(" Mark the same star ON FRAME 2")
        print('******************')
	print('* Q - quit       *'
	print('* any other-mark *'
        print('*  >>IMCURSOR<<  *')
        print('******************')
        now= fscan(imcur, x2, y2, wcs, command)
        if (command == "Q") bye
        centerfile = img2//".baltcen"
        imcntr(img2,x2,y2,cboxsize=box, >> centerfile)
        list = centerfile
        now = fscan(list,lab0,lab1,xcen,lab2,ycen)
##        print("star centroid : ",xcen,ycen)
        pointfile=mktemp("tmp$pt")
        print(xcen, ycen, > pointfile)
	x2 = xcen
	y2 = ycen
        tvmark(2,pointfile,mark='circle',radii='18,22',color=205)
        delete(pointfile, verify-)
        delete(centerfile, verify-)
        delete(tvfile, verify-)

	if (numimg>2) {
	  disp(img3,3)
          print(" Mark the same star ON FRAME 3")
          print('******************')
	  print('* Q - quit       *'
	  print('* any other-mark *'
          print('*  >>IMCURSOR<<  *')
          print('******************')
          now= fscan(imcur, x3, y3, wcs, command)
          if (command == "Q") bye
          centerfile = img3//".baltcen"
          imcntr(img3,x3,y3,cboxsize=box, >> centerfile)
          list = centerfile
          now = fscan(list,lab0,lab1,xcen,lab2,ycen)
          pointfile=mktemp("tmp$pt")
          print(xcen, ycen, > pointfile)
	  x3 = xcen
	  y3 = ycen
          tvmark(3,pointfile,mark='circle',radii='18,22',color=205)
          delete(pointfile, verify-)
          delete(centerfile, verify-)
          delete(tvfile, verify-)
	}

	if (numimg>3) {
	  disp(img4,4)
          print(" Mark the same star ON FRAME 4")
          print('******************')
	  print('* Q - quit       *'
	  print('* any other-mark *'
          print('*  >>IMCURSOR<<  *')
          print('******************')
          now= fscan(imcur, x4, y4, wcs, command)
          if (command == "Q") bye
          centerfile = img4//".baltcen"
          imcntr(img4,x4,y4,cboxsize=box, >> centerfile)
          list = centerfile
          now = fscan(list,lab0,lab1,xcen,lab2,ycen)
          pointfile=mktemp("tmp$pt")
          print(xcen, ycen, > pointfile)
	  x4 = xcen
	  y4 = ycen
          tvmark(4,pointfile,mark='circle',radii='18,22',color=205)
          delete(pointfile, verify-)
          delete(centerfile, verify-)
          delete(tvfile, verify-)
	}
	
!/bin/rm shifts
	shiftfile = 'shifts'
	print('   0.0   0.0 ',> shiftfile)
	print( (x1-x2),(y1-y2), >> shiftfile)
	if (numimg>2) {
	print( (x1-x3),(y1-y3), >> shiftfile)
	}
	if (numimg>3) {
	print( (x1-x4),(y1-y4), >> shiftfile)
	}

	disp(img1,1)
        tvfile=mktemp("tmp$tvfile")
	print(x1,y1, >> tvfile)
	tvmark(1,tvfile,mark='circle',radii='20,22',color=205)

	print(' ')
	print(' -- Now mark as many stars as you wish on frame 1')
	print(' ')
        print('******************')
	print('* Q - quit       *'
	print('* D - DONE       *'
	print('* any other-mark *'
        print('*  >>IMCURSOR<<  *')
        print('******************')
loop:
        now= fscan(imcur, xnow, ynow, wcs, command)
        if (command == "Q") bye
        if (command == "D") goto done
        centerfile = img1//".baltone"
        imcntr(img1,xnow,ynow,cboxsize=box, >> centerfile)
        list = centerfile
        now = fscan(list,lab0,lab1,xcen,lab2,ycen)
        pointfile=mktemp("tmp$pt")
        print(xcen, ycen, > pointfile)
        print(xcen, ycen, >> tvfile)
	tvmark(1,pointfile,mark='circle',radii='14',color=204)
        delete(pointfile, verify-)
        delete(centerfile, verify-)
	goto loop
done:
	print(" Calling imalign ")
	if (numimg==2) {
	  rawnames=img1//','//img2
	  imalign(rawnames,img1,tvfile,'a1,a2',shifts=shiftfile)
	}
	if (numimg==3) {
	  rawnames=img1//','//img2//','//img3
	  imalign(rawnames,img1,tvfile,'a1,a2,a3',shifts=shiftfile)
	}
	if (numimg==4) {
	  rawnames=img1//','//img2//','//img3//','//img4
	  imalign(rawnames,img1,tvfile,'a1,a2,a3,a4',shifts=shiftfile)
	}

	display("a1",1)
	display("a2",2)
	if (numimg>2) {
	   display("a3",3)
        }
	if (numimg>3) {
	   display("a4",4)
        }

	print(" ")
	print(" Done ")
 
end

	

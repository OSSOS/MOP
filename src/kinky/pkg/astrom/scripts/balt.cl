#
#   balt.cl  
#
#  (B)rett's (al)ignment (t)ool
#

procedure bal(ref, images)

        string ref  {"", prompt="reference image" }
        string images   {"", prompt="File containing list of images to align." }
        real  sbox     {13, prompt="Box size for centroiding? (pix)"}
	string *image_list
begin 
	int    numimg

	string img2
        string t_ref
        string t_images
        real  t_sbox
	string img[9]

	real   thisra,thisdec,thismag,thisid,scale

        real   x1,y1,x2,y2,x3,y3,x4,y4,xnow,ynow
        int    wcs
        string now, command, tvfile, shiftfile, centerfile, pointfile
	string rawnames

        string lab0,lab1,lab2
	real   xcen,ycen

        t_ref = ref
        t_images = images
        t_sbox = sbox

        
        display(t_ref,1)
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

        centerfile = t_ref//".baltcen"
        imcntr(t_ref,x1,y1,cboxsize=t_sbox, >> centerfile)
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

        image_list = t_images
        while ( fscan(image_list, img2)!=EOF ) {
           print img2
	   display(img2,2)
           print(" Mark the same star ON FRAME 2")
           print('******************')
	   print('* Q - quit       *'
	   print('* any other-mark *'
           print('*  >>IMCURSOR<<  *')
           print('******************')
           now = fscan(imcur, x2, y2, wcs, command)
           if (command == "Q") bye
           centerfile = img2//".baltcen"
           imcntr(img2,x2,y2,cboxsize=t_sbox, >> centerfile)
           list = centerfile
           now = fscan(list,lab0,lab1,xcen,lab2,ycen)
##         print("star centroid : ",xcen,ycen)
           pointfile=mktemp("tmp$pt")
           print(xcen, ycen, > pointfile)
	   x2 = xcen
	   y2 = ycen
           tvmark(2,pointfile,mark='circle',radii='18,22',color=205)
           delete(pointfile, verify-)
           delete(centerfile, verify-)
           delete(tvfile, verify-)
	   shiftfile = 'shifts'
	   print( (x1-x2),(y1-y2), >> shiftfile)
	}

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
        centerfile = t_ref//".baltone"
        imcntr(t_ref,xnow,ynow,cboxsize=t_sbox, >> centerfile)
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
	imalign("@"//t_images,t_ref,tvfile,"a//@"//t_images,shifts=shiftfile)
	print(" ")
	print(" Done ")
 
end

	

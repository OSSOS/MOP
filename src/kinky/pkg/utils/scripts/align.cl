#
#   align.cl  
#
#  A tool for registering images.
#  This routine displays the reference image and
#  asks you to mark a bunch of reference stars.
#  Then it displays all the images in succession, asking you
#  to mark the same star in each image.  Then
#  it does shifts the images, either integer or
#  fractional pixel shifts, and outputs the
#  result into a list of images.

procedure align(images)

	string rimage      {"", prompt="reference image"}
	string images      {"", prompt="Images to be used"}
        int  sbox          {9, prompt=" Box size for centroiding (pix) "}
        int  fframe        {1, prompt=" first display frame"}	
	struct *clx
	string outims      {"", prompt="Output images"}
	string successive  {"yes", prompt="Display in successive frames (yes|no)"}
	string pix_shifts  {"yes", prompt="Integer pixel shifts? (yes|no)"}
	string sect        {"", prompt="image section to be displayed"}
	string shifts      {"", prompt="file for storing shift values"}
	string refs        {"", prompt="file for storing reference star positions"}
	real xshift        {"0.0", prompt="additional fractional pixel x shift"}
	real yshift        {"0.0", prompt="additional fractional pixel y shift"}

begin 
        string now, command
	real   x1, y1, xpix, ypix
	real   x[100], y[100]
	real   txshift, tyshift
        int    wcs, box, currframe, i, nims, nrefs
        string centerfile, lab0, lab1, lab2
        string refposfile
	string imgfile, img, offfile, tshifts, trefs

	box = sbox
	tshifts = shifts
	trefs = refs
	txshift = xshift
	tyshift = yshift

	currframe = fframe
	display (rimage, currframe, fill-)

	# start loop for getting reference stars.
	i = 1
        now= fscan(imcur, x1, y1, wcs, command)
	while(command != "q"){
		print("foo")
		refposfile=mktemp("tmp$ref")
		imcntr(rimage,x1,y1,cboxsize=box, >> refposfile)
		list = refposfile
		now = fscan(list,lab0,lab1,xpix,lab2,ypix)
		print(xpix, " ", ypix)
		x[i] = xpix
		y[i] = ypix
		delete(refposfile, verify-)
	        now= fscan(imcur, x1, y1, wcs, command)
		i += 1
	}

	nrefs = i - 1

	delete(trefs, verify-)
	for(i=1; i<=nrefs; i += 1){
		printf("%f %f\n", x[i], y[i], >>trefs)
	}

	imgfile = mktemp ("tmp$ctr")
	sections (images, option="fullname", > imgfile)
	clx = imgfile

	i = 1
	print("Put the cursor on your registration star.")
	print("Hit any key (except q) to compute centroid")
	print("Hit q to move on to the next star or to shift images")

	# The main big loop starts here
	while (fscan (clx, img) != EOF) {

		display (img//sect, currframe, fill-)
	        now= fscan(imcur, x1, y1, wcs, command)
		while(command != "q"){
			print(" x1: ", x1, " y1: ", y1, " command: ", command)
			centerfile=mktemp("tmp$cen")
			imcntr(img,x1,y1,cboxsize=box, >> centerfile)
			list = centerfile
			now = fscan(list,lab0,lab1,xpix,lab2,ypix)
			print(" xc: ",xpix," yc: ", ypix)
			delete(centerfile, verify-)
		        now= fscan(imcur, x1, y1, wcs, command)
		}

		x[i] = xpix
		y[i] = ypix
		
		if(successive == "yes") currframe += 1
		i += 1
	 }

	delete(imgfile, verify-)

	nims = i-1
	

	offfile = mktemp ("tmp$off")

	for(i=1; i<=nims; i += 1){
			printf("%f %f\n", x[1]-x[i], y[1]-y[i], >>offfile)

	}
	imalign(images,rimage,trefs,outims,shifts=offfile)
	delete(offfile, verify-)
end

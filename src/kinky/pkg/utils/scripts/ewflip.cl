#
#  THIS IS THE CFH12k 2001 version!
#
#  ewflip.cl  
#
#   Takes an input CCD image, attempts to determine if it is a known camera,
#  and then orients it to N up and E left.
#   Currently exits if it cannot identify the image.

procedure ewflip(common,newcom)

	string common {"", prompt="Original filename "}
	string newcom {"", prompt="Output filename "}
	bool verbose {"", prompt="report progress "}
	string version {"July 2001", prompt="Version"}

begin 
	int    nimg,i, flag, xinvert, binvert
	int    chipnum
	string rootword, newroot
	string infile, outfile,flip,flipy,flipboth,ending
	string teststring

	flip = "[-*,*]"
	flipy = "[*,-*]"
	flipboth = "[-*,-*]"
	if (verbose) {
	  print(" ") 
	  print(" EWFLIP script ")
	  print(" ") 
	  print(" Takes an input CCD image, attempts to determine if it is ")
	  print(" a known camera, and then orients it to N up and E left. ")
	  print(" Currently exits if it cannot identify the image. ")
        }
	rootword = common
	outfile  = newcom
	
	imgets(rootword,'ewflip')
	if ( imgets.value == "done" ) {
	   print("already flipped");
	   imcopy(rootword,outfile,verbose=verbose);
	   bye;
	}

        imgets(rootword,'DETECTOR')
	teststring = imgets.value
	if (verbose) {
	  print(" ") 
	  print(" DETECTOR: ", teststring)
	  print(" ") 
        }
#-------------------------------------------------- PALOMAR ---------------

	if(teststring == 'TEK3') {
            if (verbose) print ("Palomar - raw images have N up and E *right*")
	    infile = rootword//flip
   	    if (verbose) print(" flipping ", infile, " in x to ", outfile)
	    imcopy(infile,outfile,verbose=verbose)
        }
#-------------------------------------------------- KPNO ------------------
	if(teststring == 'CCDMosaThin1' ) { 
            imgets(rootword,'IMAGEID')
	    chipnum = int(imgets.value)
            if (verbose) print("KPNO IMAGEID :",chipnum)
            if (chipnum >4) {
                infile = rootword//flip
                if (verbose) print("  Doing 90 degree clockwise rotation...")
            }
            if (chipnum <5) {
                infile = rootword//flip
                if (verbose) print("  Doing 90 degree clockwise rotation...")
            }
# FOR the moment it seems all chips need a 90-deg clockwise rotation, but check!
	    imtranspose(infile,outfile)
        }
	if (teststring == 'Mosaic2' ) {
	   # CTIO is in the south so it's a counter-clockwise rotation :-)
	   infile = rootword//flipy
	   if (verbose) print(" Doing a 90 deg counter clockwise rotation...")
	    imtranspose(infile,outfile)
	}
#-------------------------------------------------- CFHT ------------------
	if(teststring == 'CFH12K') {
            imgets(rootword,'CHIPID')
	    chipnum = int(imgets.value)
            if (verbose) print ("CFHT -- chip number : ",chipnum)
	    switch (chipnum) {
		case 0,1,4,5:
                  infile = rootword//flipboth
                  if (verbose) print(" Flip both x and y directions")
		case 2,3:
                  infile = rootword//flipy
                  if (verbose) print(" Flip only in y direction")
		case 6,7,8,9,10,11,12:
                  infile = rootword//flip
                  if (verbose) print(" Flip only in x direction")
	    }
   	    if (verbose) print("   Orienting ", infile, " to ", outfile)
	    imcopy(infile,outfile,verbose=verbose);
	    hedit(outfile,"ewflip","done",add+,verify-,update+,show=verbose);
        }

	if (verbose) print(" ") 
	if (verbose) print(" Done ")
	if (verbose) print(" ")

end

	

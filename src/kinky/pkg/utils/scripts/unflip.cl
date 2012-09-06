#
#  unflip.cl  
#  SPECIFICALLY FOR CFHT OLD MOSAIC DATA
#
#   Takes an input CCD image, attempts to determine if it is a known camera,
#  and then orients it to N up and E left.
#   Currently exits if it cannot identify the image.

procedure unflip(common,newcom)

	string common {"", prompt=" Original filename "}
	string newcom {"", prompt=" Output filename "}

begin 
	int    nimg,i, flag, xinvert, binvert
	int    chipnum
	string rootword, newroot
	string infile, outfile,flip,flipy,flipboth,ending
	string teststring

	flip = "[-*,*]"
	flipy = "[*,-*]"
	flipboth = "[-*,-*]"
	print(" ") 
	print(" UNFLIP script ")
	print(" ") 
	print(" Takes an input CCD image, attempts to determine if it is ")
	print(" a known camera, and then orients it to N up and E left. ")
	print(" Currently exits if it cannot identify the image. ")
	rootword = common
	outfile  = newcom

        imgets(rootword,'DETECTOR')
	teststring = imgets.value
	print(" ") 
	print(" DETECTOR: ", teststring)
	print(" ") 
#-------------------------------------------------- PALOMAR ---------------
	if(teststring == 'TEK3') {
            print ("Palomar - raw images have N up and E *right*")
	    infile = rootword//flip
   	    print(" flipping ", infile, " in x to ", outfile)
	    imcopy(infile,outfile)
        }
#-------------------------------------------------- KPNO ------------------
	if(teststring == 'CCDMosaThin1') {
            imgets(rootword,'IMAGEID')
	    chipnum = int(imgets.value)
            print("KPNO IMAGEID :",chipnum)
            if (chipnum >4) {
                infile = rootword//flip
                print("       Doing 90 degree clockwise rotation...")
            }
            if (chipnum <5) {
                infile = rootword//flip
                print("       Doing 90 degree clockwise rotation...")
            }
# FOR the moment it seems all chips need a 90-deg clockwise rotation, but check!
	    imtranspose(infile,outfile)
        }
#-------------------------------------------------- CFHT ------------------
	if(teststring == 'CFH12K') {
            imgets(rootword,'CHIPID')
	    chipnum = int(imgets.value)
            print ("*****>>> Pre Aug99 CFHT -- chip number : ",chipnum)
	    if (chipnum <6) {
                  infile = rootword//flipboth
                  print("       Must flip both x and y directions")
            }
	    if (chipnum >6) {
                  infile = rootword//flip
                  print("       Need flip this chip only in x direction")
            }
#   	    print("       Orienting ", infile, " to ", outfile)
	    imcopy(infile,outfile)
        }

	print(" ") 
	print(" Done ")
	print(" ")

end

	

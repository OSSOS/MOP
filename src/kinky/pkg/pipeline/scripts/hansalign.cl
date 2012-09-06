#
#   hansalign.cl  
#
#  Aligns 3 arbitrary frames with coordinates and shifts given by hans.
#
# Expects to find:
# hans.list - list of 3 images to align
# hans.ref  - file containing name of reference image
# hans.coo  - coordinates of bright stars on reference image
# hans.shift- shifts of images 2 and 3 to image 1
#

#procedure hansalign(simg)
procedure hansalign()

#	string simg   {"", prompt=" name of reference image?"}
	int    ssbox  {7, prompt=" small box size?"}
	int    sbbox  {11, prompt="  big  box size?"}
begin 

	string intable, refimg, outfile, dumfile
	int    sbox, bbox, nf

	print(" ") 
	print(" ************************************#*") 
	print(" * Hans Align.  n files --> a1,a2,... *")
	print(" ************************************#*") 
	print(" ") 

	outfile = "aligned.out"
	nf = 0
	list = 'hans.list'
	while (fscan(list, dumfile) != EOF) {
	  nf += 1
	  printf("a%d\n", nf, >> outfile)
	}

	list = 'hans.ref'
        while(fscan(list,refimg) != EOF) {}
	print("refimg : ",refimg)
	sbox = ssbox
	bbox = sbbox

	imalign('@hans.list',refimg,'hans.coo','@aligned.out',shifts='hans.shift',                 boxsize=sbox,bigbox=bbox,shiftimages+,trim+,verb+)

        print(" Done ")
	delete (outfile, verify-)
end


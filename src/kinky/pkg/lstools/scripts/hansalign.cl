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

	string intable,refimg
	int    sbox, bbox

	print(" ") 
	print(" *************************************") 
	print(" * Hans Align.  3 files --> a1,a2,a3 *")
	print(" *************************************") 
	print(" ") 

#	intable = simg
#	print("Reading : ",intable)
#	list=intable
	list='hans.ref'
        while(fscan(list,refimg) != EOF) {print( "Read a line (1 only!)" )}
	print("refimg : ",refimg)
	sbox = ssbox
	bbox = sbbox

	imalign('@hans.list',refimg,'hans.coo','a1,a2,a3',shifts='hans.shift',                 boxsize=sbox,bigbox=bbox,shiftimages+,trim+,verb+)

        print(" Done ")
end


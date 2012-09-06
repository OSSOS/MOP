#
#  altalign.cl  
#
#   Aligns n images to a given reference image and creates a
#  set of aligned files. 
#   ****  READS a file for a shift for each image. User has presumably
#         determined this for each image before invoking this routine.
#   ****      Xshift = Xref - Ximage , Yshift = Yref - Yimage
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and 1 <= n <= N.
#    - There must also be file named xxxxcoords that has the
#      reference objects in the coords of the ref image. 
#      Each following line is the coords (in ref. image) of the
#      alignment objects (stars).
#    - There must also be file named xxxxshifts that has N lines.
#      Each line holds a rough xshift,yshift for each image. The line
#      corresponding to the reference image should be   0.0   0.0
#

procedure myalign(common,refn)

	string common {"", prompt=" Common portion of filename "}
	int    refn   { 2, prompt=" Number of reference image (>1) "}

begin 
	int    nimg, refimgn
	real   xsh,ysh
	string coordfile, rootword
	string refimg,infile,outfile
	string currimg, shftfile

	print(" ")
	print(" ") 
	print(" Welcome to the ALTERNATE ALIGNMENT PROGRAM")
	print(" ") 
	print("  ******  If you wish to use automatic coarse centering,") 
	print("  ******  then use the script myalign.cl instead. ") 
	print(" ") 
	rootword = common
	print(" input # of images : ") 
	scan(nimg)
	refimgn = refn
	coordfile = rootword//'coords'
	print(" Accessing reference coordinates in :  ",coordfile)
	shftfile = rootword//'shifts'
	print(" Accessing coarse shifts in :  ",shftfile)
	outfile = 'rg' + rootword + refimgn
	refimg = rootword//refimgn

	print(" ") 
	print(" Aligning images: ")
	print(" ") 
#
# Create list of images 
#
	
	infile = rootword//1 
	
	for (i=2; i<=nimg; i+=1)
	{
		currimg = rootword//i 
		infile = infile + ',' + currimg
	}
	print(" Aligning ", infile, " to ", refimg)
	imalign(infile,coordfile,refimg,shifts=shftfile,trimimages-)
#
# 
	print(" ")
	print(" ")
	print(" Done ")
	print(" ")

end

	

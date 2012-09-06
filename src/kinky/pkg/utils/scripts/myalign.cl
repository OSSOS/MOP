#
#  myalign.cl  
#
#   aligns n images to a given reference image and creates a
#  set of aligned files.
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and 1 <= n <= N.
#    - There must also be file named xxxxcoords that has the
#      reference objects in the coords of the ref image. 
#      Each following line is the coords (in ref. image) of the
#      alignment objects (stars).
#

procedure myalign(common,refn)

	string common {"", prompt=" Common portion of filename "}
	int    refn   { 1, prompt=" Number of reference image  "}

begin 
	int    nimg, refimgn
	real   xsh,ysh
	string coordfile, rootword
	string refimg,infile,outfile
	string currimg

	print(" ")
	print(" ") 
	print(" Welcome to MY ALIGNMENT PROGRAM")
	print(" ") 
	rootword = common
	print(" input # of images : ") 
	scan(nimg)
	refimgn = refn
	coordfile = rootword//'coords'
	print(" Accessing reference coordinates in :  ",coordfile)
	outfile = 'rg' + rootword + refimgn
	refimg = rootword//refimgn

	print(" ") 
	print(" Aligning images: ")
	print(" ") 
#
# Align images one by one 
#
		
	for (i=1; i<refimgn; i+=1)
	{
		currimg = rootword//i 
		currimg = currimg + ',' + refimg
		print(" Aligning ", currimg, " to ", refimg)
		imalign(currimg,coordfile,refimg,trimimages-)
		imdelete(outfile)
	}

	for (i=refimgn+1; i<=nimg; i+=1)
	{
		currimg = rootword//i 
		currimg = currimg + ',' + refimg
		print(" Aligning ", currimg, " to ", refimg)
		imalign(currimg,coordfile,refimg,trimimages-)
		imdelete(outfile)
	}
	print(" copying reference file ") 
	imcopy(refimg,outfile)
#
# 
	print(" ")
	print(" ")
	print(" Done ")
	print(" ")

end

	

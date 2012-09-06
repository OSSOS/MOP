#
#  massrn.cl  
#
#   Filters N images with common filenames to another rootname.
#  Uses boxcar 20x20 median filter
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and 1 <= n <= N.

procedure massfilt(common,newcom)

	string common {"", prompt=" Common portion of old filename "}
	string newcom {"", prompt=" Common portion of new filename "}
	int    sboxsize{"20", prompt=" size of boxcar (23?) "}

begin 
	int    nimg,i,oldn,oldfirst
	int    newfirst,boxsize
	string rootword, newroot
	string infile, outfile,tmpfile,newfield

	print(" ")
	print(" ") 
	print(" Welcome to MASS FILTering ")
	print(" ") 
	rootword = common
	newroot  = newcom
	boxsize = sboxsize
	print(" input # of images : ") 
	scan(nimg)
	print(" input first # of OLD image set: ") 
	scan(oldfirst)
	print(" input first # of NEW image set: ") 
	scan(newfirst)

	print(" ") 
	print(" Filtering images: ")
	print(" ") 
#
# Filter images one by one 
#
		
	for (i=newfirst; i<=nimg+newfirst-1; i+=1)
	{
		oldn = oldfirst - newfirst + i 
		infile  = rootword//oldn
		tmpfile =  'flt'//newroot//i 
		outfile =  newroot//i 

		print("Filtering ",infile," to ",outfile)
		median(infile,tmpfile,boxsize,boxsize,boundary="nearest")
		imarith(infile,"-",tmpfile,outfile)
		imdel(tmpfile,verify-)

                newfield = 'BOXFILTER'
                hedit(outfile,newfield,(boxsize),add+,show-,update+,verify-

	}

	print(" ")
	print(" Done ")
	print(" ")

end

	

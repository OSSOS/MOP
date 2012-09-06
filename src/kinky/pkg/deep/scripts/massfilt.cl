#
#  massrn.cl  
#
#   Filters N images with common filenames to another rootname.
#  Uses boxcar 20x20 median filter
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and 1 <= n <= N.

procedure massfilt(images,oldprefix,newprefix)

	string images {"", prompt=" images"}
	string oldprefix {"", prompt="prefix"}
	string newprefix {"", prompt=" output prefix"}
	int    sboxsize{20, prompt=" size of boxcar (23?) "}

begin 
	int    nimg,i,oldn,oldfirst
	int    newfirst,boxsize
	string rootword, newroot, opr
	string infile, outfile,tmpfile,newfield



	opr = oldprefix
	newroot  = newprefix
	boxsize = sboxsize
	print(" Filtering images: ")
#
# Filter images one by one 
#
	list = images
		
	while (fscan(list,infile)!=EOF) {
		outfile =  newroot//infile
		infile =  opr//infile

		print("Filtering ",infile," to ",outfile)
	        tmpfile=mktemp("massilt");
		median(infile,tmpfile,boxsize,boxsize,boundary="nearest")
		imarith(infile,"-",tmpfile,outfile)
		imdel(tmpfile,verify-)

                newfield = 'BOXFILTER'
                hedit(outfile,newfield,(boxsize),add+,show-,update+,verify-)

	}

	print(" ")
	print(" Done ")
	print(" ")

end

	

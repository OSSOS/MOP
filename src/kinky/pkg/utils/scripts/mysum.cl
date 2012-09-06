#
#  mysum.cl  
#
#   Sums n images with a and puts the result in a summed file.
#
#  Input requirements: There must be N(=nimg) files named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#

procedure mysum(common)

	string common {"", prompt=" Common portion of filename "}

begin 
	int    nimg 
	string infile,rootword,outfile,cplist,junkfile

	print(" ")
	print(" ") 
	print(" Welcome to MY SUMMING PROGRAM")
	print(" ") 
	rootword = common
	print(" input # of images : ") 
	scan(nimg)
	outfile = rootword//'sum'
	print("Summed image will be written to :  ",outfile)

	print(" ") 
	print(" Processing files: ")
	print(" ") 
	print(" copying image 1 ") 
	infile = rootword//1
	imcopy(infile,outfile)
	print(" displaying image 1 ") 
	display(outfile,1)
	junkfile = rootword + 'junk9'
	print(" junkfile will be: ",junkfile) 
	print(" ") 
	print(" ") 
	for (i=2; i<=nimg; i+=1)
	{
		infile = rootword//i
		print("       Currently working on: ",infile)

#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		cplist = infile + ',' + outfile
		print(" summing ",cplist," and placing result in: ",junkfile) 
		imsum(cplist,junkfile)
		print(" renaming temporary files...")
		imdelete(outfile)
		imrename(junkfile,outfile)
	
#  display current total sum/shifted image
#
		print(" displaying progress...")
		display(outfile,1)
#
	}

# display the final summed file
#
	
	print(" ")
	print(" displaying summed image -------->") 
	display(outfile,1)
	print(" ")
	print(" Done ")
	print(" ")

end

	

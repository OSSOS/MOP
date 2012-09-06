#
#  myshift.cl  
#
#   shifts n images with a given rate and puts the result in a summed file.
#
#  Input requirements: There must be N(=nimg) files named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#

procedure myshift(common)

	string common {"", prompt=" Common portion of filename "}

begin 
	int    nimg 
	real   rate,xsh0,ysh0,angle
	real   xsh,ysh
	string infile,rootword,outfile,cplist,junkfl1,junkfl2

	print(" ")
	print(" ") 
	print(" Welcome to MY SHIFTING PROGRAM")
	print(" ") 
	rootword = common
	print(" input # of images : ") 
	scan(nimg)
	print(" input rate of pixel shift between images : ") 
	scan(rate)
	print(" input angle to ecliptic (degrees) : ") 
	scan(angle)
	angle = angle/57.29578
	outfile = rootword//'shifted'
	print("Shifted and summed image will be written to :  ",outfile)

	print(" ") 
	print(" Processing files: ")
	print(" ") 
	print(" copying image 1 ") 
	infile = rootword//1
	imcopy(infile,outfile)
	print(" displaying image 1 ") 
	display(outfile,1)
	junkfl1 = rootword + 'sumjunk'
	print(" sum junkfile will be: ",junkfl1) 
	print(" ") 
	print(" ") 

	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)

	for (i=2; i<=nimg; i+=1)
	{
		infile = rootword//i
		print("       Currently working on: ",infile)

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		ysh = -ysh0*(i-1)
		xsh = -xsh0*(i-1)
		junkfl2 = rootword + 'shftjunk'
		print(" shifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh)
		cplist = junkfl2 + ',' + outfile
		print(" summing ",cplist," and placing result in: ",junkfl1) 
		imsum(cplist,junkfl1)
		print(" renaming temporary files...")
		imdelete(outfile)
		imrename(junkfl1,outfile)
		imdelete(junkfl2)

#  display current total summed/shifted image
#
		print(" displaying progress... ")
		display(outfile,1)

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

	

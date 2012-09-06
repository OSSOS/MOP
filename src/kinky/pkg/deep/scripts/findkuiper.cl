#
#  findkuiper.cl  
#
#   shifts N images with a given rate and puts the result in a summed file.
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#

procedure findkuiper(common)

	string common {"", prompt=" Common portion of filename "}

begin 
	int    nimg, dispflg 
	real   rate,xsh0,ysh0,angle
	real   xsh,ysh
	real   currtime, strttime
	string infile,rootword,outfile,cplist,junkfl1,junkfl2
	string newfield

	print(" ")
	print(" ") 
	print(" *************************************") 
	print(" * Welcome to the Kuiper Belt Finder *")
	print(" *************************************") 
	print(" ") 
	print(" Input to this program is from N registered images! ") 
	print(" All images will be aligned to the first. ")
	print(" ") 
	rootword = common
	print(" input # of images : ") 
	scan(nimg)
	print(" input search rate of motion on sky (pixels/hour) : ") 
	scan(rate)
	print(" input angle to ecliptic (degrees) : ") 
	scan(angle)
	angle = angle/57.29578
	outfile = rootword//'shifted'
	print(" Display progress? (1-yes, 0-no) ") 
	scan(dispflg)
	print("Shifted and summed image will be written to :  ",outfile)

	print(" ") 
	print(" Processing files: ")
	print(" ") 
	print(" copying image 1 ") 
	infile = rootword//1
	imcopy(infile,outfile)
	if (dispflg == 1){
	  print(" displaying image 1 ") 
	  display(outfile,1,zscale+)
	}
	gettime(infile)
	strttime = gettime.outputime
	print(" **  Image 1 acquired at ", strttime, " UT") 
	junkfl1 = rootword + 'sumjunk'
#	print(" sum junkfile will be: ",junkfl1) 
	print(" ") 

	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)

	for (i=2; i<=nimg; i+=1)
	{
		infile = rootword//i
		print("       Currently working on: ",infile)
		gettime(infile)
		currtime = gettime.outputime
		print(" * Image ",i," acquired at ", currtime, " UT") 

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#

		ysh = -ysh0*(currtime - strttime)
		xsh = -xsh0*(currtime - strttime)
#		print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#		print(" xshift ",xsh," and yshift ",ysh) 

		junkfl2 = rootword + 'shftjunk'
		print(" xshifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh)
		cplist = junkfl2 + ',' + outfile
#		print(" summing ",cplist," and placing result in: ",junkfl1) 
		imsum(cplist,junkfl1,verbose-)
#		print(" renaming temporary files...")
		imdelete(outfile,verify-)
		imrename(junkfl1,outfile)
		imdelete(junkfl2,verify-)

#  display current total summed/shifted image
#
		if (dispflg == 1){
		  print(" displaying progress... ")
		  display(outfile,1,zscale+)
		}

	}

#   Scale summed file by number of images and display final summed file
#
	
	imarith(outfile,"/",nimg,outfile)
#	imreplace(outfile,100.0,lower=INDEF,upper=100.)
	print(" ")
	if (dispflg == 1){
	  print(" displaying summed image -------->") 
	  display(outfile,1,zscale+)
	}
	print(" Recording shift information ") 
# Record shift rates in header
	newfield = 'nshftimg'
	hedit(outfile,newfield,(nimg),add+,show+,update+,verify-)
	newfield = 'shftrate'
	hedit(outfile,newfield,(rate),add+,show+,update+,verify-)
	newfield = 'shftangle'
	angle = angle*57.29578
	hedit(outfile,newfield,(angle),add+,show+,update+,verify-)

	print(" ")
	print(" Done ")
	print(" ")

end

	

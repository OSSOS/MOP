#
#  shift.cl  
#
#   shifts N images with a given rate and SAVES ALL FILES!
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#

procedure myshift(common)

	string common {"", prompt=" Common portion of filename "}

begin 
	int    nimg, dispflg 
	real   rate,xsh0,ysh0,angle,anglew
	real   xsh,ysh
	real   currtime, strttime
	string infile,rootword,outfile,cplist,junkfl1,junkfl2
	string newfield

	print(" ")
	print(" ") 
	print(" *************************") 
	print(" * SHIFTING PROGRAM ONLY *     <<<******")
	print(" *************************") 
	print(" ") 
	print(" Input to this program is from N registered images! ") 
	print(" All images will be aligned to the first. ")
	print(" ALL images are saved to disk. ")
	print(" ") 
	rootword = common
	print(" input # of images : ") 
	scan(nimg)
	print(" input search rate of motion on sky (pixels/hour) : ") 
	scan(rate)
	print(" input angle to ecliptic (degrees) : ") 
	scan(angle)
	angle = angle/57.29578
       	anglew = angle*57.29578

	print(" ") 
	print(" Processing files: ")
	print(" ") 
	print(" copying image 1 ") 
	infile = rootword//1
	outfile = rootword//'sh1'
	imcopy(infile,outfile)
# Record shift rates in header
       	newfield = 'shftrate'
       	hedit(outfile,newfield,(rate),add+,show+,update+,verify-)
       	newfield = 'shftangle'
       	hedit(outfile,newfield,(anglew),add+,show+,update+,verify-)
       	newfield = 'xsh'
	xsh = 0.0
       	hedit(outfile,newfield,(xsh),add+,show+,update+,verify-)
       	newfield = 'ysh'
	ysh = 0.0
       	hedit(outfile,newfield,(ysh),add+,show+,update+,verify-)

	gettime(infile)
	strttime = gettime.outputime
	print(" **  Image 1 acquired at ", strttime, " UT") 
	junkfl1 = rootword + 'sh'
#	junkfl1 = rootword + 'sumjunk'
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

		junkfl2 = rootword + 'sh'
		junkfl2 = junkfl2//i
		print(" xshifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh)

# Record shift rates in header
        	newfield = 'shftrate'
        	hedit(junkfl2,newfield,(rate),add+,show+,update+,verify-)
        	newfield = 'shftangle'
        	hedit(junkfl2,newfield,(anglew),add+,show+,update+,verify-)
        	newfield = 'xsh'
        	hedit(junkfl2,newfield,(xsh),add+,show+,update+,verify-)
        	newfield = 'ysh'
        	hedit(junkfl2,newfield,(ysh),add+,show+,update+,verify-)

#		cplist = junkfl2 + ',' + outfile
#		print(" summing ",cplist," and placing result in: ",junkfl1) 
#		imsum(cplist,junkfl1,verbose-)
#		print(" renaming temporary files...")
#		imdelete(outfile,verify-)
#		imrename(junkfl1,outfile)
#		imdelete(junkfl2,verify-)

	}

#   Scale summed file by number of images and display final summed file
#	DELETETED
#	print(" Recording shift information ") 

	print(" ")
	print(" Done ")
	print(" ")

end

	

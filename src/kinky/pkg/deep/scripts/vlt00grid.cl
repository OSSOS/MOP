#
#  vlt00grid.cl  
#
#   shifts N images with at a variety of rates and creates a summed and
#  median image with the highest pixel rejected.
#  Accesses the image headers to get the exposure start time.
#
#  NOTE: requires oldgettime script.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES SHOULD BE PREVIOUSLY BACKGROUND SUBTRACTED AND SCALED.
#

procedure wfigrid(common,pfieldn,phrvlt)

	string common {"", prompt=" Common portion of filename "}
	string pfieldn {"", prompt=" name of header keyword for obs time "}
	int    phrvlt {"", prompt=" Number of high rejection pixels in sum "}
begin 
	int    nimg, dispflg 
	real   rate,xsh0,ysh0,angle,angledeg
	real   xsh,ysh
	real   currtime, strttime
	string infile,rootword,outfile,cplist,junkfl1,junkfl2
	string newfield,imglist,fieldn
	real   strate,maxrate, ratestep, realj
	real   stang, maxang,  angstep
	int    inta, intb, lr, hr, hrvlt

# set low and high reject parameters
	lr = 0
	hr = 3
	hrvlt = phrvlt

	print(" ")
	print(" ") 
	print(" *************************") 
	print(" * VLT 2000 SHIFTING PROGRAM *")
	print(" *************************") 
	print(" Assumes that all objects moving toward upper right ") 
	print(" Probably can't handle switching quadrant of motion ") 
	print(" ") 
	print(" Input to this program is from N registered images! ") 
	print(" All images will be aligned to the first. ")
	print(" ") 
	print(" ***** NEW -------->>> OUTPUT IMAGES are in SHORT format.")
	print(" ") 
	print(" **** WARNING!!! ------->>> MODIFIED for VLT.")
	print(" ") 
	rootword = common
	fieldn = pfieldn

	print(" input # of images : ") 
	scan(nimg)
	print(" input lowest search rate of motion on sky (pixels/hour) : ") 
	scan(strate)
	print(" input highest search rate of motion on sky (pixels/hour) : ") 
	scan(maxrate)
	print(" input resolution of motion on sky (pixels/hour) : ") 
	scan(ratestep)
	print(" input ABSOLUTE VALUE of first angle to ecliptic (degrees) : ") 
	scan(stang)
	stang = 1.0*stang
	stang = stang/57.29578
	print(" input ABSOLUTE VALUE of maximum angle to ecliptic (degrees) : ") 
	scan(maxang)
	maxang = 1.0*maxang
	maxang = maxang/57.29578
	print(" input ABSOLUTE VALUE of resolution (degrees) : ") 
	scan(angstep)
	angstep = 1.0*angstep
	angstep = angstep/57.29578

# NOTE SIGNS ON ANGLES
   for (angle=stang; angle<=maxang; angle+=angstep)
   {
   angledeg = angle*-57.29578

     for (rate=strate; rate<=maxrate; rate+=ratestep)
     {
	print(" **************************************************") 
	print(" Processing files for rate: ",rate, ' angle: ',angledeg)
	print(" ") 

#	print(" copying image 1 ") 
	infile = rootword//1
	outfile = rootword//'sh1'
	imcopy(infile,outfile)
#	gettime(infile,fieldn)
	imgets(infile,'UT')
	strttime = real(imgets.value)
	print(" **  Image 1 acquired at ", strttime, " UT") 
	junkfl1 = rootword + 'sh'

	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)
        for (i=2; i<=nimg; i+=1)
	{
		infile = rootword//i
#		gettime(infile,fieldn)
	imgets(infile,'UT')
	currtime = real(imgets.value)
#		currtime = gettime.outputime
		print(" * Image ",i," acquired at ", currtime, " UT") 

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		ysh =  ysh0*(currtime - strttime)
		xsh = -xsh0*(currtime - strttime)

#		print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
		print(" xshift ",xsh," and yshift ",ysh) 

		junkfl2 = rootword + 'sh'
		junkfl2 = junkfl2//i
#		print(" xshifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh)

# Record shift rates in header
#        	newfield = 'shftrate'
#        	hedit(junkfl2,newfield,(rate),add+,show-,update+,verify-)
#        	newfield = 'shftangle'
#        	hedit(junkfl2,newfield,(angledeg),add+,show-,update+,verify-)
#        	newfield = 'xsh'
#        	hedit(junkfl2,newfield,(xsh),add+,show-,update+,verify-)
#        	newfield = 'ysh'
#        	hedit(junkfl2,newfield,(ysh),add+,show-,update+,verify-)

#		cplist = junkfl2 + ',' + outfile
#		print(" summing ",cplist," and placing result in: ",junkfl1) 
#		imsum(cplist,junkfl1,verbose-)
#		print(" renaming temporary files...")
#		imdelete(outfile,verify-)
#		imrename(junkfl1,outfile)
#		imdelete(junkfl2,verify-)

	}

	imglist = rootword + 'sh*'
	inta = (rate + 0.0001)
	realj = ( (rate-inta)*10 + 0.0001 )
	intb  = realj
	junkfl2 = 's'//inta//'p'//intb
	inta = (-1.0*angledeg + 0.0001)
	realj = ( (-1.0*angledeg  - inta)*10 + 0.0001 )
	intb  = realj
	junkfl2 = junkfl2//'a'//inta//'p'//intb

#       create an average image and a median image, both with minmax rejection
	outfile = junkfl2//'sum'
	print( " " )
	print( " Putting AVERAGED image into: ",outfile,"  high/low reject: ",hrvlt,lr)

	imcombine(imglist,outfile,logfile="",combine="average",reject="minmax",
            outtype="real",scale='none',nlow=lr,nhigh=hrvlt,nkeep=1)

        imreplace(outfile,-100.0,lower=INDEF,upper=-100.)
#               Record shift rates in header
#       	newfield = 'shftrate'
#       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
#       	newfield = 'shftangle'
#       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
       	newfield = 'shftnimg'
       	hedit(outfile,newfield,(nimg),add+,show-,update+,verify-)
       	newfield = 'lowrej'
       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
       	newfield = 'highrej'
       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

	outfile = junkfl2//'med'
	print( " " )
	print( " Putting medianed image into: ",outfile)
	imcombine(imglist,outfile,logfile="",combine="median",reject="minmax",
            outtype="real",scale='none',nlow=lr,nhigh=hr,nkeep=1)
#       Record shift rates in header
#       	newfield = 'shftrate'
#       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
#       	newfield = 'shftangle'
#       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
       	newfield = 'shftnimg'
       	hedit(outfile,newfield,(nimg),add+,show-,update+,verify-)
       	newfield = 'lowrej'
       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
       	newfield = 'highrej'
       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

	print(" ")
	print(" Deleting shifted images! ")
	outfile = rootword//'sh*'
	imdel(outfile,go_ahead+,verify-)
	print(" ")
	print(" Done ")
	print(" ")

     }
   }
	print(" Really done ")
	print(" ")
end

	

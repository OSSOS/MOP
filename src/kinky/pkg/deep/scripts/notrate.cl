#
#  notrate.cl  
#
#   shifts N images with at a variety of rates and creates a summed and
#  median image with the highest pixel rejected.
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES SHOULD BE PREVIOUSLY BACKGROUND SUBTRACTED AND SCALED.
#

procedure notrate(common,pfieldn,plr,phr,pshflag)

	string common {"", prompt=" Common portion of filename "}
	string pfieldn {"", prompt=" name of header keyword for obs time "}
	int    plr     {0, prompt=" number of low pixels to reject (0?) "}
	int    phr     {1, prompt=" number of high pixels to reject(1?) "}
	int    pshflag {"0", prompt=" Keep shifted images? (1=yes,0=no) "}
begin 
	int    nimg, dispflg 
	real   rate,xsh0,ysh0,angle,angledeg
	real   xsh,ysh
	real   currtime, strttime
	string infile,rootword,outfile,cplist,junkfl1,junkfl2
	string newfield,imglist,fieldn
	real   strate,maxrate, ratestep, realj
	real   stang, maxang,  angstep
	int    inta, intb, lr, hr, shflag

	print(" ")
	print(" ") 
	print(" *************************") 
	print(" * Onerate  PROGRAM *")
	print(" *************************") 
	print(" Assumes that all objects moving toward upper right ") 
	print(" Probably can't handle switching quadrant of motion ") 
	print(" ") 
	print(" Input to this program is from N registered images! ") 
	print(" All images will be aligned to the first. ")
	print(" ALL images are saved to disk. ")
	print(" ") 
	rootword = common
	fieldn = pfieldn
# set low and high reject parameters, now in parameter list!
	lr = plr
	hr = phr
	shflag = pshflag

	print(" input # of images : ") 
	scan(nimg)
	print(" input search rate of motion on sky (pixels/hour) : ") 
	scan(strate)
	maxrate = strate+0.0001
	ratestep = 1.0
	print(" input ABSOLUTE VALUE of angle to ecliptic (degrees) : ") 
	scan(stang)
	maxang = stang + 0.0001
	stang = 1.0*stang
	stang = stang/57.29578
	maxang = 1.0*maxang
	maxang = maxang/57.29578
	angstep = 1.0
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
        imgets(infile,fieldn)
	strttime = real(imgets.value)
	print(" **  Image 1 acquired at ", strttime, " UT") 
	junkfl1 = rootword + 'sh'

	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)
        for (i=2; i<=nimg; i+=1)
	{
		infile = rootword//i
		imgets(infile,fieldn)
		currtime = real(imgets.value)
		print(" * Image ",i," acquired at ", currtime, " UT") 

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		ysh =  ysh0*(currtime - strttime)
		xsh = -xsh0*(currtime - strttime)

#		print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#		print(" xshift ",xsh," and yshift ",ysh) 

		junkfl2 = rootword + 'sh'
		junkfl2 = junkfl2//i
#		print(" xshifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh)

# Record shift rates in header
        	newfield = 'shftrate'
        	hedit(junkfl2,newfield,(rate),add+,show-,update+,verify-)
        	newfield = 'shftangle'
        	hedit(junkfl2,newfield,(angledeg),add+,show-,update+,verify-)
#        	newfield = 'xsh'
#        	hedit(junkfl2,newfield,(xsh),add+,show-,update+,verify-)
#        	newfield = 'ysh'
#        	hedit(junkfl2,newfield,(ysh),add+,show-,update+,verify-)
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
	print( " Putting AVERAGED image into: ",outfile,"  high/low reject: ",hr,lr)

	imcombine(imglist,outfile,logfile="",combine="average",reject="minmax",
            nlow=lr,nhigh=hr,nkeep=1)
#  HAVE REPLACED IMSUM WITH IMCOMBINE FOR MORE EFFICIENT HIGH PIXEL REJECTION
#	imsum(imglist,outfile,title="")
#       imarith(outfile,"/",nimg,outfile)

        imreplace(outfile,-100.0,lower=INDEF,upper=-100.)
#               Record shift rates in header
       	newfield = 'shftrate'
       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
       	newfield = 'shftangle'
       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
       	newfield = 'shftnimg'
       	hedit(outfile,newfield,(nimg),add+,show-,update+,verify-)
       	newfield = 'sumlowrej'
       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
       	newfield = 'sumhighrej'
       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

	outfile = junkfl2//'med'
	print( " " )
	print( " MEDIAN IMAGE *** NOT *** CREATED.  Can do from shifted set." )
#	print( " Putting medianed image into: ",outfile, " high/low reject: ",hr,lr)
#	imcombine(imglist,outfile,logfile="",combine="median",reject="minmax", nlow=lr,nhigh=hr,nkeep=1)
#       Record shift rates in header
#       	newfield = 'shftrate'
#       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
#       	newfield = 'shftangle'
#       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
#       	newfield = 'shftnimg'
#       	hedit(outfile,newfield,(nimg),add+,show-,update+,verify-)
#       	newfield = 'lowrej'
#       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
#       	newfield = 'highrej'
#       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

	if (shflag == 0) {
	  print(" ")
	  print(" Deleting shifted images! ")
	  outfile = rootword//'sh*'
	  imdel(outfile,go_ahead+,verify-)
	  } 
	  ;

	print(" ")
	print(" Done ")
	print(" ")

     }
   }
	print(" Really done ")
	print(" ")
end

	

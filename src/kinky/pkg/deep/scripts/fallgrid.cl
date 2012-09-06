#
#  onerate.cl  
#
#   shifts N images with at a variety of rates and creates a summed and
#  median image with the highest pixel rejected.
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES SHOULD BE PREVIOUSLY BACKGROUND SUBTRACTED AND SCALED.
#

procedure fallgrid(common,pfieldn,plr,phr,pshflag)

	string common {"", prompt=" Common portion of filename "}
	string pfieldn {"UTC-OBS", prompt=" name of header keyword for obs time "}
	int    plr     {0, prompt=" number of low pixels to reject (0?) "}
	int    phr     {1, prompt=" number of high pixels to reject(1?) "}
	int    pshflag {"0", prompt=" Keep shifted images? (1=yes,0=no) "}
begin 
	int    nimg, dispflg , nstart
	real   rate,xsh0,ysh0,angle,angledeg
	real   xsh,ysh, x0, y0
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
	print(" starting # of images : ") 
	scan(nstart)
	print(" input search rate of motion on sky (pixels/hour) : ") 
	scan(strate)
	rate=real(strate)
	print(" input angle to ecliptic (degrees) : ") 
	scan(stang)
	angledeg = real(stang)
	angle = stang/57.29578

# NOTE SIGNS ON ANGLES
	print(" **************************************************") 
	print(" Processing files for rate: ",rate, ' angle: ',angledeg)
	print(" ") 

        i=nstart
	list='centre.xy'
	x0=0
	y0=0
	=fscan(list,s1,x0,y0)
	print s1,x0,y0
#	print(" copying image "//nstart ) 
	infile = s1
	#infile = rootword//'/sub'//nstart//'/calibrated.fits'
	#outfile = 'sub'//'sh'//nstart
	outfile = 'sh'//infile
	imcopy(infile,outfile)
	imgets(infile,fieldn)
	#imgets(rootword//'/new'//nstart//'/calibrated.fits',fieldn)
	strttime = real(imgets.value)*24.0
	print(" **  Image "//nstart//" acquired at ", strttime, " MJD") 
	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)
        while( fscan(list,s1,x,y)!=EOF ) 
	{
	        print(x,y)
                infile = s1
		outfile = 'sh'//infile
		#infile = rootword//'/sub'//i//'/calibrated.fits'
		imgets(infile,fieldn)
	        #imgets(rootword//'/new'//i//'/calibrated.fits',fieldn)
	        currtime = real(imgets.value)*24.0
		print(" * Image ",i," acquired at ", currtime, " MJD (hours)") 

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		ysh = ysh0*(currtime - strttime)+y0-y
		xsh = xsh0*(currtime - strttime)+x0-x

		print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
		print(" xshift ",xsh," and yshift ",ysh) 

		junkfl2 = outfile
		print(" xshifting by ",xsh," pixels and creating: ")
		print(" yshifting by ",ysh," pixels and creating: ")
		imshift(infile,junkfl2,xsh,ysh)

# Record shift rates in header
        	newfield = 'shftrate'
        	hedit(junkfl2,newfield,(rate),add+,show-,update+,verify-)
        	newfield = 'shftangle'
        	hedit(junkfl2,newfield,(angledeg),add+,show-,update+,verify-)
        	newfield = 'xsh'
        	hedit(junkfl2,newfield,(xsh),add+,show-,update+,verify-)
        	newfield = 'ysh'
        	hedit(junkfl2,newfield,(ysh),add+,show-,update+,verify-)
	}

	imglist = "sh*"
	inta = (rate + 0.0001)
	realj = ( (rate-inta)*10 + 0.0001 )
	intb  = realj
	junkfl2 = 's'//inta//'p'//intb
	inta = (angledeg + 0.0001)
	realj = ( (angledeg  - inta)*10 + 0.0001 )
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
#	print( " MEDIAN IMAGE *** NOT *** CREATED.  Can do from shifted set." )
	print( " Putting medianed image into: ",outfile, " high/low reject: ",hr,lr)
	imcombine(imglist,outfile,logfile="",combine="median",reject="minmax", nlow=lr,nhigh=hr,nkeep=1)
#       Record shift rates in header
       	newfield = 'shftrate'
       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
       	newfield = 'shftangle'
       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
       	newfield = 'shftnimg'
       	hedit(outfile,newfield,(nimg),add+,show-,update+,verify-)
       	newfield = 'lowrej'
       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
       	newfield = 'highrej'
       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

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

end

	

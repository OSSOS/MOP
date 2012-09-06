#
#  fall.cl  
#
#   shifts N images with at a variety of rates and creates a summed and
#  median image with the highest pixel rejected.
#  Accesses the image headers to get the exposure start time.
#
#  NOTE: requires gettime script.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES SHOULD BE PREVIOUSLY BACKGROUND SUBTRACTED AND SCALED.
#

procedure fallgrid(pimages,prefix,pbaseimage,pfieldn)

	string pimages {"", prompt=" list "}
	string prefix {"", prompt=" prefix"}
	string pbaseimage {"", prompt=" base image "}
	string psuffix {"", prompt=" suffix"}
	string pfieldn {"", prompt=" name of header keyword for obs time "}
        int    lowrate {10, prompt="lowest rate"}
	int    highrate {12, prompt="highest rate"}
	int    lowangle {10, prompt="low angle for combine"}
	int    highangle {10, prompt="high angle for combine"}
begin 
	string images,baseimage,t_prefix
	int    nimg, dispflg, thr,tlr, tha, tla 
	real   rate,xsh0,ysh0,angle,angledeg
	real   xsh,ysh
	real   currtime, strttime
	string basefile,infile,rootword,outfile,cplist,junkfl2
	string newfield,imglist,fieldn,suffix
	real   strate,maxrate, ratestep, realj
	real   stang, maxang,  angstep
	int    inta, intb, lr, hr
	images = pimages;
	baseimage=pbaseimage;
	suffix=psuffix

# set low and high reject parameters
 	tlr = lowrate
	thr = highrate
	tla = lowangle
	tha = highangle	
	t_prefix = prefix
	lr = 0
	hr = 1

	print(" ")
	print(" ") 
	print(" ******************************") 
	print(" * Fall GRID SHIFTING PROGRAM *")
	print(" ******************************") 
	print(" Assumes that all objects moving toward LOWER right ") 
	print(" Probably can't handle switching quadrant of motion ") 
	print(" ") 
	print(" Input to this program is from N registered images! ") 
	print(" All images will be aligned to the first. ")
	print(" ") 
	print(" ***** NEW -------->>> OUTPUT IMAGES are in USHORT format.")
	print(" ") 
	fieldn = pfieldn
	rootword=baseimage;

# DIFF with springgrid follows
	stang = tla;
	stang = -1.0*stang
	stang = stang/57.29578
	maxang=tha
# DIFF with springgrid follows
	maxang = -1.0*maxang
	maxang = maxang/57.29578
	angstep=5
# DIFF with springgrid follows
	angstep = -1.0*angstep
	angstep = angstep/57.29578

# NOTE SIGNS ON ANGLES
# DIFF with springgrid follows
	gettime(baseimage,fieldn)
	strttime = gettime.outputime
	print(" **  Image ",baseimage," acquired at ", strttime, " UT") 
   for (angle=stang; angle>=maxang; angle+=angstep)
   {
   angledeg = angle*-57.29578

     for (rate=tlr; rate<=thr; rate+=4)
     {
	print(" **************************************************") 
	print(" Processing files for rate: ",rate, ' angle: ',angledeg)
	print(" ") 


	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)
	list=images
	while(fscan(list,basefile)!=EOF) {
	 	infile=t_prefix//basefile//suffix;
		gettime(infile,fieldn)
		currtime = gettime.outputime
		print (infile)
		print(" * Image ",infile," acquired at ", currtime, " UT") 

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		ysh = -ysh0*(currtime - strttime)
		xsh = -xsh0*(currtime - strttime)

		print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
		print(" xshift ",xsh," and yshift ",ysh) 

		junkfl2 = infile//"sh";
		print(" xshifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh)

	}

	inta = (rate + 0.0001)
	realj = ( (rate-inta)*10 + 0.0001 )
	intb  = realj
	junkfl2 = 's'//inta//'p'//intb
# DIFF with springgrid follows
	inta = ( -1.0*angledeg + 0.0001)
# DIFF with springgrid follows
	realj = ( ( 1.0*angledeg  - inta)*10 + 0.0001 )
	intb  = -1*realj
	junkfl2 = junkfl2//'a'//inta//'p'//intb

#       create an average image and a median image, both with minmax rejection
	outfile = junkfl2//'sum'
	print( " " )
	print( " Putting AVERAGED image into: ",outfile,"  high/low reject: ",hr,lr)

	imcombine(t_prefix//"//@"//images//"//sh",outfile,logfile="",combine="average",reject="minmax", outtype="short",scale='none',nlow=lr,nhigh=hr,nkeep=1)

        imreplace(outfile,-100.0,lower=INDEF,upper=-100.)


	outfile = junkfl2//'med'
	print( " " )
	print( " Putting medianed image into: ",outfile)
	imcombine(t_prefix//"//@"//images//"//sh",outfile,logfile="",combine="median",reject="minmax", outtype="short",scale='none',nlow=lr,nhigh=hr,nkeep=1)
#       Record shift rates in header
#       	newfield = 'shftrate'
#       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
#       	newfield = 'shftangle'
#       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)

	print(" ")
	print(" Deleting shifted images! ")
	imdel(t_prefix//"//@"//images//"//sh",go_ahead+,verify-)
	print(" ")
	print(" Done ")
	print(" ")

     }
   }
	print(" Really done ")
	print(" ")
end

	

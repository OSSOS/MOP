#
#  wfi.cl  
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

procedure jmpgrid(inlist, in_prefix, out_prefix)
# pfieldn,pnimg,pstrate,pmrate,prstep,pstang,pmang,pastep)

        file inlist  {"", prompt=" Input file list"}
	string in_prefix  {"", prompt=" Input filename prefix"}
	string out_prefix  {"", prompt=" Input filename prefix"}
	string pfieldn {"UT", prompt=" name of header keyword for obs time "}
	real   pstrate {1., prompt=" Lowest search rate of motion on sky (pix/h) "}
	real   pmrate  {10.,prompt=" Highest search rate of motion on sky (pix/h) "}
	real   prstep  {1., prompt=" Resolution of motion on sky (pix/h) "}
	real   pstang  {0., prompt=" First angle to West (degrees) "}
	real   pmang   {10.,prompt=" Last angle to West (degrees) "}
	real   pastep  {5., prompt=" ABSOLUTE VALUE of resolution (degrees) "}
	int    plr     {1,  prompt=" Number of low pixels to reject "}
	int    phr     {3,  prompt=" Number of high pixels to reject "}
	string pout    {"short", prompt=" Output type "}
begin 
	int    dispflg 
	real   rate,xsh0,ysh0,angle,angledeg, aangle
	real   xsh,ysh
	real   currtime, strttime
        string t_in_prefix, t_out_prefix
	string infile,outfile,cplist,junkfl2
	string newfield, imglist, fieldn, out
        file   t_files
	real   strate,maxrate, ratestep, realj
	real   stang, maxang,  angstep, season
	int    inta, intb, lr, hr

# set low and high reject parameters
        t_files = inlist
        t_in_prefix = in_prefix
        t_out_prefix = out_prefix
	lr = plr
	hr = phr
	out = pout

	print(" ")
	print(" ") 
	print(" *************************") 
	print(" * Spring GRID SHIFTING PROGRAM *")
	print(" *************************") 
	print(" ") 
	print(" Input to this program is from N registered images! ") 
	print(" All images will be aligned to the first. ")
	print(" ") 
	print(" ***** NEW -------->>> OUTPUT IMAGES are in "//out//" format.")
	print(" ") 
	print(" **** WARNING!!! ------->>> MODIFIED for WFI.")
	print(" ") 
	fieldn = pfieldn
	strate = pstrate
	maxrate = pmrate
	ratestep = prstep

	stang = pstang
	season = 1
	if (stang < 0.0) {season = -1}
	stang = season*stang
	stang = stang/57.29578
	maxang = pmang
	maxang = season*maxang
	maxang = maxang/57.29578
	angstep = pastep
	angstep = angstep/57.29578

# NOTE SIGNS ON ANGLES
   for (aangle=stang; aangle<=maxang; aangle+=angstep)
   {
   angle = season*aangle
   angledeg = season*angle*57.29578

     for (rate=strate; rate<=maxrate; rate+=ratestep)
     {
	print(" **************************************************") 
	print(" Processing files for rate: ",rate, ' angle: ',angledeg)
	print(" ") 

        list = t_files
        i = fscan(list, s1) 
        infile=s1
        print(infile)
	outfile = t_out_prefix//infile
        infile = t_in_prefix//infile
	print(" copying image "//infile//" to "//outfile) 
	imcopy(infile,outfile)

	gettimenew(infile,fieldn)
	strttime = gettimenew.outputime
	print(" **  Image "//infile//" acquired at ", strttime, " UT") 

	xsh0 = rate*cos(angle)
	ysh0 = rate*sin(angle)
        while(fscan(list, infile)!=EOF){
        i = i + 1
		print("       Currently working on: ",infile)
		gettimenew(infile,fieldn)
		currtime = gettimenew.outputime
		print(" * Image ",i," acquired at ", currtime, " UT") 

#  First shift this file by the required amount, then you
#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
#
		ysh = -ysh0*(currtime - strttime)
		xsh = -xsh0*(currtime - strttime)
		print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
		print(" xshift ",xsh," and yshift ",ysh) 

		junkfl2 = t_out_prefix//infile
#		print(" xshifting by ",xsh," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh,ysh,shifts_file="")

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

	imcombine(t_in_prefix//'@'//t_files,outfile,logfile="",combine="average",reject="minmax",
            outtype=out,scale='none',nlow=lr,nhigh=hr,nkeep=1)

        imreplace(outfile,-100.0,lower=INDEF,upper=-100.)
#               Record shift rates in header
       	newfield = 'shftrate'
       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
       	newfield = 'shftangle'
       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
       	newfield = 'lowrej'
       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
       	newfield = 'highrej'
       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

	outfile = junkfl2//'med'
	print( " " )
	print( " Putting medianed image into: ",outfile)
	imcombine('sh//@'//t_files,outfile,logfile="",combine="median",reject="minmax",
            outtype=out,scale='none',nlow=lr,nhigh=hr,nkeep=1)
#       Record shift rates in header
       	newfield = 'shftrate'
       	hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
       	newfield = 'shftangle'
       	hedit(outfile,newfield,(angledeg),add+,show-,update+,verify-)
       	newfield = 'lowrej'
       	hedit(outfile,newfield,(lr),add+,show-,update+,verify-)
       	newfield = 'highrej'
       	hedit(outfile,newfield,(hr),add+,show-,update+,verify-)

	print(" ")
	print(" Deleting shifted images! ")
	imdel(t_out_prefix//'*.fits', go_ahead+,verify-)
	print(" ")
	print(" Done ")
	print(" ")

     }
   }
	print(" Really done ")
	print(" ")
end

	

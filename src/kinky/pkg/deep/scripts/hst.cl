#
#   hst.cl  
#
#  shifts nimg images 
#  Reads shifts from a file.
#  Creates a median image with the highest pixel rejected.
#
#  NOTE: DOESN'T require gettime script.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES SHOULD BE PREVIOUSLY BACKGROUND SUBTRACTED AND SCALED.
#

procedure grid(sinput)

	string sinput {"", prompt=" filename of .txt file (MUST HAVE PERIOD) "}

begin 
	int    nimg, dispflg 
	real   rate,xsh0,ysh0,angle,angledeg
	real   dt,xsh[4],ysh[4],se,sn,wf2x,wf2y,wf3x,wf3y,wf4x,wf4y
	real   currtime, strttime
	string infile,rootword,outfile,cplist,junkfl1,junkfl2
	string newfield,imglist,intable,cntjunk
	real   strate,maxrate, ratestep, realj
	real   stang, maxang,  angstep
	int    inta, intb, lr, hr, periodloc
	string hansin
	int    nsigpix, xmin, ymin, xmax, ymax, nfound
	real   snrthresh
	string dummy

# set parameters for Hans' code
	nsigpix = 4
	snrthresh = 4.5
	xmin = 60
	xmax = 685
	ymin = 40
	ymax = 780

# set low and high reject parameters
	lr = 0
	hr = 1

	print(" ")
	print(" ") 
	print(" *************************") 
	print(" * HST SHIFTING PROGRAM *")
	print(" *************************") 
	print(" ") 
	print(" Input to this program is from aligned images! ") 
	print(" Creates only one output image. ")
	print(" ") 

	intable = sinput
	print(" Reading: ",intable)
	list=intable

	nimg = 0
#--------------------------------------------
	while(fscan(list,rootword,dt,se,sn,wf2x,wf2y,wf3x,wf3y,wf4x,wf4y) != EOF)
	{
	   nimg= nimg+1
	   print("************ ",nimg," *************")
	   xsh[2] = wf2x
	   ysh[2] = -wf2y
	   xsh[3] = wf3x
	   ysh[3] = -wf3y
	   xsh[4] = wf4x
	   ysh[4] = -wf4y

	   for(i=2; i<=4; i+=1)
	   {
#	       print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#	       print(" xshift ",xsh[i]," and yshift ",ysh[i]) 
		infile = 'wf'
		infile = infile//i 
		infile = infile + '/wf' 
		infile = infile//i 
		infile = infile + 'b' 
		infile = infile//nimg 

		junkfl2 = 'wf'
		junkfl2 = junkfl2//i 
		junkfl2 = junkfl2 + '/hsttmp' 
	        inta = 100 * i + nimg
		junkfl2 = junkfl2//inta
		print(" xshifting ",infile, " by ",xsh[i]," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh[i],ysh[i])
	   }
	}
### END READ AND SHIFT LOOP ######### ALL hsttmp IMAGES NOW EXIST #########

        print("*****************************************************")
	print("* Finished shifting *")
	print("*********************")
	print("* Creating medians  *")
	print("*********************")
	periodloc = stridx(".",intable)
	rootword=substr(intable,1,periodloc-1)
	for(i=2; i<=4; i+=1)
	{
	    imglist = 'wf'
	    imglist = imglist//i
	    imglist = imglist + '/hsttmp*.imh'
	    outfile = 'wf'
	    outfile = outfile//i
	    outfile = outfile +'/' + rootword
	    outfile = outfile//i
	    outfile = outfile +'.fits' 
	    print(" taking median of ",imglist, " and creating: ",outfile) 
#	    imcombine(imglist,outfile,logfile="",combine="median",reject="none",scale="exposure",expname="EXPTIME")
	    imcombine(imglist,outfile,logfile="",combine="median",reject="minmax",nlow=lr,nhigh=hr,nkeep=1,scale="exposure",expname="EXPTIME")
	    print(" Deleting tmp images in wf",i) 
	    imdel(imglist,verify-)

# WRITE INPUT FILE FOR HANS
	    hansin="hans.input"
	    junkfl1 = '/big/t/HST/' 
	    junkfl1 = junkfl1 // outfile
	    print(junkfl1, > hansin)
	    print(nsigpix, snrthresh, >> hansin)
	    print(xmin, xmax, >> hansin)
	    print(ymin, ymax, >> hansin)
# CALL HANS' CODE
	    !find_obj
	    list='hans.output'
	    dummy = fscan(list,nfound) 
	    print("     **************  nfound : ",nfound)	
#	    if(nfound .lt. 0) print '*PANIC*************'
# DELETE TEMPORARY FILES
	    delete(hansin, verify-)
	    junkfl2 = 'hans_dummy' 
	    delete(junkfl2, verify-)
	    junkfl2 = 'hans.output' 
	    delete(junkfl2, verify-)
	    
	}
##### Loop back to analyse next median image ################

	print(" ")
	print(" Done ")
	print(" ")
end

	

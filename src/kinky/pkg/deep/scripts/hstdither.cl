#
#   hstdither.cl  
#
#  shifts nimg images  INTENDED ONLY TO REMOVE DITHER
#  READS image names and shifts from a file.
#  Creates a summed and #  median image with the highest pixel rejected.
#
#  NOTE: DOESN'T require gettime script.
#
#  Input requirements: There must be N(=nimg) registered image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES SHOULD BE PREVIOUSLY BACKGROUND SUBTRACTED AND SCALED.
#

procedure grid(sinput)

	string sinput {"", prompt=" filename of dithers"}

begin 
	int    nimg, dispflg, i, j 
	real   rate,xsh0,ysh0,angle,angledeg
	real   dt,xsh[4],ysh[4],se,sn,wf2x,wf2y,wf3x,wf3y,wf4x,wf4y
	real   currtime, strttime
	string infile,rootword,outfile,medfile,junkfl1,junkfl2
	string newfield,imglist,intable,cntjunk
	real   strate,maxrate, ratestep, realj
	real   stang, maxang,  angstep
	int    inta, intb, lr, hr, scale
	string stime
	real   extime, mtime, ratio

# set low and high reject parameters
	lr = 0
	hr = 1

	print(" ")
	print(" ") 
	print(" ***************************") 
	print(" * HST ANTI-DITHER PROGRAM *")
	print(" ***************************") 
	print(" ") 
	print(" Input to this program is from N images! ") 
	print(" All images will be aligned to the first. ")
	print(" ALL images are saved to disk. ")
	print(" ") 

	intable = sinput
	print(intable)
	list=intable

	nimg = 0
#--------------------------------------------
     while(fscan(list,rootword,dt,se,sn,wf2x,wf2y,wf3x,wf3y,wf4x,wf4y) != EOF)
	{
	   nimg= nimg+1
#	   print("************ ",nimg," *************")
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
		infile = 'u5589' + rootword + '.c0h['
		infile = infile//i 
		infile = infile + ']' 
		junkfl2 = 'wf'
		junkfl2 = junkfl2//i 
		junkfl2 = junkfl2 + '/wf' 
		junkfl2 = junkfl2//i
# a identifier indicates aligned images.
		junkfl2 = junkfl2 +'a'
		junkfl2 = junkfl2//nimg
#		hselect(infile,"CRVAL1","yes")
		print(" xshifting ",infile, " by ",xsh[i]," pixels and creating: ",junkfl2) 
		imshift(infile,junkfl2,xsh[i],ysh[i])
	    }
	}

# END READ AND SHIFT LOOP ############# ALL aligned IMAGES NOW EXIST #####

	print("***********************************************")
        print("* Finished shifting *")
        print("*********************")
        print("* Creating masters  *")
        print("*********************")
	for(i=2; i<=4; i+=1)
        {
            imglist = 'wf'
            imglist = imglist//i
            imglist = imglist + '/wf'
            imglist = imglist//i
            imglist = imglist + 'a*.imh'
            medfile = 'wf'
            medfile = medfile//i
            medfile = medfile +'/wf'
            medfile = medfile//i
            medfile = medfile + 'master'
            print(" taking median of ",imglist, " and creating: ",medfile) 
            imcombine(imglist,medfile,logfile="",combine="median",reject="none", scale="exposure",expname="EXPTIME")
	    imgets(medfile,"EXPTIME")
            stime = imgets.value
            mtime = real(stime)

	    for(j=1; j<=16; j+=1)
            {
		imglist = 'wf'
            	imglist = imglist//i
            	imglist = imglist + '/wf'
            	imglist = imglist//i
            	imglist = imglist + 'a'
            	imglist = imglist//j
		imgets(imglist,"EXPTIME")
		stime = imgets.value
		extime = real(stime)
		ratio = mtime/extime
           	junkfl2 = 'wf'
            	junkfl2 = junkfl2//i
            	junkfl2 = junkfl2 +'/scaled'
		print("    scaling ",imglist," by ",ratio," to ",junkfl2)
		imarith(imglist,'*',ratio,junkfl2)
           	outfile = 'wf'
            	outfile = outfile//i
            	outfile = outfile + '/wf'
            	outfile = outfile//i
# b identifier indicates subtracted images.
            	outfile = outfile + 'b'
            	outfile = outfile//j
		print("    subtracting ",medfile," to give ",outfile)
		imarith(junkfl2,'-',medfile,outfile)
		imdel(junkfl2,verify-)
            }
        }
	print(" ")
	print(" Done ")
	print(" ")
end

	

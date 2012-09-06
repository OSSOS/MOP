#
#   ccmatch.cl  
#
#  Matchs a USNO catalogue with a plate solution.
#
# TELESCOPE CODES (internal to this script):
#  telesc : CFHT, ESO2, ESO3, UT-1, Pal5
#  future : KPNO, NoOT

#procedure ccmatch(sinput,simg,sscale,sminmag,smaxmag,strial)
procedure ccmatch(simg,sscale,sminmag,smaxmag,strial)

#	string sinput {"", prompt=" filename of RA/DECs/MAG/ID?"}
	string simg   {"", prompt=" image to match"}
	real sscale   {"", prompt=" pixel scale (arcsec/pix) "}
	real sminmag  {"", prompt=" BRIGHTEST star to keep (Bmag) "}
	real smaxmag  {"", prompt=" FAINTEST  star to keep (Bmag) "}
        int  strial   {5, prompt=" Number of trial stars?" }
	real sbuf     {10, prompt=" Buffer region around edges? (pix) "}
        int  sbox     {9, prompt=" Box size for centroiding? (pix) "}
begin 
	int    i,xsize,ysize,xcent,ycent,nstars,ngood,box
	real   thisra,thisdec,thismag,thisid,scale
	real   minmag,maxmag,buf
	real   ra[9000],dec[9000],mag[9000]
	real   racent,deccent,xpix[9000],ypix[9000]
	real   gra[9000],gdec[9000],gmag[9000],gid[9000]
	real   gx[9000],gy[9000]
        real    id[9000]
	string inimg,stringra,stringdec,stringeq,stringtel,telesc
	string intable, tvfile, bfile, jfile, trialfile, solfile, trialsol
        string now, command, pointfile
	real   x1,y1,equinox
        int    wcs, closest, count, ntrial
	real   dist,dxs,dys, xcen,ycen
        string centerfile, lab0, lab1, lab2
	real   thisx,thisy
	string filexyradec
	int    nchip, offlag, firstflag
	real   xx1,yy1,xx2,yy2

	print(" ")
	print(" ") 
	print(" ******************************") 
	print(" * Catalogue Matching Program *")
	print(" * multiple telescope version *")
	print(" ******************************") 
	print(" -- output file is usno.ccmap  ") 
	print(" ") 
	print(" KPNO and NOT are not tested yet...") 
	print(" --->>> Don't use on KPNO data without rotation!") 
	print(" ") 

#	intable = sinput
	intable = 'usno.ccmap'
	set stdimage = imt4096
#	print(intable)
	list=intable
        inimg = simg
	scale = sscale
	minmag = sminmag
	maxmag = smaxmag
	ntrial = strial
	buf = sbuf
	box = sbox

#	xsize = sxsize
	imgets(inimg,'i_naxis1')
	xsize = int(imgets.value)
	xcent = xsize/2
#	ysize = sysize
	imgets(inimg,'i_naxis2')
	ysize = int(imgets.value)
	ycent = ysize/2
	print(" ") 
	print(' Image pixel center : ',xcent,ycent)

#--------------------------------------------
# Attempt to identify telescope
#
	imgets(inimg,'TELESCOP')
        stringtel = imgets.value
        telesc = 'UNKN'
	if(stringtel == 'MPG/ESO-2.2') telesc = 'ESO2'
	if(stringtel == 'ESO-3P6')     telesc = 'ESO3'
	if(stringtel == 'ESO-NTT')     telesc = 'NTT'
	if(stringtel == 'ESO-VLT-U1')  telesc = 'UT-1'
	if(stringtel == 'CFHT 3.6m')   telesc = 'CFHT'
# Palomar does not currently have TELESCOP keyword, but...
	if(telesc == 'UNKN') {
	   imgets(inimg,'DETECTOR')
           stringtel = imgets.value
	   if(stringtel == 'TEK3') telesc = 'Pal5'
        }
	print(' TELESCOPE : ',telesc)
#--------------------------------------------
# Get field center estimate from header
#
	offlag = 0
        imgets(inimg,'EQUINOX')
        stringeq = imgets.value
        equinox=real(stringeq)
	imgets(inimg,'RA')
        stringra = imgets.value
	imgets(inimg,'DEC')
        stringdec = imgets.value
	if(telesc=='NOT'||telesc=='ESO2'||telesc=='NTT'){
            racent = real(stringra) 
        } else {
            racent = real(stringra) * 15.0
        } ;
	if(telesc=='ESO2') racent = racent * 15.0
	deccent = real(stringdec)
        print('*************************')
        print('*HEADER Equinox: ',equinox,' *')
        if(equinox<1999.0)print('>>>>>>>>>>>>>>>>>>>>> WARNING, Not J2000 !!')
        if(equinox>2001.0)print('>>>>>>>>>>>>>>>>>>>>> WARNING, Not J2000 !!')
	print('***************************************')
	print('*HEADER  RA    : ',stringra,'  ',racent)
	print('*HEADER  DEC   : ',stringdec,'  ',deccent)
        if (telesc == 'CFHT') command = "c"
        if (telesc == 'ESO2') command = "w"
        if (telesc == 'ESO3') command = "3"
        if (telesc == 'Pal5') command = "p"
        if (telesc == 'UT-1') command = "v"
        if (telesc == 'KPNO') command = "k"
        if (telesc == 'NTT') command = "n"
	
        if (telesc == 'UNKN') goto centchk
	print(' ')
	print(' Telescope Identified : ',telesc)
	print(' ')
        goto offset

   centchk:
	print(' ')
	print(' Telescope UNKNOWN ')
	print('***************************************')
	print(' Do you wish to : ')
	print(' Q - Quit CCMATCH.cl ')
	print(' u - use this as image center. ')
	print(' m - manually input RA/DEC of image center. ')
# To prevent accidental multiple offsets, the following only appear once.
#        if(offlag == 0) {
#	  print(' k - Kitt Peak approx offset from this (N-left,E-down)')
#	  print(' c - CFH12K  approx offset from this (new 2000 config)')
#        }
	print("   >>IMCURSOR<<                        ")
	print(" **************************************")
        now= fscan(imcur, x1, y1, wcs, command)
#--------------------------------------------
   offset:
#--------------------------------------------
# ESO 3.6m (ASSUMING EFOSC2!)
        if (command == "3" && offlag == 0) {
	    imgets(inimg,'INSTRUME')
            print(" INSTRUMENT: ",imgets.value)
	    offlag = 1
	    print('*nominal  RA    : ',racent)
	    print('*nominal  DEC   : ',deccent)
            print('------>>>> USEFUL INFORMATION <<<<------- ')
	    pwd
	    print('usnoget ',racent,' ',deccent,' 8.0')
            print(' ')
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# ESO NTT (ASSUMING SUSI!)
        if (command == "n" && offlag == 0) {
	    imgets(inimg,'INSTRUME')
            print(" INSTRUMENT: ",imgets.value)
	    offlag = 1
	    print('*nominal  RA    : ',racent)
	    print('*nominal  DEC   : ',deccent)
            print('------>>>> USEFUL INFORMATION <<<<------- ')
	    pwd
	    print('usnoget ',racent,' ',deccent,' 14.0')
            print(' ')
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# ESO VLT (UT1 or UT2)
        if (command == "v" && offlag == 0) {
	    imgets(inimg,'INSTRUME')
            print(" INSTRUMENT: ",imgets.value)
	    offlag = 1
	    print('*nominal  RA    : ',racent)
	    print('*nominal  DEC   : ',deccent)
            print('------>>>> USEFUL INFORMATION <<<<------- ')
	    pwd
	    print('usnoget ',racent,' ',deccent,' 9.0')
            print(' ')
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# Palomar 5m, assuming no re-imager
        if (command == "p" && offlag == 0) {
	    offlag = 1
	    print('*nominal  RA    : ',racent)
	    print('*nominal  DEC   : ',deccent)
            print('------>>>> USEFUL INFORMATION <<<<------- ')
	    pwd
	    print('usnoget ',racent,' ',deccent,' 13.0')
            print(' ')
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# ESO 2.2m Mosaic
        if (command == "w" && offlag == 0) {
	    imgets(inimg,'CHIPINDE')
	    nchip = int(imgets.value)
            print(" WFI mosaic chip number: ",nchip)
	    if(nchip > 9 || nchip < 1) print("********** PANIC on chip number")
# 9 arcmin = 0.15 degree E or W offset always, but correct for declination
	    if(nchip > 0 && nchip < 5) deccent = deccent + 0.140
	    if(nchip > 4 && nchip < 9) deccent = deccent - 0.140
	    if(nchip == 1 || nchip == 5) racent=racent+0.19/cos(deccent/57.3)
	    if(nchip == 2 || nchip == 6) racent=racent+0.070/cos(deccent/57.3)
	    if(nchip == 3 || nchip == 7) racent=racent-0.070/cos(deccent/57.3)
	    if(nchip == 4 || nchip == 8) racent=racent-0.21/cos(deccent/57.3)
	    offlag = 1
	    print('*new  RA    : ',racent)
	    print('*new  DEC   : ',deccent)
            print('------>>>> USEFUL INFORMATION <<<<------- ')
	    pwd
	    print('usnoget ',racent,' ',deccent,' 19.0')
            print(' ')
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# CFHT
        if (command == "c" && offlag == 0) {
	    imgets(inimg,'CHIPID')
	    nchip = int(imgets.value)
            print(" CFH12K Mosaic chip number: ",nchip)
	    if(nchip > 11 || nchip < 0) print("********* PANIC on chip number")
	    if(nchip > -1 && nchip < 6) deccent = deccent + 0.117
	    if(nchip > 5 && nchip < 12) deccent = deccent - 0.117
	    if(nchip == 0 || nchip == 6)  racent = racent - 0.3138
	    if(nchip == 1 || nchip == 7)  racent = racent - 0.1883
	    if(nchip == 2 || nchip == 8)  racent = racent - 0.0628
	    if(nchip == 3 || nchip == 9)  racent = racent + 0.0628
	    if(nchip == 4 || nchip == 10) racent = racent + 0.1883
	    if(nchip == 5 || nchip == 11) racent = racent + 0.3138
	    offlag = 1
	    print('*new  RA    : ',racent)
	    print('*new  DEC   : ',deccent)
	    print(' ')
            print(" >>>>>>>> I hope that you have rotated N up and E left...!")
	    print(' ')
	    print('---->>>> USEFUL INFORMATION ***************************** ')
	    pwd
	    print('usnoget ',racent,' ',deccent,' 15.0')
	    print('---->>>> ************************************************ ')
	    print(' ')
	    print(' Do you wish to : ')
	    print(' Q - Quit CCMATCH.cl ')
	    print(' u - USE this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# NOT
        if (command == "n" && offlag == 0) {
            print('>> I need a catalogue named usnomap in the correct format ')
            print('---->>>> USEFUL INFORMATION ***************************** ')
            pwd
            print('usnoget ',racent,' ',deccent,' 9.0')
            print('---->>>> ************************************************ ')
            print(' ')
            print(' Do you wish to : ')
            print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
            print(' Q - Quit CCMATCH.cl ')
            print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
         }
#--------------------------------------------
# KPNO Mosaic
        if (command == "k" && offlag == 0) {
	    imgets(inimg,'IMAGEID')
	    nchip = int(imgets.value)
            print(" KPNO Mosaic chip number: ",nchip)
	    if(nchip > 9 || nchip < 1) print("********** PANIC on chip number")
# 9 arcmin = 0.15 degree E or W offset always, but correct for declination
	    if(nchip > 0 && nchip < 5) racent = racent + 0.15/cos(deccent/57.3)
	    if(nchip > 4 && nchip < 9) racent = racent - 0.15/cos(deccent/57.3)
# 13.5 arcmin = 0.225 degree north for chips 1 and 5, etc...
	    if(nchip == 1 || nchip == 5) deccent = deccent + 0.225
	    if(nchip == 2 || nchip == 6) deccent = deccent + 0.075
	    if(nchip == 3 || nchip == 7) deccent = deccent - 0.075
	    if(nchip == 4 || nchip == 8) deccent = deccent - 0.225
	    offlag = 1
	    print('*new  RA    : ',racent)
	    print('*new  DEC   : ',deccent)
	    print('usnoget ',racent,' ',deccent,' 19.0')
	    print('---->>>> YOU MAY WANT TO RUN SKYCAT NOW!!!! *************')
	    print('---->>>> (to create usno.raw in the directory below) ')
	    print('---->>>> (then run ccc to create usno.ccmap) ')
	    pwd
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            print(" *** WARNING: Will ASSUME YOU rotated N up and E left... ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
# Manual.  THIS SHOULD STAY THE LAST CHOICE!
        if (command == "m") {
            print(" Enter RA  of field center in decimal format (0--360) ")
            scan(racent)
            print(" Enter Dec of field center in decimal format (-90,90) ")
            scan(deccent)
            print('---->>>> USEFUL INFORMATION ***************************** ')
            pwd
            print('usnoget ',racent,' ',deccent,' 15.0')
            print('---->>>> ************************************************ ')
            print(' ')
            print(' Do you wish to : ')
            print(' u - use this as image center and CONTINUE. ')
            print(' Q - Quit CCMATCH.cl ')
            print("   >>IMCURSOR<<  ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#--------------------------------------------
        if (command == "Q") bye
        if (command != "u") {
            print(" Unrecognized input...try again...")
	    goto centchk
        }
#------------------------------------------------------------------------
# Read all USNO stars
#
	usno(racent,deccent,500,"usno.ccmap")
        !mv usno.ccmap usno.nosort
        sort('usno.nosort',column=3,numer+, > 'usno.ccmap')
#
	i = 0
        while(fscan(list,thisra,thisdec,thismag,thisid) != EOF)
	{
	   i = i+1
	   ra[i] = thisra
	   dec[i] = thisdec
	   mag[i] = thismag
	   id[i] = thisid
#	   print(i,ra[i],dec[i],mag[i],id[i])
	}

	nstars = i
        print("------------------------nstars ",nstars)

#--------------------------------------------
# Display all stars using header center and approx pix scale
# Store stars which are on image in shortened array.
#
        firstflag = 0
      dispmark:
	print(" ")
	print(" Display ",inimg," with mag range ",minmag," -- ",maxmag)
	display(inimg,1)

        tvfile=mktemp("tmp$tvfile")
        bfile=mktemp("tmp$bfile")
        jfile=mktemp("tmp$jfile")
	ngood = 0
        for (j=1; j<=nstars; j+=1)
        {
           ypix[j]= ycent + (dec[j] - deccent)*3600.0/scale
           xpix[j]= xcent - cos(dec[j]/57.29578)*(ra[j] - racent)*3600.0/scale
	   if (xpix[j] < buf) goto skip;
	   if (xpix[j] > (xsize - buf) ) goto skip;
	   if (ypix[j] < buf) goto skip;
	   if (ypix[j] > (ysize - buf) ) goto skip;
	   if (mag[j] < minmag) goto skip;
	   if (mag[j] > maxmag) goto skip;
           ngood = ngood + 1
	   print(xpix[j],ypix[j],ra[j],dec[j],mag[j],id[j], >> tvfile)
           if (ngood < 6) {
	   print(xpix[j],ypix[j],ra[j],dec[j],mag[j],id[j], >> bfile)
           }
	   gx[ngood] = xpix[j]
	   gy[ngood] = ypix[j]
	   gra[ngood] = ra[j]
	   gdec[ngood] = dec[j]
	   gmag[ngood] = mag[j]
	   gid[ngood] = id[j]

         skip:
	   print(xpix[j],ypix[j],ra[j],dec[j],mag[j],id[j], >> jfile)
        }
        print("marking...")
	tvmark(1,tvfile,mark='circle',radii='22',color=205)
        print("marked.")
        delete(tvfile, verify-)
        delete(jfile, verify-)
#--------------------------------------------
# Have check magnitude range roughly correct to eliminate most saturated stars
#
        trialfile=inimg//".trial"
        delete(trialfile, verify-)
	count = 0
	tvmark(1,bfile,mark='circle',radii='40,43',color=204)
# BG. Following branch skips imexam on first round.
        if (firstflag == 0) goto loopchk
	print(" ******************************************")
	print(" *  You are in IMEXAM!   Determine if the *")
	print(" *  brightest stars (red) are saturated.  *")
	print(" *  q - Quit IMEXAM after radial profiles *")
	print(" *>>IMCURSOR<<                            * ")
	print(" ******************************************")
	imexam()
    loopchk:
	firstflag = 1
	print(" **************************************")
	print(" *  MAGNITUDE range OK?               *")
	print(" *     r = reset mag range            *")
	print(" *     Q = quit                       *")
	print(" *     o = offset correction to center*")
	print(" *     c = Continue processing        *")
	print(" *>>IMCURSOR<<                        * ")
	print(" **************************************")
        now= fscan(imcur, x1, y1, wcs, command)
        if (command == "o" && count == 0) {
	  print(" **************************************")
	  print(" * OFFSET CORRECTION TO CENTER        *")
	  print(" * Please go to on an isolated CIRCLE *")
	  print(" *     Q = quit                       *")
	  print(" *     OTHER = choose this circle     *")
	  print(" **************************************")
	  print(" *>>IMCURSOR<< * ")
	  print(" ***************")
          now= fscan(imcur, x1, y1, wcs, command)
          if (command == "Q") bye
	  dist = 100000000.0
          for (j=1; j<=ngood; j+=1)
          {
	    dxs = (gx[j] - x1)*(gx[j] - x1)
	    dys = (gy[j] - y1)*(gy[j] - y1)
            if ( (dxs + dys) < dist ) {
		dist = dxs + dys
	        closest = j
            }
          }
          print("closest ",closest,gx[closest],gy[closest])
          pointfile=mktemp("tmp$pt")
          print(gx[closest],gy[closest], > pointfile)
	  tvmark(1,pointfile,mark='circle',radii='27',color=206)
          xx1 = gx[closest]
          yy1 = gy[closest]
	  print(" **************************************")
	  print(" * MARK this star's location          *")
	  print(" **************************************")
	  print(" *>>IMCURSOR<< * ")
	  print(" ***************")
          now= fscan(imcur, x1, y1, wcs, command)
          xcent = xcent + (x1-xx1)
          ycent = ycent + (y1-yy1)
	  goto dispmark
        }
        if (command == "r" && count == 0) {
	  print(" ")
	  print(" Resetting mag range -- Old range: ",minmag," -- ",maxmag)
	  print(" Brightest mag to keep? ")
	  scan(minmag)
	  print(" Faintest mag to keep? ")
	  scan(maxmag)
	  goto dispmark
        }
        if (command == "Q") bye
        if (command != "c") goto loopchk
# don't need bright star file anymore
        delete(bfile, verify-)
#--------------------------------------------
# Have user click on a star and a corresponding circle
#
	print(" ")
	print(" *************************************************")
	print(" * You are going to mark ",ntrial," stars for me *")
	print(" *************************************************")
   select3:
	print(" ")
	print(" -----------> Star : ", (count+1) )
	print(" **************************************")
	print(" * Please go to on an isolated CIRCLE *")
	print(" *     Q = quit                       *")
	print(" *     OTHER = choose this circle     *")
	print(" **************************************")
	print(" *>>IMCURSOR<< * ")
	print(" ***************")
        now= fscan(imcur, x1, y1, wcs, command)
#	print(" x1,y1,command ",x1,y1,command)
        if (command == "Q") bye
	dist = 100000000.0
        for (j=1; j<=ngood; j+=1)
        {
	    dxs = (gx[j] - x1)*(gx[j] - x1)
	    dys = (gy[j] - y1)*(gy[j] - y1)
            if ( (dxs + dys) < dist ) {
		dist = dxs + dys
	        closest = j
            }
        }
        print("closest ",closest,gx[closest],gy[closest])
        pointfile=mktemp("tmp$pt")
        print(gx[closest],gy[closest], > pointfile)
	tvmark(1,pointfile,mark='circle',radii='27',color=206)
        delete(pointfile, verify-)

	print(" *******************************")
	print(" * Now the corresponding star  *")
	print(" *     Q = quit                *")
	print(" *     OTHER = mark the star   *")
	print(" *******************************")
	print(" *>>IMCURSOR<< * ")
	print(" ***************")
        now= fscan(imcur, x1, y1, wcs, command)
        if (command == "Q") bye
        centerfile = inimg//".cen"
        imcntr(inimg,x1,y1,cboxsize=box, >> centerfile)
        list = centerfile
        now = fscan(list,lab0,lab1,xcen,lab2,ycen)
        print("star centroid : ",xcen,ycen)
#       mark for reference to not accidentally re-chosen later
        pointfile=mktemp("tmp$pt")
        print(xcen, ycen, > pointfile)
	tvmark(1,pointfile,mark='circle',radii='30',color=203)
#	tvmark(1,pointfile,mark='circle',radii='30,33',color=203)
        delete(pointfile, verify-)
        delete(centerfile, verify-)

	count = count + 1
	print(xcen,ycen,gra[closest],gdec[closest],gid[closest])	
	print(xcen,ycen,gra[closest],gdec[closest],gid[closest], >> trialfile)	

	if (count < ntrial ) goto select3
#---------------------------------------------------------
#  Compute trial plate solution from this small sample
#  This is done in case there is a non-negligible rotation.
#

        solfile=mktemp("tmp$solfile")
        trialsol = "first.sol"

#     solfile = "ccmap.trial"

#  ccmap(trialfile,solfile,results="",xcol=1,ycol=2,lngcol=3,latcol=4,xmin="INDEF",xmax="INDEF",ymin="INDEF",ymax="INDEF",lngunits="degrees",latunits="degrees",insystem="j2000",refpoint="coords")

	ccmap(trialfile,solfile,solutions=trialsol,results="",xcol=1,ycol=2,lngcol=3,latcol=4,lngunits="degrees",latunits="degrees",lngrefunits="degrees",latrefunits="degrees",refpoint="coords",inter+, xxorder=2, yyorder=2, xxterms="half", yxterms="half", yxorder=2, xyorder=2 )

#---------------------------------------------------------
#   Now display all stars under this rough solution.
#
        tvfile=mktemp("tmp$tvfile")
        bfile=mktemp("tmp$bfile")
        cctran(intable,tvfile,solfile,trialsol,forward-,xcol=1,ycol=2,lngformat="%13.7g",latformat="%13.7g")
# delete old solution, not needed now
        delete(solfile, verify-)

        list = tvfile
	i = 0
        while(fscan(list,thisx,thisy,thismag,thisid) != EOF)
	{
	   i = i+1
#	   print("i thisx thisy thismag thisid ", i,thisx,thisy,thismag,thisid)
	   xpix[i] = thisx
	   ypix[i] = thisy
	   if (id[i] != thisid) print(" PANIC, file mismatch ",i)
	}
        delete(tvfile,verify-)
# next line debug
	print(" ")
	print(" ----------------------------- nstars i ", nstars, i)
        nstars = i

        ngood = 0
        for (j=1; j<=nstars; j+=1)
        {
# reject bad stars
#           print(" j,xpix,ypix ", j, xpix[j],ypix[j])
	   if (xpix[j] < buf) goto skip2
	   if (xpix[j] > (xsize - buf) ) goto skip2
	   if (ypix[j] < buf) goto skip2
	   if (ypix[j] > (ysize - buf) ) goto skip2
	   if (mag[j]  < minmag) goto skip2
	   if (mag[j]  > maxmag) goto skip2

###        Beware, second usage of this file, but previous already deleted.
           centerfile = mktemp("tmp$cen")
###        Centroid on this guessed stellar center, and retrieve
           imcntr(inimg,xpix[j],ypix[j],cboxsize=box, >> centerfile)
           list = centerfile
           now = fscan(list,lab0,lab1,x1,lab2,y1)
#           print(" i x1 y1 ",i,x1,y1)
           delete(centerfile, verify-)

           ngood = ngood + 1
#	   print(x1,y1,ra[j],dec[j],mag[j],id[j])
	   print(x1,y1,ra[j],dec[j],mag[j],id[j], >> bfile)
           goto skip2

         skip2:
#           print("Done i ",i)
	}


	print(" Displaying again")
	display(inimg,1)
	tvmark(1,bfile,mark='circle',radii='22',color=205)
  loopc2:
        print(" **************************")
        print(" Trial plate solution done ")
        print("      DID IT LOCK ON?      ")
        print(" * Q to exit and restart   ")
        print(" * c to continue           ")
        print(" **************************")
	print(" *>>IMCURSOR<<*            ")
        print(" ***************")
        now= fscan(imcur, x1, y1, wcs, command)
        if (command == "Q") bye
        if (command != "c") goto loopc2
        delete(trialfile,verify-)

        print(" ")
        print(" ***********************************")
        print(" * Voila! Reject outliers and quit *")
        print(" ***********************************")
        solfile=inimg//".fullsol"
#  If there is an older version of this file it would append. Don't want that!
        delete(solfile, go_ahead+, verify-)
        trialsol = "full.sol"
	ccmap(bfile,solfile,solutions=trialsol,results="",xcol=1,ycol=2,lngcol=3,latcol=4,lngunits="degrees",latunits="degrees",lngrefunits="degrees",latrefunits="degrees",refpoint="coords",inter+)

        delete(bfile,verify-)
	print(" ")
	print(" Wrote :")
	print(solfile)
	print(" ")
	print(" Done ")
 
end

	

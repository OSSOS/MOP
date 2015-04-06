#
#   ccmatch.cl  
#
#  Matchs a USNO catalogue with a plate solution.
#
# TELESCOPE CODES (internal to this script):
#  telesc : CFHT, ESO2, ESO3, UT-1, UT-2, Pal5
#  future : KPNO, NoOT

#procedure ccmatch(sinput,simg,sscale,sminmag,smaxmag,strial)
procedure ccmatch(simg,sscale,sminmag,smaxmag,strial)

#	string sinput {"", prompt=" filename of RA/DECs/MAG/ID?"}
	string simg   {"", prompt=" image to match"}
	real sscale   {0.1454, prompt=" pixel scale (arcsec/pix) "}
	real sminmag  {10., prompt=" BRIGHTEST star to keep (Bmag) "}
	real smaxmag  {14., prompt=" FAINTEST  star to keep (Bmag) "}
        int  strial   {4, prompt=" Number of trial stars?" }
	real sbuf     {9, prompt=" Buffer region around edges? (pix) "}
        int  sbox     {9, prompt=" Box size for centroiding? (pix) "}
begin 
	int    i,xsize,ysize,nstars,ngood,box
	real   thisra,thisdec,thismag,thisid,scale
	real   crval1,crval2,crpix1,crpix2,cd1_1,cd2_2, cd1_2, cd2_1
	real   minmag,maxmag,buf
	real   ra[9000],dec[9000],mag[9000]
	real   racent,deccent,xpix[9000],ypix[9000]
	real   gra[9000],gdec[9000],gmag[9000],gid[9000]
	real   gx[9000],gy[9000]
        int    id[9000]
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
	print(" NOT and FLW1 are not yet fully tested ...") 
	print(" --->>> Don't use on KPNO data without rotation!") 
	print(" ") 

#	intable = sinput
	intable = 'usno.ccmap'
#	print(intable)
        inimg = simg
	scale = sscale
	cd2_2 = -scale/3600.0
	cd1_1 = scale/3600.0
	minmag = sminmag
	maxmag = smaxmag
	ntrial = strial
	buf = sbuf
	box = sbox

#--------------------------------------------
# Attempt to identify telescope
#
	imgets(inimg,'TELESCOP')
        stringtel = imgets.value
        telesc = 'UNKN'
	if(stringtel == 'Gemini-North') telesc = 'GEMN'
	if(stringtel == 'MPG/ESO-2.2') telesc = 'ESO2'
	if(stringtel == 'ESO-3P6')     telesc = 'ESO3'
	if(stringtel == 'ESO-VLT-U1')  telesc = 'UT-1'
	if(stringtel == 'ESO-VLT-U2')  telesc = 'UT-2'
	if(stringtel == 'CFHT 3.6m')   telesc = 'CFHT'
	if(stringtel == '1.2M')        telesc = 'FLW1'
	if(stringtel == 'KPNO 4.0 meter telescope')   telesc = 'KP4m'
	if(stringtel == 'CTIO 4.0 meter telescope')   telesc = 'CTIO'
# Palomar does not currently have TELESCOP keyword, but...
	if(telesc == 'UNKN') {
	   imgets(inimg,'DETECTOR')
           stringtel = imgets.value
	   if(stringtel == 'TEK3') telesc = 'Pal5'
	   if(stringtel == 'CCDMosaThin1') telesc = 'KP4m'
        }
	print(' TELESCOPE : ',telesc)
#
#--------------------------------------------
# Get the xsize and ysize
        if(telesc=='GEMN'){
        imgets(inimg,'naxis1')
        xsize = int(imgets.value)
        imgets(inimg,'naxis2')
        ysize = int(imgets.value)
        } else {
        imgets(inimg,'i_naxis1')
        xsize = int(imgets.value)
        imgets(inimg,'i_naxis2')
        ysize = int(imgets.value)
        } ;
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
#	if(telesc=='NOT'||telesc=='ESO2'||telesc=='UT-1'){
	if(telesc=='NOT'||telesc=='ESO2'||telesc=='GEMN'||telesc=='UT-1'){
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
        if (telesc == 'CTIO') command = "t"
        if (telesc == 'ESO2') command = "w"
        if (telesc == 'ESO3') command = "3"
        if (telesc == 'Pal5') command = "p"
        if (telesc == 'UT-1') command = "g"
        if (telesc == 'UT-2') command = "g"
        if (telesc == 'KP4m') command = "k"
        if (telesc == 'FLW1') command = "f"
	if (telesc == 'GEMN') command = "g"
	
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
# Gemini North  (repike)
        if (command == "g" && offlag == 0) {
            offlag = 1
            imgets(inimg,'CRPIX1')
            crpix1 = real(imgets.value)
            imgets(inimg,'CRPIX2')
            crpix2 = real(imgets.value)
            imgets(inimg,'CRVAL1')
            crval1 = real(imgets.value)
            imgets(inimg,'CRVAL2')
            crval2 = real(imgets.value)
            imgets(inimg,'CD1_1')
            cd1_1 = real(imgets.value)
            imgets(inimg,'CD2_2')
            cd2_2 = real(imgets.value)
            imgets(inimg,'CD1_2')
            cd1_2 = real(imgets.value)
            imgets(inimg, 'CD2_1')
            cd2_1 = real(imgets.value)
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
	    imgets(inimg,'IMAGEID')
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
	    imgets(inimg,'CRVAL1')
	    crval1 = real(imgets.value)
	    imgets(inimg,'CRVAL2')
	    crval2 = real(imgets.value)
	    imgets(inimg,'CRPIX1')
	    crpix1 = real(imgets.value)
	    imgets(inimg,'CRPIX2')
	    crpix2 = real(imgets.value)
	    imgets(inimg,'CD1_1')
	    cd1_1 = real(imgets.value)
	    imgets(inimg,'CD2_2')
	    cd2_2 = real(imgets.value)
	    imgets(inimg,'CD1_2')
	    cd1_2 = real(imgets.value)
	    imgets(inimg, 'CD2_1')
	    cd2_1 = real(imgets.value)
	    racent   = crval1 + cd1_1*(xsize/2.0 - crpix1) + cd1_2 * (xsize/2.0 - crpix1)
	    deccent  = crval2 + cd2_2*(ysize/2.0 - crpix2) + cd2_1 * (ysize/2.0 - crpix2)
	    print('*new (CRVAL based)  RA    : ',racent)
	    print('*new (CRVAL based)  DEC   : ',deccent)
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
# FLWO 4shooter
        if (command == "f" && offlag == 0) {
            imgets(inimg,'IMAGEID')
            nchip = int(imgets.value)
            print(" FLWO 4shooter chip number: ",nchip)
            if(nchip > 4 || nchip < 1) print("********** PANIC on chip number")
            if(nchip == 1){
                racent = racent - 0.095
                deccent = deccent - 0.095
                }
            if(nchip == 2){
                racent = racent + 0.095
                deccent = deccent - 0.095
                }
            if(nchip == 3){
                racent = racent + 0.095
                deccent = deccent + 0.095
                }
            if(nchip == 4){
                racent = racent - 0.095
                deccent = deccent + 0.095
                }
            offlag = 1
            print('*new  RA    : ',racent)
            print('*new  DEC   : ',deccent)
	    pwd
            print('---->>>> USEFUL INFORMATION ***************************** ')
            pwd
            print('usnoget ',racent,' ',deccent,' 19.0')
            print('---->>>> ************************************************ ')
            print(' ')
            print(' Do you wish to : ')
            print(' u - use this as image center and CONTINUE. ')
            print(' Q - Quit CCMATCH.cl ')
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
	    if(nchip == 3 || nchip == 7) deccent = deccent - 0.079
	    if(nchip == 4 || nchip == 8) deccent = deccent - 0.225
	    offlag = 1
	    print('*new  RA    : ',racent)
	    print('*new  DEC   : ',deccent)
	    print(' ')
	    print('---->>>> USEFUL INFORMATION ***************************** ')
	    print(' ')
	    pwd
 	    print('search_usno.pl --ra ',racent,' --dec ',deccent,' --xsize ',1.3*4096*scale/2.0,' --ysize ',1.3*2048*scale/3600.0,' --file usno.ccmap --num 1000' )
	    #print('usnoget ',racent,' ',deccent,' 19.0')
	    print('---->>>> ************************************************ ')
	    print(' ')
	    print(' Do you wish to : ')
	    print(' u - use this as image center and CONTINUE. ')
	    print(' m - manually input RA/DEC of image center. ')
	    print(' Q - Quit CCMATCH.cl ')
	    print("   >>IMCURSOR<<  ")
            print(" *** WARNING: Will ASSUME YOU rotated N up and E left... ")
            now= fscan(imcur, x1, y1, wcs, command)
        }
#------------------------------------------------
# CTIO Mosaic
        if (command == "t" && offlag == 0) {
	    imgets(inimg,'IMAGEID')
	    # IMAGEID returns numbers 1-16 (2 amps/ccd)
	    nchip = int(imgets.value)
            print(" CTIO Mosaic chip/amp number: ",nchip)
	    if(nchip > 17 || nchip < 1) print("********** PANIC on chip number")
# 9 arcmin = 0.15 degree E or W offset always, but correct for declination
	    if(nchip > 0 && nchip < 9) racent = racent - 0.15/cos(deccent/57.3)
	    if(nchip > 8 && nchip < 17) racent = racent + 0.15/cos(deccent/57.3)
# 13.5 arcmin = 0.225 degree north for chips 1 and 5, etc...
	    if(nchip == 1 || nchip == 9) deccent = deccent - 0.225
	    if(nchip == 3 || nchip == 11) deccent = deccent - 0.075
	    if(nchip == 5 || nchip == 13) deccent = deccent + 0.075
	    if(nchip == 7 || nchip == 15) deccent = deccent + 0.225
	    offlag = 1
	    print('*new  RA    : ',racent)
	    print('*new  DEC   : ',deccent)
	    print(' ')
	    print('---->>>> USEFUL INFORMATION ***************************** ')
	    print(' ')
	    pwd
	    print('search_usno.pl --ra ',racent,' --dec ',deccent,' --xsize ',4096*scale,' --ysize',2048*scale,' --file usno.nosort' )
	    print('---->>>> ************************************************ ')
	    print(' ')
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
            print('search_usno.pl --ra ',racent,' --dec ',deccent,' --xsize 0.1 --ysize --0.1 --file usno.ccmap --num 1000')
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
	#if ( !access("usno.nosort") ) {
	#  racent,deccent,600,"usno.nosort");
        #}
        # !mv usno.ccmap usno.nosort
        #sort('usno.nosort',column=3,numer+, > 'usno.ccmap')
#
	i = 0
	intable='usno.ccmap'
	list=intable
        while(fscan(list,thisra,thisdec,thismag,thisid) != EOF)
	{
           print thisra, thisdec, thismag
	   i = i+1 ;
	   ra[i] = thisra ;
	   dec[i] = thisdec ;
	   mag[i] = thismag ;
	   id[i] = i ; 
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
	   ypix[j] = crpix2 + ((dec[j]-crval2)*cd1_1 - (ra[j] - crval1)*cd2_1)/(cd2_2*cd1_1 - cd2_1*cd1_2)
	   xpix[j] = crpix1 + ((ra[j]-crval1)*cd2_2 - (dec[j] - crval2)*cd1_2)/(cd2_2*cd1_1 - cd2_1*cd1_2)
	   if (xpix[j] < buf) goto skip;
	   if (xpix[j] > (xsize - buf) ) goto skip;
	   if (ypix[j] < buf) goto skip;
	   if (ypix[j] > (ysize - buf) ) goto skip;
	   if (mag[j] < minmag) goto skip;
	   if (mag[j] > maxmag) goto skip;
           ngood = ngood + 1
	   print(xpix[j],ypix[j],ra[j],dec[j],mag[j],id[j], >> tvfile)
	   #printf("%10.5f %10.5f %10.5f %10.5f \n",ra[j],dec[j],mag[j],id[j], >> intable )
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
        print("marking...",ngood)
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
          crpix1 = crpix1 + (x1-xx1)
          crpix2 = crpix2 + (y1-yy1)
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
        kdelete(centerfile)
        imcntr(inimg,x1,y1,cboxsize=box, >> centerfile)
        list = centerfile
        now = fscan(list,lab0,lab1,xcen,lab2,ycen)
        print("star centroid : ",xcen,ycen)
#       mark for reference to not accidentally re-chosen later
        pointfile=mktemp("tmp$pt")
        print(xcen, ycen,"\n", > pointfile)
        print("Mark started.. ",pointfile)
	tvmark(1,pointfile,mark='circle',radii='31',color=203)
        print("Mark finished.. ")
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

	ccmap(trialfile,solfile,solutions=trialsol,results="",xcol=1,ycol=2,lngcol=3,latcol=4,lngunits="degrees",latunits="degrees",lngrefunits="degrees",latrefunits="degrees",refpoint="coords",inter+)

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
	   #if (id[i] != thisid) print(" PANIC, file mismatch ",i)
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

        #delete(bfile,verify-)
	print(" ")
	print(" Wrote :")
	print(solfile)
	print(" ")
	print(" Done ")
 
end

	

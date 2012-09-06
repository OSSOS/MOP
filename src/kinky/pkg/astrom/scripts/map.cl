# bM)ultiple telescope (A)strometry and (P)hotometry

procedure map(solc,pin,pof,phd,put,pexp,pdate,pname,pcode,pzp,psee,pac,ppb)

#  This program uses the computed plate solution to translate
#  pixel coordinates to RA and DEC.  
#  AND it does aperture corrected photomery (whoa...!).
#  Assume the images are named a1,a2,a3, and that the file
#  hans.ast
#  is a list of the asteroids, 3 lines per asteroid, x y coords on a1,a2,a3
#
#  *IF* the images have not been processed with a 
#  BANADA (Built In Nice Automatic Detection Algorithm) which produces
#  all those files you should be using ccastrom instead.
#
# TELESCOPE CODES (internal to this script):
#  telesc : CFHT, ESO2, ESO3, UT-1, UT-2, Pal5
#  future : KPNO, NoOT


string solc {"", prompt="file with plate solution for all images?"}
int    nframes  {3, prompt="number of aligned files to measure astrom for"}
string pin  {"hans.ast", prompt="file with list of objects to measure?"}
string pof  {"Astrom", prompt="Output astrometry filename?"}
string phd      {"", prompt="Exposure date keyword  ? "}
string put      {"", prompt="UT-time START keyword  ? "}
string pexp     {"EXPTIME", prompt="Exposure length keyword? "}
string pdate    {"", prompt="Date in (yyyy mm dd)?"}
string pcode    {"809", prompt="Observatory code (3chars)? "}
string pname    {"", prompt="Object name? (7 char max)"}
int    pbox     {9,  prompt="box size for centroiding?"}
real   pzp      {25.0,prompt="Photometric ZP?"}
real    psee     {3,   prompt="APERTURE: approx FWHM, pix (INTEGER!)?"}
int    ppost    {256,   prompt="APERTURE: approx FWHM, pix (INTEGER!)?"}
real   pac      {0.3, prompt="Aperture correction from FWHM to 14 pix?"}
string ppb      {"R", prompt="Photometric passband (1 Char!)?"}

begin
    string now, coeffile, command, pointfile
    string imgname, centerfile, lab0, lab1, lab2, outfile
    real x1, y1, xcen, ycen, xkeep,ykeep
    real ra,dec,ut,exp,utcenter,dd,uttime
    string utkey,expkey,datestring,sdd,syy,blank,helput
    string outstring,oname,ustring,stra,stdec,datekey,logfile
    int wcs, box, yy, len, jn, nbuf, k, xsize,ysize
    real hh,mm,ss
    string shh,smm,sss,ocode
    string telesc,stringtel

    string this_image
    string oblist,prefix,schip,expname,filter,sflag,smag
    string hansreal,sxmin,symin,sxmax,symax,sj,imgsec,outastrom
    int    nread,nnew,newflag[100],i,j,xmin,ymin,xmax,ymax
    int    nchip, post
    real   xi[10,100], diffx,diffy
    real   yi[10,100]
    real   xc[3],yc[3]
    real   zp,ac,msky,mag
    int    isee,t_num, inum
    real   rsee

        print("  ")
        print(" ----- Multiple Telescope Astrometry/Photometry script ----- ")
        print(" I am assuming that : ")
        print(" 1. all DAOphot datapars are correct and ")
        print(" 2. that DAOPhot is loaded. ")
        print(" 3. you know the aperture correction/seeing. ")
        print(" ")
        print(" NOTE: BUG for decs from 0:00:00 to -1:00:00, to be fixed")
        print(" CONTINUE? (any character)")
	scan(command)

	coeffile=solc
	t_num=nframes
#-------------------------------
# Attempt to identify telescope
#
	ocode = "-99"

        imgets('a1.fits','TELESCOP')
        stringtel = imgets.value
        telesc = 'UNKN'
        if(stringtel == 'MPG/ESO-2.2') {
                telesc = 'ESO2'
		ocode = '809'
	        helput = 'UTC'
        }
        if(stringtel == 'ESO-3P6') {
                telesc = 'ESO3'
		ocode = '809'
	        helput = 'UT'
        }
        if(stringtel == 'ESO-VLT-U1' || stringtel == 'ESO-VLT-U2') {
                telesc = 'VLT'
		ocode = '309'
	        helput = 'UT'
        }
        if(stringtel == 'ESO-VLT-U1') {
                telesc = 'UT-1'
		ocode = '309'
	        helput = 'UT'
        }
        if(stringtel == 'CFHT 3.6m') {
                telesc = 'CFHT'
		ocode = '568'
	        helput = 'UTC-OBS'
        }
        if(stringtel == '1.2M') {
                telesc = 'FLW1'
		ocode = '696'
	        helput = 'UT'
        }
        if(stringtel == 'KPNO 4.0 meter telescope') {
                telesc = 'KP4m'
		ocode = '695'
	        helput = 'TIME-OBS'
        }
	if(stringtel == 'CTIO 4.0 meter telescope') {
		telesc = 'CTIO4m'
		ocode = '807'
		helput = 'TIME-OBS'
	}
# Palomar does not currently have TELESCOP keyword, but...
        if(telesc == 'UNKN') {
           imgets('a1.fits','DETECTOR')
           stringtel = imgets.value
           if(stringtel == 'TEK3') {
		telesc = 'Pal5'
		ocode = '675'
	        helput = 'UT-START'
           }
           ;
        }
        print(' TELESCOPE : ',telesc)
        print(' Usual UT keyword : ',helput)

        if (real(ocode) > -1) goto gparams
	print(" **************************************************** ")
	print(" * Obs. codes: 675(Pal)  568(M.Kea)    695(KPNO)    * ")
	print(" *             807(CTIO) 809(La Silla) 950(La Palma)* ")
	print(" **************************************************** ")
	ocode = pcode

   gparams:
  	box = pbox
	utkey = put
	expkey = pexp
	pointfile=mktemp("tmp$pt")
	datekey = phd
        outastrom= pof
#--------------------------------------------
#--------------------------------------------
        imgets('a1','i_naxis1')
        xsize = int(imgets.value)
        imgets('a1','i_naxis2')
        ysize = int(imgets.value)
        print('*** Auto-detected size of chip : ',xsize,ysize)
        print('------PHOTOMETRIC PARAMETERS------')
	zp = pzp
	photpars.zmag = zp
	rsee = psee
	isee = int(rsee)
	photpars.apertures = rsee
	ac = pac
	filter = ppb

	nread = 0
	nnew = 0
        print('------ INPUT FILE ------')
	hansreal = pin
	list = hansreal
        while(fscan(list,x1,y1) != EOF) {
	     nread = nread + 1
	     print(" nread : ", nread)
             xi[1,nread] = x1
             yi[1,nread] = y1
	     for ( i=2; i<=t_num; i+=1) {
	         if ( fscan(list,x1,y1)==EOF) {
		     error(1,'File length and image list mismatch')
	         }
                 xi[i,nread]=x1
	         yi[i,nread]=y1
             }
	     newflag[nread]=1
	     nnew = nnew + 1
	     i = 1
	     while( i < nread ) {
	        diffx = x[1,nread] - xi[1,i]
	        diffy = y[1,nread] - yi[1,i]
		if( (diffx*diffx + diffy*diffy) < 9.0 ) {
                         newflag[nread]=0
                         nnew = nnew - 1
                }
                i=i+1
             }
        }
        print( " " )
        print( "-->Read ",nread," objects, of which *",nnew,"* are distinct.")
        print( " " )
	
#=========================================================================
        print("------------- looping through all objects  -----------")
	i=0
       newimg:
        i=i+1
	if( i > nread) goto finish
	if(newflag[i] == 0) goto newimg
	print('************ Displaying object ',i)
        
#          ------------------- compute cut section -------------------
	   post = ppost
           xmin=xsize
	   xmax=0
	   ymin=ysize
           ymax=0
	   for (inum=1; inum<=t_num;inum+=1){
	      if ( xmin>xi[inum,i] ) xmin=xi[inum,i]

              if ( xmax<xi[inum,i] ) xmax=xi[inum,i]

	      if ( ymin>yi[inum,i] ) ymin=yi[inum,i]

              if ( ymax<yi[inum,i] ) ymax=yi[inum,i]

	      ;
	      print (inum,xmin,xmax,ymin,ymax)
	   }

	   xmin=xmin-(post/2.0)
	   if (xmin < 1) xmin = 1
	   if ( (xmin+post) > xsize ) xmin = xsize - post

	   xmax = xmax+(post/2.0)
	   if ( xmax < xmin+post ) xmax=xmin+post 
           if ( xmax > xsize ) xmax=xsize

           sxmin = xmin
           sxmax = xmax

	   ymin = ymin - (post/2)
	   if (ymin < 1) ymin = 1
	   if ( (ymin+post) > ysize ) ymin = ysize - post

	   ymax = ymax+(post/2.0)
	   if ( ymax < ymin+post ) ymax = ymin+post
           if ( ymax > ysize ) ymax=ysize

           symin = ymin
           symax = ymax

           imgsec = '['//sxmin//':'//sxmax//','//symin//':'//symax//']'
#       ------------------- compute object centroids -------------------
	for (inum=1;inum<=t_num;inum+=1) {
           this_image='a'//inum
           centerfile = this_image//'.cen'
           imcntr(this_image//'.fits',xi[inum,i],yi[inum,i],cboxsize=box, >> centerfile)
	   list = centerfile
           now = fscan(list,lab0,lab1,x1,lab2,y1)
	   xc[inum] = x1
           yc[inum] = y1
           delete(centerfile, verify-)
        }
#       ------------------- Display all 3 for user to see -------------------
	for(j=1;j<=t_num;j+=1) {
         sj = j
	 imgname='a'//sj//imgsec
	 display(imgname,j)
	 pointfile=mktemp("tmp$pt")
         print(xc[j], yc[j], > pointfile)
	 tvmark(j,pointfile,mark='circle',radii='18',color=204)
         delete(pointfile, verify-)
        }
#      ------------------- Now loop through 3 and measure -------------------
       for(k=1;k<=t_num;k+=1) 
       {

	pnts:
	 print("  - - - - - - - - - - - -      Looking at image *** ",k,"***")
         sj = k
         imgname='a'//sj//imgsec
         display(imgname,k)
         pointfile=mktemp("tmp$pt")
         print(xc[k], yc[k], > pointfile)
#         tvmark(k,pointfile,mark='circle',radii='14',color=204)
#                    ------- mark inner/outer sky annulus ----------
         tvmark(k,pointfile,mark='circle',radii=(9*isee),color=205)
         tvmark(k,pointfile,mark='circle',radii=(10*isee),color=205)
         delete(pointfile, verify-)
         print("*******************************************************")
         print("* 'q'=quit, 'h'=here, 'r'=recentroid, 'o'=centroid OK *")
         print("*******************************************************")
         print("* >>IMCURSOR<< *")
         print("****************")

	 now= fscan(imcur, x1, y1, wcs, command)
	 xkeep = x1
	 ykeep = y1
	 if (command == "q") bye
#	 if (command == "n") goto newimg
         print(x1, y1, > pointfile)
	 if (command == "h") 
	 {
	   print(" \n *** NON-CENTROIDED ***********************************")
	   outfile=mktemp("tmp$out")
           cctran(pointfile,outfile,coeffile,"full.sol",forward+,xcol=1,ycol=2,lngformat="%12.2h",latformat="%11.1h")
           tvmark(k, pointfile, mark="circle",radii=isee, label=yes, color=206)
           delete(pointfile, verify-)
	   print(" ******************************************************")
           goto mpc
         }
	 if (command == "r")
	 {
#          Don't need the coordinates written in pointfile, have x1,y1
           delete(pointfile, verify-)

           imgname = "a"//k
	   print(" ")
	   print(" imgname: ",imgname)
	   centerfile = imgname//".cen"
	   imcntr(imgname,x1,y1,cboxsize=box, >> centerfile)
#	   print(x1,y1)
	   list = centerfile
	   now = fscan(list,lab0,lab1,xcen,lab2,ycen)
	   xkeep = xcen
	   ykeep = ycen
           delete(centerfile, verify-)
#	   print("xcen, ycen  ",xcen,ycen)
	   pointfile=mktemp("tmp$pt")
           print(xcen, ycen, > pointfile)
	   outfile=mktemp("tmp$out")
           cctran(pointfile,outfile,coeffile,"full.sol",forward+,xcol=1,ycol=2,lngformat="%12.2h",latformat="%11.1h")
           tvmark(k, pointfile, mark="circle",radii=isee, label=yes, color=207)
           delete(pointfile, verify-)
           goto mpc
         }
	 if (command == "o")
	 {
	   pointfile=mktemp("tmp$pt")
           print(xc[k], yc[k], > pointfile)
	   outfile=mktemp("tmp$out")
           cctran(pointfile,outfile,coeffile,"full.sol",forward+,xcol=1,ycol=2,lngformat="%12.2h",latformat="%11.1h")
           tvmark(k, pointfile, mark="circle",radii=isee, label=yes, color=207)
           delete(pointfile, verify-)
	   xkeep = xc[k]
	   ykeep = yc[k]
         }
         else
#        INVALID INPUT!
         {
	   goto pnts
         }
#-----------------------------------------------------------------------
#  ALL OF THE FOLLOWING MESS IS JUST FORMATTING TO MPC FORMAT...!

  mpc:

#   Get RA/DEC from cctran temp file
        list = outfile
	now = fscan(list,ra,stdec)
#        print( ra," ",stdec )
        delete(outfile, verify-)
#   Get UT from image header, in hours.  For some telescopes its given in sec.
        imgets(imgname,utkey)
	ut = real(imgets.value)
#	print(utkey,"  ",ut)
        if(telesc=='ESO2') ut = ut/3600.00 
#	       Following only if have not applied hierarch28
#        if(telesc=='ESO2'||telesc=='UT-1') ut = ut/3600.00 
        if(ut >23.0 || ut <1.0) print("****** WARNING: Near day boundary !")
#   Get exposure length from image header, in seconds. 
        imgets(imgname,expkey)
 	exp = real(imgets.value)
#	print(expkey,"  ",exp)
	print(" IMAGE OBTAINED : ",ut," UT  for ",exp," sec.")
        utcenter = (   ut + (exp/(3600.0))/2.0   )/24.0
	if (utcenter < 0.0) print ("************** PANIC, UTCENTER < 0 ")
	if (utcenter > 1.0) print ("************** PANIC, UTCENTER > 1 ")
#  utcenter now has UT fraction of day  of center.  
#  Assemble UTday string for MPC format
#  Get date as string in yyyy mm dd format
	imgets(imgname,datekey)
	print("*** Keyword ",datekey," = ",imgets.value)
	datestring = pdate
        sdd = substr(datestring,9,10)
     	dd = real(sdd)
	uttime = dd + utcenter
#	print(datestring," ",uttime)

        blank= " "
#  PRODUCE OBJECT NAME.  THIS WAS ALL FOR CFHT AUTO GENERATION.  FOR
#  now just accept the name entered on command line. 
#	if(k == 1) {
#              imgets('a1','FILENAME')
#              expname = imgets.value
#              len = stridx("j",expname)
#	       oname = prefix // substr(expname,len+1,len+3)
#              imgets('a1','IMAGEID')
#              schip = imgets.value
#              nchip = int(imgets.value)
#	      oname = oname//"c"
#	      if (nchip < 10) oname = oname//"0"
#	      oname = oname//str(nchip)
#	      oname = oname//"n"
#	      oname = oname//str(i)
# 
#              print("OBJECT NAME: ",oname)
#        }

        prefix = pname
	oname = "     "//prefix

	len = strlen(oname)
	outstring = oname
# pad to 15 characters for MPC format
	for(j=len+1;j<=14;j+=1) {
            outstring = outstring//blank
        }
        outstring = outstring//"C"
# Now add on date, including day.
        outstring = outstring//substr(datestring,1,8)
	if (dd <9.999999999999) outstring = outstring//"0"
#	print(outstring)
        ustring = str(uttime)
#        print("ustring ",ustring
	len = stridx(".",ustring)
	outstring = outstring//substr(ustring,1,len-1)
#	print(outstring)
# Note that the following truncates rather than rounds the last sig. digit
	outstring = outstring//substr(ustring,len,len+5)
#	print(outstring)
	outstring = outstring//" "
# Build RA
	ra = ra/15.000000000
	stra = str(ra)
	len = strlen(stra)
	j = stridx(".",stra)
	shh = substr(stra,1,j-1)
	hh = real(shh)
	if (hh < 9.99999999999) outstring = outstring//"0"
	outstring = outstring//shh
	outstring = outstring//" "
#	print(outstring)
	ra = (ra - hh)*60.0
        stra = str(ra)
	j = stridx(".",stra)
	smm = substr(stra,1,j-1)
	mm = real(smm)
# 	print(outstring)
#	print("smm,mm ",smm,"  ",mm)
	if (mm < 9.99999999999) outstring = outstring//"0"
	outstring = outstring//smm
	outstring = outstring//" "
#	print(outstring)
	ra = (ra-mm)*60.0
        stra = str(ra)
	j = stridx(".",stra)
	if (j == 2) outstring = outstring//"0"
	outstring = outstring//substr(stra,1,j+2)
	outstring = outstring//" "
#	print(outstring)
# Build Dec
	print("stdec:",stdec)
	print("substr:",substr(stdec,1,1))
	j = stridx(":",stdec)
	hh = real( substr(stdec,1,j-1) )
# print(' hh ',hh)
# ADD SIGN to DEC. FAILS if dec IS +/-00
	if (hh > 0) outstring = outstring//"+"
	if (hh < 0) outstring = outstring//"-"
# following probably not necessary
	if (hh < 9.9999999 && hh > -9.999999) print(">>>>CHECK DECLINATION !!")
## FOLLOWING TEST RECENTLY ADDED.
	if (hh < 0.0000001 && hh > -0.000001) {
		print(">>>>CHECK DECLINATION !!")
		if(substr(stdec,1,1) == "-") {
			outstring = outstring//"-"
		} else {
			outstring = outstring//"+"
		}
		;
	}
	;
	if (hh < 0.000000001 && hh > -0.00000001) outstring = outstring//"0"
	if (hh < 9.999999999 && hh > -9.99999999) outstring = outstring//"0"
	if (hh > 0) outstring = outstring//substr(stdec,1,j-1)
	if (hh < 0) outstring = outstring//substr(stdec,2,j-1)

	outstring = outstring//" "
	outstring = outstring//substr(stdec,j+1,j+2)
	outstring = outstring//" "
	outstring = outstring//substr(stdec,j+4,j+7)

#       ------------------- Do photometry here ----------------------
# Write into .coo.1 file for photometry
	fitskypars.annulus = (4*rsee)
	fitskypars.dannulus = (3*rsee)
	logfile=substr(imgname,1,2)//".coo.1"
	delete(logfile,verify-)
	print(xkeep,ykeep," ",outstring,>>logfile)

        print(" ########## PHOTOMETERY ############# ")
        print(" Using ",rsee," pixel aperture (drawn) with shown bounds ")

        centerfile = 'phot.tmp'
#  	    phot(substr(imgname,1,2),'default','default',verify-)
  	phot(substr(imgname,1,2),'default','default',verify-,verbose+, >> centerfile)
	delete(logfile,verify-)
	list = centerfile
        now = fscan(list,lab0,x1,y1,msky,mag,sflag)
	delete(centerfile,verify-)
	logfile=substr(imgname,1,2)//".mag.1"
	delete(logfile,verify-)

        print("*************************************")
#        print("* Phot done. Reporting:             ")
        print("* Flag                : ",sflag)
        print("* sky                 : ",msky)
	if (mag < 8.0 || mag > 30.0) goto photfail
	if (mag == INDEF) goto photfail
        print("* mag                 : ",mag)
        print("* ap.-corr. magnitude : ", (mag-ac),"<------")
        print("* filter              : ",filter,"   ")
        print("*************************************")
        print("* Accept (a) or (r) for MPC report?  ")
        print("*************************************")
        print("* >>IMCURSOR<< *")
        print("****************")
	goto photqry
  photfail:
        print("*************************************")
        print("* PHOTOMETRY FAILED *")
        print("* Reject for MPC    *")
        print("*************************************")
        print("* Accept (a) or (r) for MPC report?  ")
        print("*************************************")
        print("* >>IMCURSOR<< *")
        print("****************")

  photqry:
	now= fscan(imcur, x1, y1, wcs, command)
	if (command == "r") {
#           -- Pad blank spaces --
	    for(j=1;j<=22;j+=1) {
              outstring = outstring//blank
            }
        }
	else if (command == "a") {
#           -- Pad blank spaces --
	    for(j=1;j<=10;j+=1) {
              outstring = outstring//blank
            }
#           -- Insert mag --
	    mag = mag - ac
#                   fix rounding, because substr() truncates
	    if ( ( 10.0*mag-int(10.0*mag) ) > 0.500 ) {mag=mag+0.050}
	    smag = str(mag)
	    outstring = outstring//substr(smag,1,4)
#           -- add filter --
            outstring = outstring//blank
            outstring = outstring//filter
#           -- Pad blank spaces --
	    for(j=1;j<=6;j+=1) {
            outstring = outstring//blank
            }
        }
	else {
        goto photqry
        }
        ;
#       ------------------- End photometry --------------------------

# Add observatory code
	outstring = outstring//substr(ocode,1,3)
	print(outstring)

	print(outstring,>>outastrom)

#	goto pnts
# brace loops to next frame of this object
      }

#=========================================================================
# next real object to measure
      goto newimg

      finish:
      print(" ")
      print("       Done. All objects written to ------------>  ",outastrom)
      print(" ")
      cat(outastrom)
end

# (MPC)-producing (PHOT)ometry script intended for CFHT pipeline

procedure mpcphot(pin)

#  INPUT: 
#    On command line: 
#	name of file with candidates (image_name.cands.astrom)
#    ASSUME DAOPHOT LOADED
#    Also requires input files:
#	  aper.corr    (with   inner outer correction   for stars, 3 frames)
#    zeropoint.used    (with zeropoint of CCD)
#
#  MODIFICATIONS
#
#  9 nov 2001 : BG added algorithm to register postage stamps. Needs work
# 10 nov 2001 : BG modified so that user sees UNPLANTED images

string pin  {"FILE.cands.astrom", prompt="file with header and x/y/RA/DEC?"}
##string pof  {"MPC", prompt="Output MPC astrometry filename?"}
##string phd      {"", prompt="Exposure date keyword  ? "}
##string put      {"", prompt="UT-time START keyword  ? "}
##string pexp     {"EXPTIME", prompt="Exposure length keyword? "}
##string pdate    {"", prompt="Date in (yyyy mm dd)?"}
##string pcode    {"809", prompt="Observatory code (3chars)? "}
##string pname    {"", prompt="Object name? (10 char max)"}
##int    pbox     {9,  prompt="box size for centroiding?"}
##real   pzp      {25.0,prompt="Photometric ZP?"}
##int    psee     {3,   prompt="APERTURE: approx FWHM, pix (INTEGER!)?"}
##real   pac      {0.3, prompt="Aperture correction from FWHM to 14 pix?"}
int    ppost    {256,   prompt="postage stamp size"}
string ppb      {"R", prompt="Photometric passband (1 Char!)"}

begin
    string now, coeffile, command, pointfile, realfile
    string astfile, dummy, junk, instidstr
    string imgname[3]
    string datestr[3], savestr[3], photstr[3]
    string imgread, centerfile, lab0, lab1, lab2, outfile
    real x1, y1, xa1, ya1, xcen, ycen, xkeep,ykeep
    real ra,dec,ut,exp,utcenter,dd,uttime
    real raall[100,4],decall[100,4]
    real xof[3], yof[3]
    string utkey,expkey,datestring,sdd,syy,blank
    string outstring,oname,ustring,stra,stdec,datekey,logfile,tmpstr
    int    wcs, box, yy, len, jn, nbuf, k, xsize,ysize
    int     id, chipid, whereami, posk
    string  year, month, day, instid
    real    maxcounts, ramos, decmos
    real    pixscale,xoff,yoff
    real    hh,mm,ss
    string shh,smm,sss,ocode
    string telesc,stringtel
    real   ratemin,ratemax,conecent,conewidth

    string zpfile,prefix,schip,expname,filter,sflag,smag
    string hansreal,sxmin,symin,sxmax,symax,sj,imgsec,outastrom
    int    nread,nnew,newflag[100],i,j,xmin,ymin,xmax,ymax
    int    nchip, post, ntaken
    real   xi1[100],xi2[100],xi3[100], diffx,diffy
    real   yi1[100],yi2[100],yi3[100]
    real   xc[3],yc[3]
    real   zp,ac[3],acr,msky,mag[3],tmag
    int    ac1r,ac2r,acin[3],acout[3]
    real   isee[3],iseer,th
#--------------------------------------------

    realfile=mktemp("tmp$pt")
    ntaken = 1

        print("  ")
        print(" ----- PIPELINE Photometry script ----- ")
        print(" I am assuming that : ")
        print(" 1. all DAOphot datapars are correct and ")
        print(" 2. that DAOPhot is loaded. ")
        print(" 3. the aper.corr file exists. ")

#-------------------------------
# Attempt to identify telescope DELETED FROM MPCPHOT. ONLY CFHT FOR NOW.
#
        print('------------------------> CURRENTLY WORKS ONLY FOR CFHT ')
        print(" ")
        telesc = 'CFHT'
	ocode = '568'

#-------------------------------
   gparams:
	zpfile = "zeropoint.used"
        if ( !access(zpfile) ) {
	  print(" ")
          error(1,"File   "//zpfile//"   doesn't exist. STOP. \n");
          ;
        }
	list = "zeropoint.used"
	dummy=fscan(list,zp)

	zpfile = "aper.corr"
        if ( !access(zpfile) ) {
	  print(" ")
          error(1,"File   "//zpfile//"   doesn't exist. STOP. \n");
          ;
        }
	list = "aper.corr"
	for(j=1; j<=3; j+=1){
	   dummy=fscan(list,ac1r,ac2r,acr)
	   acin[j] = ac1r
	   acout[j] = ac2r
	   ac[j] = acr
	}

        print('PHOTOMETRIC PARAMETERS READ.  Using zeropoint: ',zp)
	print(" ")
#       print(" >>> zp,acin,acout,ac : ", zp,acin,acout,ac)

	photpars.zmag = zp
	datapars.ccdread = ''
	datapars.gain = "GAIN"
	datapars.readnoi = 7.0
	datapars.filter = ''
	datapars.obstime = "UTC-OBS"
	filter = ppb
#  	box = pbox


!       ls -1 *.astrom
	print(" ")
	astfile = pin
### HARDWIRE         outastrom= pof
	zpfile = "MPC"
        if ( access(zpfile) ) {
	  print(" ")
          error(1," File   "//zpfile//"   ALREADY EXISTS.    STOP. \n");
          ;
        }
        outastrom= "MPC"
	list = astfile
	for(j=1;j<=3;j+=1) {
	  dummy=fscan(list,junk,imgread)
	     imgname[j] = imgread
	     print(" imgname[j] : ",j, imgname[j])
	     len = strlen(imgname[j])
             posk = stridx("k",imgname[j])
             imgname[j] = substr(imgname[j],posk+1,len)
	     print(" imgname[j] ,posk : ",j, imgname[j], posk)
        }
    dummy=fscan(list,junk,year,month,day,exp,th,iseer,maxcounts,ramos,decmos,id)
	isee[1] = iseer
#	     print(" year month day id: ", year,month,day,id)
	datestr[1] = year//" "//month//" "//day
#	     print(" datestr1 : ", datestr[1])
	dummy=fscan(list,junk,pixscale,chipid,xoff,yoff,xsize,ysize,instidstr)
    dummy=fscan(list,junk,year,month,day,exp,th,iseer,maxcounts,ramos,decmos,id)
	isee[2] = iseer
	datestr[2] = year//" "//month//" "//day
#	     print(" datestr2 : ", datestr[2])
	dummy=fscan(list,junk,pixscale,chipid,xoff,yoff,xsize,ysize,instidstr)
    dummy=fscan(list,junk,year,month,day,exp,th,iseer,maxcounts,ramos,decmos,id)
	isee[3] = iseer
	datestr[3] = year//" "//month//" "//day
#	     print(" datestr3 : ", datestr[3])
	dummy=fscan(list,junk,pixscale,chipid,xoff,yoff,xsize,ysize,instidstr)

	dummy=fscan(list,junk,ratemin,ratemax,conecent,conewidth)

#------------ NOW READ ENTIRE OBJECT LIST , checks for duplicates ------------
	nread = 0
	nnew = 0
        print('------ INPUT FILE READ')
#       First line should be blank line.
        while(fscan(list,junk) != EOF) {
	     nread = nread + 1
#	     print(" nread : ", nread)
             if (fscan(list,x1,y1,xa1,ya1,ra,dec) != EOF) {
               xi1[nread] = x1
               yi1[nread] = y1
	       raall[nread,1] = ra
	       decall[nread,1] = dec
             }
             if (nread == 1) {
                xof[1] = 0.0
                yof[1] = 0.0
             }

#            ------ Check we haven't already seen this one ; REDUNDANT ? -----
	     newflag[nread]=1
	     nnew = nnew + 1
	     i = 1
	     while( i < nread ) {
	        diffx = x1 - xi1[i]
	        diffy = y1 - yi1[i]
#                            ---- tolerance is 3-pix radius ---
		if( (diffx*diffx + diffy*diffy) < 9.0 ) {
                         newflag[nread]=0
                         nnew = nnew - 1
                }
                i=i+1
             }

             if (fscan(list,x1,y1,xa1,ya1,ra,dec) != EOF) {
               xi2[nread] = x1
               yi2[nread] = y1
	       raall[nread,2] = ra
	       decall[nread,2] = dec
             }
             if (nread == 1) {
                xof[2] = x1 - xa1
                yof[2] = y1 - ya1
             }
             if (fscan(list,x1,y1,xa1,ya1,ra,dec) != EOF) {
               xi3[nread] = x1
               yi3[nread] = y1
	       raall[nread,3] = ra
	       decall[nread,3] = dec
             }
             if (nread == 1) {
                xof[3] = x1 - xa1
                yof[3] = y1 - ya1
             }

        }
        print( "-->Read ",nread," objects, of which *",nnew,"* are distinct.")
        print( " " )
        print( "OFFSETS : " )
        print(xof[1],yof[1],xof[2],yof[2],xof[3],yof[3])


#=========================================================================
      print("------------- looping through all objects  -----------")
      i=0

   newimg:
        i=i+1
	if( i > nread) goto finish
	if(newflag[i] == 0) goto newimg
	print('************ Displaying object ',i)
        
#  ------------------- Display all 3 for user to see -------------------
        post = ppost
        for(j=1;j<=3;j+=1) {

#         ----------------- compute cut section, object i -------------------
# The following algorithm has troubles near the chip edges. To be improved.

          xmin = xi1[i]-(post/2) + xof[j]
          if (xmin < 1) xmin = 1 + xof[j]
          if ( (xmin+post) > xsize ) xmin = xsize - post + xof[j]
          xmax = xmin + post
# Following occurs if the offset is negative
          if (xmin < 1) {
                xmin = 1 
                xmax = post 
          }
# Following occurs if the offset is postive
          if (xmax > xsize) {
                xmin = xsize - post 
                xmax = xsize
          }
          sxmin = xmin
          sxmax = xmax

          ymin = yi1[i]-(post/2) + yof[j]
          if (ymin < 1) ymin = 1 + yof[j]
          if ( (ymin+post) > ysize ) ymin = ysize - post + yof[j]
          ymax = ymin + post 
# Following occurs if the offset is negative
          if (ymin < 1) {
                ymin = 1 
                ymax = post 
          }
# Following occurs if the offset is postive
          if (ymax > ysize) {
                ymin = ysize - post 
                ymax = ysize
          }
          symin = ymin
          symax = ymax
          imgsec = '['//sxmin//':'//sxmax//','//symin//':'//symax//']'

          print( "imgsec : ",imgsec )

          imgread=imgname[j]//imgsec
          display(imgread,j)

        }

# --------------------------------------------------
	pointfile=mktemp("tmp$pt")
        print(xi1[i], yi1[i], > pointfile)
        tvmark(1,pointfile,mark='circle',radii=acout[1],color=205)
        delete(pointfile, verify-)
	pointfile=mktemp("tmp$pt")
        print(xi2[i], yi2[i], > pointfile)
        tvmark(2,pointfile,mark='circle',radii=acout[2],color=205)
        delete(pointfile, verify-)
	pointfile=mktemp("tmp$pt")
        print(xi3[i], yi3[i], > pointfile)
        tvmark(3,pointfile,mark='circle',radii=acout[3],color=205)
        delete(pointfile, verify-)

        if(ntaken > 1) {
            tvmark(1,realfile,inter-,mark="rectangle",lengths="5.0",color=205)
        }

#  ------------ Now loop through 3 sub-images and measure astrometry ---------
       for(k=1;k<=3;k+=1) 
       {
#       -------- Get RA/DEC for object 'i' from list in memory -----
	ra  = raall[i,k]
	dec = decall[i,k]
        blank= " "

# TEMPORARY BLANK NAME, OBJECT NAMED AFTER ACCEPTANCE OF ASTROMETRY
        oname ="       "
	len = strlen(oname)
	outstring = oname
# ---------- now pad name to 15 characters for MPC format
	for(j=len+1;j<=14;j+=1) {
            outstring = outstring//blank
        }
        outstring = outstring//"C"
# ---------- Now add on date using the data in the .cands.comb file
        outstring = outstring//substr(datestr[k],1,16)//" "
# ---------- Build RA
	whereami = 1
	goto buildra
     mainra:
# ---------- Build DEC
	whereami = 1
	goto builddec
     maindec:

# --------- Pad blank spaces , before photometric entry --
	for(j=1;j<=9;j+=1) {
           outstring = outstring//blank
        }
# --------- Now save outstring into savestr[] and pad astrometry line with COD
	savestr[k] = outstring
	tmpstr = outstring//"            "//substr(ocode,1,3)
	print(tmpstr)
# NEXT BRACE LOOPS ON image k, object i, to complete all the astrometric lines
      } 

# ================= Now confirm object and astrometric solution =============
      astprompt:
         print("****************")
         print("*  ASTROMETRY  *")
         print("*********************************************************")
         print("* 'd'= flag as discovery,             'A'=abort object  *")
         print("* 'o'= OK , proceed to naming object                    *")
         print("* 'c'=centroid an image ,             'Q'=Quit program  *")
         print("*********************************************************")
         print("* >>IMCURSOR<< *")
         print("****************")
         now= fscan(imcur, x1, y1, wcs, command)
         xkeep = x1
         ykeep = y1
         if (command == "Q") bye
         if (command == "d") 
         {
	   j = strlen(savestr[1])
	   savestr[1] = substr(savestr[1],1,12)//"*"//substr(savestr[1],14,j)
	   tmpstr = savestr[1]//"            "//substr(ocode,1,3)
	   print(tmpstr)
	   tmpstr = savestr[2]//"            "//substr(ocode,1,3)
	   print(tmpstr)
	   tmpstr = savestr[3]//"            "//substr(ocode,1,3)
	   print(tmpstr)
	  }
	  if (command == "o") {
#             PRODUCE OBJECT NAME.  THIS IS FOR CFHT AUTO GENERATION.  
#                                   But can overrule.
              imgets(imgname[1],'FILENAME')
              expname = imgets.value
	      j = strlen(expname)
              len = stridx("o",expname)
	      if(len==0 || len > j) print(" *****>>>>>> PANIC ON NAME. STOP!")
	      oname = substr(expname,1,len-1)//"c"
              imgets(imgname[1],'IMAGEID')
              schip = imgets.value
              nchip = int(imgets.value)
 	      if (nchip < 10) oname = oname//str(schip)
 	      if (nchip == 10) oname = oname//"A"
 	      if (nchip == 11) oname = oname//"B"
 	      if (nnew > 1) oname = oname//str(ntaken)
  	      print(" CFHT Pipe suggested name : PASTE IN or CHANGE")
	      print(" ")
  	      print(oname)
	      print(" ")
      nameqry:
	      prefix="?"
	      print(" Enter object name (2-11 characters)")
              scan(prefix)
	      len = strlen(prefix)
	      if (len < 2) goto nameqry
	      if (len > 11) goto nameqry
	      if( len > 7) {
	        oname = " "//prefix 
	      } else {
	        oname = "     "//prefix 
	      }
	      ;
	      len = strlen(oname)
	      for(j=len+1;j<=12;j+=1) {
                  oname = oname//blank
              }
	      j=strlen(savestr[1])
	      savestr[1] = oname//substr(savestr[1],13,j)
	      j=strlen(savestr[2])
	      savestr[2] = oname//substr(savestr[2],13,j)
	      j=strlen(savestr[3])
	      savestr[3] = oname//substr(savestr[3],13,j)
              goto photprompt
          }
	  if (command == "A") goto newimg
	  if (command == "c")
	  {
		print("Which image (1-3) do you wish to recentroid?")
	        scan(k)
	        imgread=imgname[k]//imgsec
		disp(imgread,k)
                print("********************************************")
	        print("* MARK IMAGE CENTROID MANUALLY WITH CURSOR *")
	        print("* E =error, return to oringal astrometry   *")
	        print("* ANY OTHER KEY to choose manual centroid  *")
                print("********************************************")
                print("* >>IMCURSOR<< *")
                print("****************")
	        now= fscan(imcur, x1, y1, wcs, command)
	        if (command == "E") goto photprompt
		pointfile="pipephot.tmpxy"
	        tmpstr = imgname[k]//".platesol"
		print(tmpstr, > pointfile)
		print(x1,y1, >> pointfile)
           tvmark(k,pointfile,mark="circle",radii=2,label=yes,color=206)
# CALL HANS SINGLE-PASS Fortran CODE HERE.  Delete both temp files after use.
		print("    Using   hansastone   for (x,y)-->(ra,dec)")
                ! hansastone
		delete(pointfile)
		pointfile="pipephot.tmpradec"
		list = pointfile
        	now = fscan(list,ra,dec)
#	           print(' RA : ',ra,'      DEC: ',dec)
		delete(pointfile)
# NEED TO REBUILD outstring here. First 32 characters are name+date.
		outstring = substr(savestr[k],1,32)
		whereami=2
		goto buildra
	     recentra:
		goto builddec
	     recentdec:
#               Pad back on blanks for photometry and COD.
	        for(j=1;j<=9;j+=1) {
                      outstring = outstring//blank
                }
	        savestr[k] = outstring
	        tmpstr = outstring//"            "//substr(ocode,1,3)
	        print(savestr[1]//"            "//substr(ocode,1,3))
	        print(savestr[2]//"            "//substr(ocode,1,3))
	        print(savestr[3]//"            "//substr(ocode,1,3))
	  }
	  goto astprompt

#=========================================================================
 photprompt:
#      OBJECT ACCEPTED, SO INCREMENT COUNTER used for naming accepted TNOs
       ntaken = ntaken + 1
       print("%8.2f%8.2\n", xi1[i], yi1[i], >> realfile)


       print(" ########## PHOTOMETERY ############# ")
       print(" Using apertures (drawn) with shown sky-annulus bounds ")

#      ---------- compute object centroids, NOTE: For photom only -----------
       centerfile = 'a1.cen'
	box = isee[1] * 2.0
	if (box < 5.0) box = 5.0
   print(" isee box : ",isee[1], box)
       imcntr(imgname[1],xi1[i],yi1[i],cboxsize=box, >> centerfile)
        list = centerfile
        now = fscan(list,lab0,lab1,x1,lab2,y1)
        xc[1] = x1
        yc[1] = y1
       delete(centerfile, verify-)
       centerfile = 'a2.cen'
   print(" isee box : ",isee[1], box)
	box = isee[2] * 2.0
	if (box < 5.0) box = 5.0
       imcntr(imgname[2],xi2[i],yi2[i],cboxsize=box, >> centerfile)
	list = centerfile
        now = fscan(list,lab0,lab1,x1,lab2,y1)
	xc[2] = x1
        yc[2] = y1
       delete(centerfile, verify-)
       centerfile = 'a3.cen'
	box = isee[3] * 2.0
	if (box < 5.0) box = 5.0
   print(" isee box : ",isee[1], box)
       imcntr(imgname[3],xi3[i],yi3[i],cboxsize=box, >> centerfile)
	list = centerfile
        now = fscan(list,lab0,lab1,x1,lab2,y1)
	xc[3] = x1
        yc[3] = y1
       delete(centerfile, verify-)

#	print(' xc1 yc1 ',xc[1],yc[1])
#	print(' xc2 yc2 ',xc[2],yc[2])
#	print(' xc3 yc3 ',xc[3],yc[3])

#       ------------------- Do photometry here ----------------------
# Write into .coo.1 file for photometry
       for(k=1;k<=3;k+=1)
       {
	fitskypars.annulus = (acout[k]+1)

# WARNING: FOLLOWING CLUDGE TAKES CARE OF SEEING-PROBLEM IN MATT's CODE
# SHOULD BE ELIMINATED
	if (isee[k] < 2.0) {
		isee[k] = 2.0
		print("   WARNING: Reset annulus width ")
	}
	fitskypars.dannulus = (3*isee[k])
	photpars.apertures = acin[k]
        pointfile=mktemp("tmp$pt")
        print(xc[k], yc[k], > pointfile)
        tvmark(k,pointfile,mark='circle',radii=acin[k],color=205)
        tvmark(k,pointfile,mark='circle',radii=(acout[k]+1),color=204)
        tvmark(k,pointfile,mark='circle',radii=(acout[k]+3*isee[k]+1),color=204)
#       ------- Create input file for daophot -----------
	logfile=imgname[k]//".coo.1"
	print(xc[k],yc[k]," ",outstring,>>logfile)
        centerfile = 'phot.tmp'
        phot(imgname[k],'default','default',verify-,verbose+, >> centerfile)
	delete(logfile,verify-)
#       ------- retrieve photometry and clean up -----------
	list = centerfile
        now = fscan(list,lab0,x1,y1,msky,tmag,sflag)
	delete(centerfile,verify-)
	logfile=imgname[k]//".mag.1"
	delete(logfile,verify-)
#       ------- diagnose photometry and store strings -----------
	if (tmag < 8.0 || tmag > 30.0) goto photfail
        if (tmag == INDEF) goto photfail
        mag[k] = tmag - ac[k]
        if ( ( 10.0*mag[k]-int(10.0*mag[k]) ) > 0.500 ) {mag[k]=mag[k]+0.050}
        smag = str(mag[k])
        photstr[k] = substr(smag,1,4)//" "//filter//"      "
	goto photprint
  photfail:
	photstr[k] = "            "
  photprint:
        if(sflag == 'ok') goto photok
	print("              DAOPHOT ERROR ON FOLLOWING LINE !! Code: ",sflag)
  photok:
	print(  savestr[k]//photstr[k]//substr(ocode,1,3)  )
       }
#       -----------------------------------------------------------

  photqry:
        print("*****************************************")
        print("* (n) next  object (accept photometry)  * ")
        print("* (r) reject a photometric measurement  * ")
        print("* (d) declare first entry discovery     * ")
        print("* (A) abort object (declare unreal)     * "
        print("* (Q) Quit all (terminal)               * ")
        print("*****************************************")
        print("* >>IMCURSOR<< *")
        print("****************")
	now= fscan(imcur, x1, y1, wcs, command)

        if (command == "Q") bye
        if (command == "d") 
        {
	   j = strlen(savestr[1])
	   savestr[1] = substr(savestr[1],1,12)//"*"//substr(savestr[1],14,j)
	   print( savestr[1]//photstr[1]//substr(ocode,1,3) )
	   print( savestr[2]//photstr[2]//substr(ocode,1,3) )
	   print( savestr[3]//photstr[3]//substr(ocode,1,3) )
	   goto photqry
	 }
         if (command == "n") {
	    print(savestr[1]//photstr[1]//substr(ocode,1,3),>>outastrom)
	    print(savestr[2]//photstr[2]//substr(ocode,1,3),>>outastrom)
	    print(savestr[3]//photstr[3]//substr(ocode,1,3),>>outastrom)
	    goto newimg
         }
 	if (command == "r") {
           delqry:
            print(" Delete which phometric entry (1,2,3, or E to keep old) ?")
	    scan(command)
	    if(command == "1") {
		photstr[1] = "            "
	    }
	    else if (command == "2") {
		photstr[2] = "            "
	    }
	    else if (command == "3") {
		photstr[3] = "            "
	    }
	    else if (command == "E") {
		print(" OK. No change" )
	    } else { 
		goto delqry 
            }
	    ;
	    print( savestr[1]//photstr[1]//substr(ocode,1,3) )
	    print( savestr[2]//photstr[2]//substr(ocode,1,3) )
	    print( savestr[3]//photstr[3]//substr(ocode,1,3) )
	    goto photqry
        } else if (command == "A") {
	    goto newimg
        }
	else {
        goto photqry
        }
        ;
	goto photqry
#       ------------------- End photometry --------------------------

# next real object to measure
	print("PANIC ------------- SHOULD NEVER SEE THIS.")
        goto newimg
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
# SUBROUTINES (but badly programmed as spaghetti gotos...Baaaaad Brett...)
#=========================================================================
#=========================================================================
#=========================================================================
  buildra:
#         
#  takes decimal-format RA in 'ra' and adds 11-character RA to 'outstring'
#
	ra = ra/15.000000000
	if(ra<0. || ra>24.)print(">>>>>>>>>>>>>>>>> RA error!, RA= ",ra)
	stra = str(ra)
	len = strlen(stra)
	j = stridx(".",stra)
	shh = substr(stra,1,j-1)
	hh = real(shh)
	if (hh < 9.99999999999) outstring = outstring//"0"
	outstring = outstring//shh
	outstring = outstring//" "
	ra = (ra - hh)*60.0
        stra = str(ra)
	j = stridx(".",stra)
	smm = substr(stra,1,j-1)
	mm = real(smm)
	if (mm < 9.99999999999) outstring = outstring//"0"
	outstring = outstring//smm
	outstring = outstring//" "
	ra = (ra-mm)*60.0
        stra = str(ra)
	j = stridx(".",stra)
	len = strlen(stra)
	if (j == 1) outstring = outstring//"00"
	if (j == 2) outstring = outstring//"0"
	outstring = outstring//substr(stra,1,j+2)
	if ( (len-j)==0 ) outstring = outstring//"00"
	if ( (len-j)==1 ) outstring = outstring//"0"
	outstring = outstring//" "
	if(whereami == 1) goto mainra
	if(whereami == 2) goto recentra
	print("**** PANIC **** in buildra")
#------------
  builddec:
#         
#  takes decimal-format DEC in 'dec' and adds 11-character DEC to 'outstring'
#
	if (dec>90. || dec<-90.) print(" >>>>>>>>>>DEC PROBLEM. DEC = :",dec)
	stdec = str(dec)
	if (dec > 0) outstring = outstring//"+"
	if (dec == 0.0) outstring = outstring//"+"
	if (dec < 0) outstring = outstring//"-"
#   print("stdec:",stdec)
	j = stridx(".",stdec)
	hh = real( substr(stdec,1,j-1) )
#   print(' hh ',hh)
#   if (hh < 0.0000001 && hh > -0.000001) outstring = outstring//"0"
 	if (hh < 9.9999999 && hh > -9.999999) outstring = outstring//"0"
	if (hh < 0.0000) hh = -1.0*hh
	if (dec < 0.0000) dec = -1.0*dec
	shh = str(hh)
	j = stridx(".",shh)
#   print(' shh ',shh)
	outstring = outstring//substr(shh,1,j-1)
	outstring = outstring//" "
# 	print(outstring)
	dec = (dec-hh)*60.00000
	stdec = str(dec)
#   print("stdec:",stdec)
	j = stridx(".",stdec)
	smm = substr(stdec,1,j-1)
	mm = real(smm)
	if (mm < 9.99999999999) outstring = outstring//"0"
	outstring = outstring//smm
	outstring = outstring//" "
# 	print(outstring)
	dec = (dec-mm)*60.0
        stdec = str(dec)
#   print("stdec:",stdec)
	if(dec <0.1) stdec = "00.0"
	j = stridx(".",stdec)
	len = strlen(stdec)
	if (j == 1) outstring = outstring//"00"
	if (j == 2) outstring = outstring//"0"
	outstring = outstring//substr(stdec,1,j+1)
	if ( (len-j) == 0) outstring = outstring//"0"
	outstring = outstring//" "
	if(whereami == 1) goto maindec
	if(whereami == 2) goto recentdec
	print("**** PANIC **** in builddec")

#===========================================================================

 finish:

      print(" ")
      print("       Done. All objects written to ------------>  ",outastrom)
      print(" ")
      cat(outastrom)
      delete(realfile)

      if(ntaken < 3 && nread > 1){
	print(" ")
	print(" WARNING: Only 1 accepted object.  Insure no trailing number!")
      }

  end

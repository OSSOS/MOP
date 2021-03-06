# (LS) MPC-producing photometry script intended for (MOP) pipeline
#
procedure lsmop

#  INPUT: 
#    On command line: 
#	name of file with candidates (image_name.cands.astrom)
#    ASSUME DAOPHOT LOADED
#    Also requires input files:
#	  aper.corr    (with   inner outer correction   for stars, 3 frames)
#    zeropoint.used    (with zeropoint of CCD)
#
#  OUTPUT
#    Files named OBJECT_NAME.ast , one for each object
#
#  MODIFICATIONS
#
#  9 nov 2001 : BG added algorithm to register postage stamps. Needs work
# 10 nov 2001 : BG modified so that user sees UNPLANTED images
# 14 mar 2003 : BG modified for MOP.  NOTE THAT FALSE CANDIDATES EXIST.
# 17 jul 2003 : BG modified mpcmop to lsmop for legacy survey.
# 29 jan 2004 : BG modified lsmop to examine non-planted AND prepare nailing
#                Note that there is now 1 file per object.
# 17 jul 2004 : BG modified to fix naming for LS and better nailing list
# 13 March 2006 : LJ modified to check for EXPOSURE time keyword automatically
#             also to look for EXTVER keyword instead of CHIPID (for JJ)
#             and removed naillist construction (confuses uni)

#string pin  {"FILE.cands.astrom", prompt="file w/header x/y/RA/DEC?"}
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
#string ptag     {"o", prompt="l|o|t|r for lead|opp|trail|recov DEFUNCT"}
#string ppb      {"g", prompt="Photometric passband (1 Char!)"}

begin
    string ppb, ptag
    string now, coeffile, command, pointfile, realfile
    string astfile, dummy, junk, inidstr, comment1,comment2,comment3
    struct gencomment
    string imgname[3], nailimg, nailccd
    int    nnailccd, nextccd,ncomment
    string datestr[3], savestr[3], photstr[3]
    string imgread, centerfile, lab0, lab1, lab2
    real x1, y1, xa1, ya1, xcen, ycen, xkeep,ykeep
    real ra,dec,ut,exp,utcenter,dd,uttime
    real raall[100,4],decall[100,4]
    real xof[3], yof[3]
    string utkey,expkey,datestring,sdd,syy,blank
    string outstring,oname,ustring,stra,stdec,datekey,logfile,tmpstr
    string oobject,thisobject
    int    wcs, box, yy, len, jn, nbuf, k, xsize,ysize
    int     id, chipid, whereami, posk
    string  year, month, day, instid
    real    maxcounts, ramos, decmos
    real    pixscale,xoff,yoff
    real    hh,mm,ss
    string shh,smm,sss,ocode
    string telesc,stringtel,tag, sexpnum
    real   ratemin,ratemax,conecent,conewidth

    string zpfile,prefix,schip,slchip,expname,filter,sflag,smag,infilename
    string hansreal,sxmin,symin,sxmax,symax,sj,imgsec,outastrom
    int    nread,nnew,newflag[100],i,j,xmin,ymin,xmax,ymax
    int    nchip, post, ntaken
    real   xi1[100],xi2[100],xi3[100], diffx,diffy
    real   yi1[100],yi2[100],yi3[100]
    real   xc[3],yc[3]
    real   zp,ac[3],acr,msky,mag[3],tmag
    int    ac1r,ac2r,acin[3],acout[3]
    real   isee[3],iseer,th
    int	   stat
    real   gn,rdn
    real   holdxa1,holdya1
    int    dupflag,fkdispflag 
#--------------------------------------------

    realfile=mktemp("tmp$pt")
    ntaken = 0
    fkdispflag = 0

        print("  ")
        print(" ----- PIPELINE Photometry script ----- ")
        print(" I am assuming that : ")
        print(" 1. all DAOphot datapars are correct and ")
        print(" 2. that DAOPhot is loaded. ")
        print(" 3. the zeropoint & aper.corr file exist (or I will use defaults) ")

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
          print("File   "//zpfile//"   doesn't exist. ");
	  print("Using default value of 26.46");
	  zp=26.46	
# 	set filter to D for default .. b/c using default ZP
	  filter="D"
          ;
        } else {
	  list = "zeropoint.used"
	  dummy=fscan(list,zp)
	# filter will get reset to proper value later
	  filter="r" 
          print('PHOTOMETRIC PARAMETERS READ.  Using (1s) zeropoint: ',zp)
	} 
	;

	zpfile = "aper.corr"
        if ( !access(zpfile) ) {
	  print(" ")
          print("File   "//zpfile//"   doesn't exist. STOP. \n");
	  print("Using default values of 5, 15 and 0.2\n");
	  for(j=1; j<=3; j+=1){
	   acin[j] = 5
	   acout[j] = 15
	   ac[j] = 0.2
          ;
          }
	;
	} else {
	;
	list = zpfile
	ac1r=5
	ac2r=15
	acr=0.2
	for(j=1; j<=3; j+=1){
	   dummy=fscan(list,ac1r,ac2r,acr)
	   acin[j] = ac1r
	   acout[j] = ac2r
	   ac[j] = acr
	}
	;
	}
	;

	print(" ")
#       print(" >>> zp,acin,acout,ac : ", zp,acin,acout,ac)

	photpars.zmag = zp
	datapars.ccdread = ''
	datapars.gain = "GAIN"
	datapars.readnoi = 7.0
	datapars.filter = "FILTER"
	datapars.obstime = "UTC-OBS"
        datapars.exposur = "EXPTIME"
#  	box = pbox


!       ls -1 *FAIL*
!       ls -1 *WARN*
!       ls -1 *.astrom
!       ls -1 *.cands.astrom > candsastrom.filename

#	astfile = pin
	infilename = "candsastrom.filename"
	list = infilename
	dummy = fscan(list,astfile)
	print(" ")
	print(" Reading the following file:")
	print(astfile)
	
!	/bin/rm candsastrom.filename

	list = astfile
#-------- Get image names
	for(j=1 ; j<=3; j+=1) {

	     dummy=fscan(list,junk,imgread)
	     imgname[j] = imgread
#	     print(" imgname[j] : ",j, imgname[j])

### HARDWIRE         outastrom= pof
             if(j==1) {
        	outastrom= imgname[1]//".NONE.astrom"
                if ( access(outastrom) ) {
	  	   print(" ")
                error(1," File "//outastrom//" ALREADY EXISTS.  STOP. \n");
                ;
                }
        	outastrom= imgname[1]//".real.astrom"
                if ( access(outastrom) ) {
	  	   print(" ")
                error(1," File "//outastrom//" ALREADY EXISTS.  STOP. \n");
                ;
                }
	     } 
#	     print("# ", imgname[j], >> outastrom)

# If implanted images are read, then peel off the first two characters

	     len = strlen(imgname[j])
             posk = stridx("k",imgname[j])
             imgname[j] = substr(imgname[j],posk+1,len)
	     if ( ! access(imgname[j]//".fits") ) {
		print("imcopy "//imgname[j]//".fits.fz[1] "//imgname[j]//".fits", > "zzzzz");
!		source ./zzzzz
!		/bin/rm zzzzz
	     }
#	     print(" imgname[j] ,posk : ",j, imgname[j],"  ", posk)

#	   LJ -  set filter
	     if(j==1) {
	       	imgets(imgname[j], 'FILTER')
		ppb = imgets.value
		ppb = substr(ppb,1,1)	
#		print ("Find image ", imgname[j]," has filter ", ppb , " \n")
		if(filter!="D") filter=ppb
	     }
        }


#-------- Get MOPheader information
       for(j=1;j<=3;j+=1) {
          stat = fscan(list, line)
#          print(line, >> outastrom)
          stat = fscan(list, line)
#          print(line, >> outastrom)
          stat = fscan(list, line)
#          print(line, >> outastrom)
    dummy=fscan(list,junk,year,month,day,exp,th,iseer,maxcounts,ramos,decmos,id)
#    print("# ",year,month,day,exp,th,iseer,maxcounts,ramos,decmos,id,>>outastrom)
	isee[j] = iseer
##	     print(" year month day id: ", year,month,day,id)
	datestr[j] = year//" "//month//" "//day
##	     print(" datestr1 : ", datestr[j])
          stat = fscan(list, line)
#          print(line, >> outastrom)
    dummy=fscan(list,junk,pixscale,chipid,xoff,yoff,xsize,ysize,inidstr,gn,rdn)
#    print("# ",pixscale,chipid,xoff,yoff,xsize,ysize,inidstr,gn,rdn,>>outastrom)

        }


#-------- Get MOPheader information and echo) ---- rate infor and header
#	dummy=fscan(list,junk,ratemin,ratemax,conecent,conewidth)

          stat = fscan(list, line)
#          print(line, >> outastrom)
          stat = fscan(list, line)
#          print(line, >> outastrom)
          stat = fscan(list, line)
#          print(line, >> outastrom)

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
	       holdxa1 = xa1
	       holdya1 = ya1
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
	     dupflag = 0
	     i = 1
	     while( i < nread ) {
	        diffx = x1 - xi1[i]
	        diffy = y1 - yi1[i]
#                            ---- tolerance is 3-pix radius ---
		if( ((diffx*diffx + diffy*diffy) < 9.0) && dupflag==0 ) {
                         newflag[nread]=0
                         nnew = nnew - 1
			dupflag = 1
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
#        print( " " )
#        print( "OFFSETS : " )
#        print(xof[1],yof[1],xof[2],yof[2],xof[3],yof[3])


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
   disploop:
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

#          print( "imgsec : ",imgsec )

          imgread=imgname[j]//imgsec
	  if (fkdispflag > 0.5 ) imgread = 'fk'//imgread
          display(imgread,j)

        }
	if (fkdispflag > 0.5 ) {
	 	fkdispflag=0
                print("     *********** showing IMPLANTED images ******")
	}

# --------------------------------------------------
	pointfile=mktemp("tmp$pt")
        print(xi1[i], yi1[i], > pointfile)
        tvmark(1,pointfile,mark='circle',radii=acout[1],color=205,interactive=no)
        delete(pointfile, verify-)
	pointfile=mktemp("tmp$pt")
        print(xi2[i], yi2[i], > pointfile)
        tvmark(2,pointfile,mark='circle',radii=acout[2],color=205,interactive=no)
        delete(pointfile, verify-)
	pointfile=mktemp("tmp$pt")
        print(xi3[i], yi3[i], > pointfile)
        tvmark(3,pointfile,mark='circle',radii=acout[3],color=205,interactive=no)
        delete(pointfile, verify-)

        if(ntaken > 0) {
            tvmark(1,realfile,inter-,mark="rectangle",lengths="5.0",color=205,interactive=no)
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
         print("* 'r'= re-display non-planted images  'f'=see fakes     *")
         print("* 'c'=centroid an image ,             'Q'=Quit program  *")
         print("*********************************************************")
         print("* >>IMCURSOR<< *")
         print("****************")
         now= fscan(imcur, x1, y1, wcs, command)
         xkeep = x1
         ykeep = y1
         if (command == "Q") bye
         if (command == "r") {
		fkdispflag = 0
		goto disploop
	 }
         if (command == "f") {
		fkdispflag = 1
		goto disploop
	 }
         if (command == "d")   {
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

#             PRODUCE OBJECT NAME.  THIS IS FOR CFEPS LS AUTO GENERATION.  
#                                   But can overrule.
#		START WITH TAG, WHICH IS l/o/t/r for now, but any single
#		digit is OK  .. LJ - this is dropped later, but still counting
	      ptag = "o"
	      tag=ptag
	      j = strlen(tag)
	      if (j>1) print ("*** tag > 1 character! STOP! ***********")
	      oname = tag

# BG eliminated the leading o on July 17/2004 as per Lynne's request
		oname = ''


#              len = stridx("t",expname)
#	      if(len==0 || len > j) print(" *****>>>>>> PANIC ON NAME. STOP!")
#	      oname = substr(expname,len+1,len+3)//"a"

              imgets(imgname[1],'OBJECT')
              expname = imgets.value
	      len = strlen(expname)
	      
	      expname = substr(expname,3,len)
# 		Expect all OBJECT names to be 9 characters
	      j = strlen(expname)
	      if (j >9 || j<9) print ("*** WARNING, field ne 9 char *********")
# The leading letter has now disappeared, so subscript now 1,9 intead of 2,9
	      if (ntaken == 0) oname = oname//substr(expname,1,9)//"a"
 	      if (ntaken == 1) oname = oname//substr(expname,1,9)//"b"
 	      if (ntaken == 2) oname = oname//substr(expname,1,9)//"c"
 	      if (ntaken == 3) oname = oname//substr(expname,1,9)//"d"
 	      if (ntaken == 4) oname = oname//substr(expname,1,9)//"e"
	      if (ntaken == 5) oname = oname//substr(expname,1,9)//"f"
	      if (ntaken == 6) oname = oname//substr(expname,1,9)//"g"
	      if (ntaken == 7) oname = oname//substr(expname,1,9)//"h"
	      if (ntaken == 8) oname = oname//substr(expname,1,9)//"i"
	      if (ntaken == 9) oname = oname//substr(expname,1,9)//"j"
	      if (ntaken == 10) oname = oname//substr(expname,1,9)//"k"
#		I ASSUME THIS SHOULD NEVER HAPPEN IN THE LEGACY SURVEY...
 	      if (ntaken > 10) oname = "namePANIC"

#		Chips is a zero-based for legacy survey.
#              imgets(imgname[1],'CHIPID')
              imgets(imgname[1],'EXTVER')
              schip = imgets.value
              nchip = int(imgets.value)
#		Following is to have a leading zero ready
	      if(nchip > 9) slchip=''
	      if(nchip < 10) slchip='0'
 	      if (nchip < 0 || nchip > 36) print ("!! chip out of bounds !!")
 	      if (nchip < 10) oname = oname//str(schip)
 	      if (nchip == 10) oname = oname//"A"
 	      if (nchip == 11) oname = oname//"B"
 	      if (nchip == 12) oname = oname//"C"
 	      if (nchip == 13) oname = oname//"D"
 	      if (nchip == 14) oname = oname//"E"
 	      if (nchip == 15) oname = oname//"F"
 	      if (nchip == 16) oname = oname//"G"
 	      if (nchip == 17) oname = oname//"H"
 	      if (nchip == 18) oname = oname//"I"
 	      if (nchip == 19) oname = oname//"J"
 	      if (nchip == 20) oname = oname//"K"
 	      if (nchip == 21) oname = oname//"L"
 	      if (nchip == 22) oname = oname//"M"
 	      if (nchip == 23) oname = oname//"N"
 	      if (nchip == 24) oname = oname//"O"
 	      if (nchip == 25) oname = oname//"P"
 	      if (nchip == 26) oname = oname//"Q"
 	      if (nchip == 27) oname = oname//"R"
 	      if (nchip == 28) oname = oname//"S"
 	      if (nchip == 29) oname = oname//"T"
 	      if (nchip == 30) oname = oname//"U"
 	      if (nchip == 31) oname = oname//"V"
 	      if (nchip == 32) oname = oname//"W"
 	      if (nchip == 33) oname = oname//"X"
 	      if (nchip == 34) oname = oname//"Y"
 	      if (nchip == 35) oname = oname//"Z"

  	      print(" CFHT LS name : PASTE IN or CHANGE")
	      print(" ")
 	      print(oname)
	      print(" ")
      nameqry:
	      prefix="?"
	      print(" Enter object name (2-11 characters)")
# Hardwire.  Note that thisobject is saved for nailing tool operations
#              scan(prefix)
	      prefix = oname
	      thisobject = oname
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

		pointfile='fk'//imgname[k]//'.tmpxy'
		print(x1,y1, > pointfile)
                tvmark(k,pointfile,mark="circle",radii=2,label=yes,color=206,interactive=no)
#		type(pointfile)
#          CALL HANS SINGLE-PASS Fortran CODE HERE.
		junk='callhansastone'
		dummy = 'fk'//imgname[k]
	        print( "hansastone  ",dummy, >junk )
		print("source ./callhansatone")
#          AM CALLING THIS WITH AN UGLY SYSTEM CALL, BUT IT WORKS
!               chmod 744 callhansastone
		print("    Using   hansastone   for (x,y)-->(ra,dec)")
!               source callhansastone
#          DELETE ALL TEMP FILES AFTER USE.
!               /bin/rm callhansastone dummy_ref_stars
		delete(pointfile)
#          NOW NEED TO DETERMINE IF IT RETURNED THE .OK FILE
#          FOR NOW AM TERMINATING LSMOP IF HANSASTONE FAILS...IMPROVE!
	        junk = 'fk'//imgname[k]//'.hansastone.OK'
                if ( !access(junk) ) {
	           print(" ")
                   error(1,"File   "//junk//"   doesn't exist. STOP. \n");
                   ;
                }
		delete(junk)
	        junk = 'fk'//imgname[k]//'.hansastone.FAILED'
		delete(junk)
		pointfile='fk'//imgname[k]//'.tmpradec'
		list = pointfile
        	now = fscan(list,ra,dec)
                print(' Hansastone returns --->  RA : ',ra,'  DEC: ',dec)
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

#--------------------------------------------------


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
   print(" isee box : ",isee[2], box)
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
   print(" isee box : ",isee[3], box)
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
        tvmark(k,pointfile,mark='circle',radii=acin[k],color=205,interactive=no)
        tvmark(k,pointfile,mark='circle',radii=(acout[k]+1),color=204,interactive=no)
        tvmark(k,pointfile,mark='circle',radii=(acout[k]+3*isee[k]+1),color=204,interactive=no)
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

	comment1 = ' '
	comment2 = ' '
	comment3 = ' '
  photqry:
        print("*****************************************")
        print("* (c) ADD COMMENT to any observation    * ")
        print("* (n) next object (accept photometry)   * ")
        print("* (r) reject a photometric measurement  * ")
        print("* (d) declare first entry discovery     * ")
        print("* (A) abort object (declare unreal)     * "
        print("* (Q) Quit all (terminal)               * ")
        print("*****************************************")
        print("* >>IMCURSOR<< *")
        print("****************")
	now= fscan(imcur, x1, y1, wcs, command)

        if (command == "Q") bye
        if (command == "c") 
	{
   dufus:
	   print(" Comment for which of the 3 astrometric lines (1,2,3) ")
	   scan(ncomment)
	   if (ncomment < 1) goto dufus
	   if (ncomment > 3) goto dufus
	   print(" Enter a <41-character comment string. ")
#	   scanf("%40s",junk)
	   scan(gencomment)
	   junk=gencomment
	   print(" junk: ",junk)
	   j = strlen(junk)
	   if (j>40) j=40
	   if (ncomment == 1) comment1=' '//substr(junk,1,j)
	   if (ncomment == 2) comment2=' '//substr(junk,1,j)
	   if (ncomment == 3) comment3=' '//substr(junk,1,j)
	}
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
# 	NOW this goes out to real.astrom and individual to each list
	    oobject = thisobject//'.ast'
            imgets(imgname[1],'EXPNUM')
            sexpnum = imgets.value
	    print("#L ", sexpnum,"o_",slchip,nchip," ",expname, comment1, >>outastrom)
	    print(savestr[1]//photstr[1]//substr(ocode,1,3),>>outastrom)
	    print("#L ", sexpnum,"o_",slchip,nchip," ",expname, comment1, >oobject)
	    print(savestr[1]//photstr[1]//substr(ocode,1,3),>>oobject)
            imgets(imgname[2],'EXPNUM')
            sexpnum = imgets.value
	    print("#L ", sexpnum,"o_",slchip,nchip," ",expname, comment2, >>outastrom)
	    print(savestr[2]//photstr[2]//substr(ocode,1,3),>>outastrom)
	    print("#L ", sexpnum,"o_",slchip,nchip," ",expname, comment2, >>oobject)
	    print(savestr[2]//photstr[2]//substr(ocode,1,3),>>oobject)
            imgets(imgname[3],'EXPNUM')
            sexpnum = imgets.value
	    print("#L ", sexpnum,"o_",slchip,nchip," ",expname, comment3, >>outastrom)
	    print(savestr[3]//photstr[3]//substr(ocode,1,3),>>outastrom)
	    print("#L ", sexpnum,"o_",slchip,nchip," ",expname, comment3, >>oobject)
	    print(savestr[3]//photstr[3]//substr(ocode,1,3),>>oobject)

#	BUILD NAILING LIST. Discovery triplet, this nailing CCD and the
#       the next higher numbered CCD.  This should work >90% of the time.
# NOPE: Now find and place ALL nailing images into this list.

#     LJ : Now don't do this at all, because it confuses uni

#	    junk = thisobject//'.naillist'
#	    print(imgname[1],  >junk)
#	    print(imgname[2], >>junk)
#	    print(imgname[3], >>junk)
# Changed July 17 2004 to structure in which nailing images aren't below
# BG !	    cd nailing ; ls -1 *.fits > ../nailingimage ; cd ..

#!	    /bin/rm -f nailingimages
#!	    ls -1 ../../chip*/*N/7*o_??.fits > ./nailingimages 
#!		echo N >> ./nailingimages

#	    dummy = 'nailingimages'
#            if ( !access(dummy) ) {
#	           print(" ")
#                   print(" NO NAILING IMAGE ")
#	           print("# NO NAILING IMAGE ", >>junk)
#		   goto nailprompt
#                   ;
#                }
#	    list = 'nailingimages'
#nailloop:
#	      dummy=fscan(list,nailimg)
#	      j = stridx("N",nailimg)
#	      if (j == 1) goto nailprompt
#	      print(substr(nailimg,j+2,j+11), >>junk)
#	    goto nailloop

#	      j = stridx("_",nailimg)
#	      nailccd = substr(nailimg,j+1,j+2)
#	      nnailccd = int(nailccd)
#	    nextccd= nnailccd + 1
#	    dummy   = substr(nailimg,1,j)
#	    if(nextccd < 10) dummy= dummy//'0'
#	    dummy= dummy//nextccd
#	    print(dummy, >>junk)

#nailprompt:
#!	    /bin/rm nailingimages

#	    print(" ")
#	    print(" May wish to run nailing tool with : ")
#	    print(" ")
#	    print(" nail ",thisobject)
#	    print(" ")
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
      rename(astfile,astfile//"checked")

      if ( access(outastrom) ) {
	 cat(outastrom) ;
      }
      ;

      if ( access(realfile) ) { 
	delete(realfile) ; 
      }
      ;

      for (j=1; j<=3; j+=1) {
	  if ( ! access(imgname[j]//".fits.fz") ) {
#	     print ("imcopy "//imgname[j]//".fits "//imgname[j]//".fits.fz[compress]", > "zzzzz");
#!	     source ./zzzzz
#!	     /bin/rm zzzzz
#	  }
#	  print("/bin/rm "//imgname[j]//".fits", > "zzzzz")
#!	  source ./zzzzz
#!	  /bin/rm zzzzz
#      }
      if(ntaken == 0){
	print(" ")
	print(" WARNING: no accepted objects!")
        if ( access(outastrom) ) { 
	   delete(outastrom, verify-)
        }
        ;
	zpfile = imgname[1]//".NONE.astrom"
	print(" All candidates reject ",>>zpfile)
#!	touch allrejected.NONE.astrom
      }
  end

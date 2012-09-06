#  pcfh99I.cl  modification of cfhtp.cl from B. Gladman and JJ. Kavelaars
# 
# Modification history:
# Mar 26/99 Petit: modified to allow magnitude variation of reference star
#                  and uses PSFs for each image.
#
#   Plants fake objects in N images at a random rate and direction.
#  Accesses the image headers to get the exposure start time.  User
#
#  Input requirements: There must be N(=nimg) REGISTERED image named xxxxn
#      with xxxx being the common file name and  n sequential.
#
#  Reference star is one chosen on the object frame with the same AM as the
#  calibrator frame.  Magnitude variations are computed for this ref star
#  and all planted stars in each frame are put in with the same magnitude 
#  offset.
#
#  -------->  NOTE:  This script reads the .pst files created by phot.  In
#  building the reference star list one runs phot initially, then pstselect
#  (which creates image.pst.1) and then psf (which creates image.pst.2).  It
#  is this SECOND image (image.pst.2) from which the magnitude of the reference
#  star is taken.  Thus, no other .pst images should exist in this directory.
#

procedure pcfh99I (common,prefimg,pfieldn,pseed)
        string common {"", prompt=" Common portion of filename "}
	string prefimg {"", prompt=" name of reference image "}
	string pfieldn {"", prompt=" name of header keyword for obs time "}
        int pseed     {"", prompt=" seed number "}
	int pnimg     {"", prompt=" number of aligned images "}
	int pnobjmin  {"", prompt=" minimum number of objects to plant"}
	real pnobjdis  {"", prompt=" max number MORE than min number"}
        real pfrmsz   {1900, prompt=" frame size (pixels) 100 pix padded to y"}
        real pratem   {"", prompt=" MEAN rate on sky (pixels/hour)"}
        real pratedis {"", prompt=" rate dispersion (pixels/hour)"}
        real panglem   {"", prompt=" MEAN |angle| (degrees)"}
        real pangledis {"", prompt=" angle dispersion (degrees)"}
	real minmag    {"", prompt="minimum INSTRUMENTAL mag to plant at"}
	real magdis    {"", prompt="max mag FAINTER than min mag"}
begin 
        int    nimg, cpflag, nobj, i, j, nobjmin
        real   rate,xsh0,ysh0,angle,outang, nobjdis
	real   ratedis,angledis,raternd,anglernd
	real   ratem,anglem
        real   xsh,ysh
        real   currtime, strttime
        real   xstart,ystart,mag,rnum,realmag
	real   xss[150],yss[150],as[150],rs[150],mags[150]
	real   dx,dy,dist
	real   rmag,frmsz
	real   xcen,ycen,refmag,thisrefmag,dmag,thismag
	int    rseed
        string infile,rootword,outfile,tfile,dummy
        string objfile,newfield,refimg,fieldn,thisfile
 
        cache("utilities.urand")
	cache("daophot")
        cache("digiphot.daophot.addstar")

        rootword = common
	refimg = prefimg
	fieldn = pfieldn
	rseed  = pseed
	nobjmin = pnobjmin
	nobjdis = pnobjdis
	frmsz = pfrmsz
	ratem   = pratem
	ratedis=pratedis
	anglem  = panglem
	angledis=pangledis
	nimg = pnimg

	if (nobjmin > 150) {nobjdis = 150}
	if (nobjmin+nobjdis > 151) {nobjdis = 151 - nobjmin}
        print(" ")
        print(" CFH Object planting program") 
        print(" ") 
        print(" Input to this program is from N registered images! ")
	i = nobjmin
	j = nobjmin+nobjdis-1
        print(" Plants ", i,"-", j, " objects at given rate with dispersion")
        print(" ") 
        print(" Objects move to upper right! ") 
        print(" Frame size doubled for y axis due to non-squarility :-)") 
        print(" ") 

# difference with jjp.cl is that signs removed on next 2 lines
        anglem = anglem/57.29578
        angledis = angledis/57.29578
        print("  ") 
        print(" Modifying images: ",rootword) 
        print(" Will create fkxxxxx frames. Input anything to continue ") 
# Note that this is now a dummy read.
        scan(cpflag)
	cpflag=1

        print("  ") 
        print(" Working... ") 
	urand(1,1,ndigits=5,seed=rseed,scale_factor=nobjdis) | scan nobj
 	nobj = nobj + nobjmin
        objfile=("Object.list")
	for (i=1; i<=nobj; i+=1) {

	 redo:

	 urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed,scale_factor=magdis) | scan rmag

# compute instrumental mag
# FOR CFH99I all photometry is on standard system, matched to AM of SA98 2 sec.
	 mag = minmag + rmag
	 realmag = mag 
	 urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=frmsz) | scan xstart
         urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=2.*frmsz) | scan ystart
	 ystart=ystart + 100.
         urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 rate = ( 2.0*(rnum-0.5) )*ratedis + ratem
         urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 angle = ( 2.0*(rnum-0.5) )*angledis + anglem

	 xss[i] = xstart
	 yss[i] = ystart
	 mags[i] = mag
	 rs[i] = rate
	 as[i] = angle
#		print(" xss",i," =",xstart,"      mag=",mag)
	   for (j=1; j<i; j+=1)
	   {
		dx = xss[i] - xss[j]
		dy = yss[i]-yss[j]
		dist = dx*dx + dy*dy
		if (dist < 1.0) {
	     print(" PANIC! Duplicate i",i," and j",j," Redoing last object.")
		   goto redo
		}
		;
	   }
	   ;
         print(xstart,ystart,realmag,rate,57.3*angle, >> objfile)
	}

#        print("KILL shell if any PANICs appear above. Enter to 1 continue") 
# Note that this is now a dummy read.
#        scan(cpflag)
#	if (cpflag != 1) print ("ERROR ! ! STOP STOP STOP !!!!")
#--------------------------------------------------------------------------

        print(" ") 
        print(" Object list built.  Processing files: ")
        print(" ") 

	tfile=mktemp("tmp$pt")
	infile = refimg//".pst.2"
	pdump(infile,"MAG","yes", >> tfile)
	list=tfile
	dummy = fscan(list,refmag)
	delete(tfile, verify-)

        infile = rootword//1
        gettime(infile,fieldn)
        strttime = gettime.outputime
        print(" **  Image 1 acquired at ", strttime, " UT") 

        for (j=1; j<=nimg; j+=1)
        {
            infile = rootword//j
            outfile = 'fk'//rootword//j
            print("       Currently working on: ",outfile)
            gettime(infile,fieldn)
            currtime = gettime.outputime
            print(" * Image ",j," acquired at ", currtime, " UT") 
   	    thisfile = infile//".pst.2"
	    tfile=mktemp("tmp$pt")
            pdump(thisfile,"MAG","yes", >> tfile)
            list=tfile
            dummy = fscan(list,thisrefmag)
	    delete(tfile, verify-)
	    dmag = thisrefmag - refmag
# This is a sloppy re-use of the objfile variable....but it's OK.
            objfile=mktemp("addstar")
            for (i=1; i<=nobj; i+=1)
            {
        	xsh0 = rs[i]*cos(as[i])
        	ysh0 = rs[i]*sin(as[i])

#  Note - signs (compare findkuiper) have been removed!
                ysh = ysh0*(currtime - strttime)
                xsh = xsh0*(currtime - strttime)
		xsh = xsh + xss[i]
		ysh = ysh + yss[i]
		thismag = mags[i] + dmag
         	print(xsh,ysh,thismag,i, >> objfile)

#	                print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#               	print(" xshift ",xsh," and yshift ",ysh) 

            }
	    print(" planting objects in image ",j)
            addstar(infile,objfile,"default",outfile,simple+,verify-,verbose-)
 	    delete(objfile, verify-)
 	    delete(outfile, verify-)
        
            newfield = 'pcfh99I-img-no'
            hedit(outfile,newfield,(j),add+,show-,update+,verify-)

        }

#        print(" Deleting object file!")
#        delete(objfile, verify-)
        print(" ")
        print(" Done ")
        print(" Object.list EXISTS; gzip it!")
        print(" ")

end


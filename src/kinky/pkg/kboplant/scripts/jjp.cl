#  jjp.cl    JJ's Plant, using addstar
#
#   Plants fake objects in N images at a random rate and direction.
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) REGISTERED image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#
#   Using the scaling in the globular cluster frame, the instrumental
# system is 0.38 mags too high (brighter) (eg. star 18.12 in instrumental
# system is really 18.50).  So I adjusted the phot zeropoint to 25.38 to
# compensate, making the instrumental and standard systems agree.
#

#procedure sop(common,pseed,pobjects)
procedure jjp(common,pseed)
        string common {"", prompt=" Common portion of filename "}
	string psffile {"", prompt="PSFILE"}
        int pseed     {"", prompt=" seed number "}
#	int pobjects  {"", prompt=" number of objects to generate (49 max)"}
	int pnimg     {"", prompt=" number of aligned images"}
        real pfrmsz   {"", prompt=" frame size (pixels) 50 pix padded to y"}
        real pratem   {"", prompt=" MEAN rate on sky (pixels/hour)"}
        real pratedis {"", prompt=" rate dispersion (pixels/hour)"}
        real panglem   {"", prompt=" MEAN |angle| (degrees)"}
        real pangledis {"", prompt=" angle dispersion (degrees)"}
	real minmag    {"", prompt="minimum INSTRUMENTAL mag to plant at"}
	real magdis    {"", prompt="max mag FAINTER than min mag"}
begin 
        int    nimg, cpflag, nobj, i, j
        real   rate,xsh0,ysh0,angle,outang
	real   ratedis,angledis,raternd,anglernd
	real   ratem,anglem
        real   xsh,ysh
        real   currtime, strttime
        real   xstart,ystart,mag,rnum,realmag
	real   xss[100],yss[100],as[100],rs[100],mags[100]
	real   dx,dy,dist
	real   rmag,frmsz
	int    rseed
        string infile,rootword,outfile
        string objfile,newfield,tpsf
 
        cache("utilities.urand")

        print(" ")
        print(" Object planting program") 
        print(" ") 
        print(" Input to this program is from N registered images! ") 
        print(" Plants 41-55 objects at given rate with dispersion")
        print(" ") 
        rootword = common
	tpsf = psffile
	rseed  = pseed
	frmsz = pfrmsz
	ratem   = pratem
	ratedis=pratedis
	anglem  = panglem
	angledis=pangledis
	nimg = pnimg
#	nobj = pobjects

        anglem = -anglem/57.29578
        angledis = -angledis/57.29578
        print("  ") 
        print(" Modifying images: ",rootword) 
        print(" Will create fkxxxxx frames. Input anything to continue ") 
# Note that this is now a dummy read.
        scan(cpflag)
	cpflag=1

        print("  ") 
        print(" Working... ") 
	urand(1,1,ndigits=5,seed=rseed,scale_factor=15.) | scan nobj
#	nobj = nobj + 1
	nobj = nobj + 40
	for (i=1; i<=nobj; i+=1) {
         objfile=("Object.list")


	 urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed,scale_factor=magdis) | scan rmag
# compute instrumental mag
	 mag = minmag + rmag
# standard system is NO LONGER the same!  This is written to Object.file
#					  and addstar uses mag
	 realmag = mag + 0.32
	 urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=frmsz) | scan xstart
         urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=frmsz) | scan ystart
	 ystart=ystart + 50.
         urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 rate = ( 2.0*(rnum-0.5) )*ratedis + ratem
         urand(1,1,ndigits=5,seed=rseed,scale_factor=100000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 angle = ( 2.0*(rnum-0.5) )*angledis + anglem

         print(xstart,ystart,realmag,rate,57.3*angle, >> objfile)
	 xss[i] = xstart
	 yss[i] = ystart
# note that INSTRUMENTAL magnitude is passed to addstar, while the
# magnitude on standard system is written to objfile above
	 mags[i] = mag
	 rs[i] = rate
	 as[i] = angle
#		print(" xss",i," =",xstart,"      mag=",mag)
	   for (j=1; j<i; j+=1)
	   {
		dx = xss[i] - xss[j]
		dy = yss[i]-yss[j]
		dist = dx*dx + dy*dy
		if (dist < 1.0) print(" PANIC! Duplicate i",i," and j ",j)
	   }
	   ;
	}

        print("KILL shell if any PANICs appear above. Enter to 1 continue") 
# Note that this is now a dummy read.
        scan(cpflag)
	if (cpflag != 1) print ("ERROR ! ! STOP STOP STOP !!!!")

        print(" ") 
        print(" Processing files: ")
        print(" ") 
        infile = rootword//1
        gettime(infile)
        strttime = gettime.outputime
        print(" **  Image 1 acquired at ", strttime, " UT") 

        for (j=1; j<=nimg; j+=1)
        {
                infile = rootword//j
                outfile = 'fk'//rootword//j
                print("       Currently working on: ",outfile)
                gettime(infile)
                currtime = gettime.outputime
                print(" * Image ",j," acquired at ", currtime, " UT") 
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
         	print(xsh,ysh,mags[i],i, >> objfile)
#	                print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#               	print(" xshift ",xsh," and yshift ",ysh) 

               }
		print(" planting stars in image ",j)
                addstar(infile,objfile,tpsf,outfile)
 	        delete(objfile, verify-)
        
                newfield = 'jjpcl-img-no'
                hedit(outfile,newfield,(j),add+,show-,update+,verify-)

        }

#        print(" Deleting object file!")
#        delete(objfile, verify-)
        print(" ")
        print(" Done ")
        print(" Object file STILL EXISTS")
        print(" ")

end


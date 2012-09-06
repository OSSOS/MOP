#  sop.cl    Son of Plant
#
#   Plants fake objects in N images at a random rate and direction.
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) REGISTERED image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#  THESE IMAGES WILL BE OVERWRITTEN WITH THE PLANTED OBJECT!
#
#  NB: mag=-1.2 below gives a R=23.0 source in a 480 sec scaled COSMIC frame
#      and thus mag=0 implies R=24.2
#  SCALING IS ONLY CORRECT IF PLANTED IN THE ORIGINAL (NON-SKY SUBTRACTED) IMAGES
#   (it looks like in sept8 sky-sub frames real magnitude is 0.6 mags off)
#

#procedure sop(common,pseed,pobjects)
procedure sop(common,pseed)
        string common {"", prompt=" Common portion of filename "}
        int pseed     {"", prompt=" seed number "}
#	int pobjects  {"", prompt=" number of objects to generate (49 max)"}
	int pnimg     {"", prompt=" number of aligned images"}
        real pratem   {"", prompt=" MEAN rate on sky (pixels/hour)"}
        real pratedis {"", prompt=" rate dispersion (pixels/hour)"}
        real panglem   {"", prompt=" MEAN |angle| (degrees)"}
        real pangledis {"", prompt=" angle dispersion (degrees)"}
begin 
        int    nimg, cpflag, nobj, i, j
        real   rate,xsh0,ysh0,angle,outang
	real   ratedis,angledis,raternd,anglernd
	real   ratem,anglem
        real   xsh,ysh
        real   currtime, strttime
        real   xstart,ystart,mag,rnum,realmag
	real   xss[50],yss[50],as[50],rs[50],mags[50]
	int    rseed
        string infile,rootword,outfile
        string objfile,newfield
 
        cache("noao")
        cache("noao.artdata")
        cache("noao.artdata.mkobjects")
        cache("utilities.urand")

        print(" ")
        print(" Object planting program") 
        print(" ") 
        print(" Input to this program is from N registered images! ") 
        print(" Plants 15-30 objects at given rate with dispersion")
        print(" ") 
        rootword = common
	rseed  = pseed
	ratem   = pratem
	ratedis=pratedis
	anglem  = panglem
	angledis=pangledis
	nimg = pnimg
#	nobj = pobjects

        anglem = -anglem/57.29578
        angledis = -angledis/57.29578
        print(" THESE IMAGES WILL BE OVERWRITTEN!! Input anything to continue ") 
        scan(cpflag)
	cpflag=1

	urand(1,1,ndigits=5,seed=rseed,scale_factor=15.) | scan nobj
	nobj = nobj + 15
	for (i=1; i<=nobj; i+=1) {
         objfile=("Object.list")
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
# rnum from 0 to 1, so get number from 1.0 to 2.0
	 mag = rnum*1.00000 + 1.0
#>>>>>>>>>>>>> real magnitude is mag+24.2, so from 25.2 to 26.2
	 realmag = mag + 24.2
	 urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=1900.) | scan xstart
         urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=1900.) | scan ystart
	 ystart=ystart + 50.
         urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 rate = ( 2.0*(rnum-0.5) )*ratedis + ratem
         urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 angle = ( 2.0*(rnum-0.5) )*angledis + anglem

         print(xstart,ystart,realmag,rate,57.3*angle, >> objfile)
	 xss[i] = xstart
	 yss[i] = ystart
	 mags[i] = mag
	 rs[i] = rate
	 as[i] = angle
	}

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
                if (cpflag == 1){
                outfile = infile
                }
                if (cpflag == 0){
                outfile = 'fk'//rootword//i
                imcopy(infile,outfile)
                }
                print("       Currently working on: ",outfile)
                gettime(outfile)
                currtime = gettime.outputime
                print(" * Image ",j," acquired at ", currtime, " UT") 
               for (i=1; i<=nobj; i+=1)
               {
        	xsh0 = rs[i]*cos(as[i])
        	ysh0 = rs[i]*sin(as[i])

#  Compute offset, then add object
#  Note - signs (compare findkuiper) have been removed!
                ysh = ysh0*(currtime - strttime)
                xsh = xsh0*(currtime - strttime)
#               print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#               print(" xshift ",xsh," and yshift ",ysh) 

         	objfile=mktemp("fake")
         	print(xss[i],yss[i],mags[i], >> objfile)
                mkobjects(outfile,objects=objfile,xoffset=xsh,yoffset=ysh, star="gaussian", radius=1.7, exptime=1., magzero=7.0,comments-)
	        delete(objfile, verify-)
               }
        
# Record shift rates in header
#                newfield = 'xstart'
#                hedit(outfile,newfield,(xstart),add+,show-,update+,verify-)
#                newfield = 'ystart'
#                hedit(outfile,newfield,(ystart),add+,show-,update+,verify-)
#                newfield = 'xshift'
#                hedit(outfile,newfield,(xsh),add+,show-,update+,verify-)
#                newfield = 'yshift'
#                hedit(outfile,newfield,(ysh),add+,show-,update+,verify-)
#                newfield = 'magfake'
#                hedit(outfile,newfield,(mag),add+,show-,update+,verify-)
#                newfield = 'shftrate'
#                hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
#                newfield = 'shftangle'
#                outang = angle*57.29578
#                hedit(outfile,newfield,(outang),add+,show-,update+,verify-)

        }

#        print(" Deleting object file!")
#        delete(objfile, verify-)
        print(" ")
        print(" Done ")
        print(" Object file STILL EXISTS")
        print(" ")

end


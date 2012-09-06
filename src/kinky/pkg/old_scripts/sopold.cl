#  sop.cl    Son of Plant
#
#   Plants fake objects in N images at a given rate and direciton.
#  Accesses the image headers to get the exposure start time.
#
#  Input requirements: There must be N(=nimg) REGISTERED image named xxxxn
#      with xxxx being the common file name and 1 <= n <= N.
#
#  NB: mag=-1.2 below gives a R=23.0 source in a 480 sec scaled COSMIC frame
#      and thus mag=0 implies R=24.2
#

procedure sop(common,pseed,pobjects)
        string common {"", prompt=" Common portion of filename "}
        int pseed  {"", prompt=" seed number "}
        int pobjects  {"", prompt=" number of objects to generate"}

begin 
        int    nimg, cpflag, nobj
        real   rate,xsh0,ysh0,angle,outang
        real   xsh,ysh
        real   currtime, strttime
        real   xstart,ystart,mag,rnum
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
        print(" plants objects at given rate")
        print(" ") 
        rootword = common
	rseed = pseed
	nobj = pobjects
        print(" input # of images : ") 
        scan(nimg)
        print(" input object rate of motion on sky (pixels/hour) : ") 
        scan(rate)
        print(" input |angle| to ecliptic (degrees) : ") 
        scan(angle)
        angle = -angle/57.29578
#        print(" xcoord to start?  ") 
#        scan(xstart)
#        print(" ycoord to start?  ") 
#        scan(ystart)
#        print(" mag of object?  ") 
#        scan(mag)
        print(" Overwrite(1)? or make new copy with fk prefix (0)? ") 
        scan(cpflag)

        objfile=mktemp("Object.list")
	for (i=1; i<=nobj; i+=1) {
	 urand(1,1,ndigits=5,seed=rseed) | scan rnum
	 mag = rnum*1.70000 - 0.2
#	 real magnitude is mag+24.2
	 urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=2000.) | scan xstart
         urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
         urand(1,1,ndigits=5,seed=rseed,scale_factor=2000.) | scan ystart
         urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
         print(xstart,ystart,mag, >> objfile)
	}

        print(" ") 
        print(" Processing files: ")
        print(" ") 
        infile = rootword//1
        gettime(infile)
        strttime = gettime.outputime
        print(" **  Image 1 acquired at ", strttime, " UT") 

        xsh0 = rate*cos(angle)
        ysh0 = rate*sin(angle)

        for (i=1; i<=nimg; i+=1)
        {
                infile = rootword//i
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
                print(" * Image ",i," acquired at ", currtime, " UT") 

#  Compute offset, then add object
#  Note - signs (compare findkuiper) have been removed!
                ysh = ysh0*(currtime - strttime)
                xsh = xsh0*(currtime - strttime)
#               print(" xsh0 ",xsh0," and ysh0 ",ysh0) 
#               print(" xshift ",xsh," and yshift ",ysh) 
                
                mkobjects(outfile,objects=objfile,xoffset=xsh,yoffset=ysh, star="gaussian", radius=1.7, exptime=1., magzero=7.0)

#   Scale summed file by number of images and display final summed file
#
        
# Record shift rates in header
                newfield = 'xstart'
                hedit(outfile,newfield,(xstart),add+,show-,update+,verify-)
                newfield = 'ystart'
                hedit(outfile,newfield,(ystart),add+,show-,update+,verify-)
                newfield = 'xshift'
                hedit(outfile,newfield,(xsh),add+,show-,update+,verify-)
                newfield = 'yshift'
                hedit(outfile,newfield,(ysh),add+,show-,update+,verify-)
                newfield = 'magfake'
                hedit(outfile,newfield,(mag),add+,show-,update+,verify-)
                newfield = 'shftrate'
                hedit(outfile,newfield,(rate),add+,show-,update+,verify-)
                newfield = 'shftangle'
                outang = angle*57.29578
                hedit(outfile,newfield,(outang),add+,show-,update+,verify-)

        }

#        print(" Deleting object file!")
#        delete(objfile, verify-)
        print(" ")
        print(" Done ")
        print(" ")
        print(" Object file STILL EXISTS")

end


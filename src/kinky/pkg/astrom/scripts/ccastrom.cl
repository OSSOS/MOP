procedure ccastrom(solc,pbox,pimgfile,phdate,put,pexp,pdate,pname,pcode)

#  This program uses the computed plate solution to translate
#  pixel coordinates to RA and DEC.  You may wish to display other
#  comparison images (for blinking) before invoking this task.

string solc {"", prompt="file with plate solution for all images?"}
int    pbox     {"", prompt="box size for centroiding?"}
string pimgfile {"", prompt="Image to analyse?"}
string phdate   {"", prompt="Exposure date keyword? "}
string put      {"", prompt="Exposure start keyword? "}
string pexp     {"", prompt="Exposure length keyword?"}
string pdate    {"", prompt="Re-enter exposure date in (yyyy mm dd)?"}
string pname    {"", prompt="Object name? (10 CHAR MAX)"}
string pcode    {"", prompt="Observatory code? (xxx)"}

begin
    string now, coeffile, command, pointfile
    string imgname, centerfile, lab0, lab1, lab2, outfile
    real x1, y1, xcen, ycen, xkeep,ykeep
    real ra,dec,ut,exp,utcenter,dd,uttime
    string utkey,expkey,datestring,sdd,syy,blank
    string outstring,oname,ustring,stra,stdec,datekey,logfile
    int wcs, box, yy, len, jn, nbuf
    real hh,mm,ss
    string shh,smm,sss,ocode

	coeffile=solc
  	box = pbox
	utkey = put
	expkey = pexp
	pointfile=mktemp("tmp$pt")
	datekey = phdate
	print(" ********************************************** ")
	print(" * Obs. codes: 675(5m), 568(M.Kea), 695(KPNO) * ")
	print(" ********************************************** ")
	ocode = pcode

        print("------------- going into loop -----------")
        newimg:
	 imgname= pimgfile
	 print(" display image in which ximtool buffer (1-4)? ")
	 scan(nbuf)
	 display(imgname,nbuf)
           
	pnts:
           print("*************************************")
           print("  'q'=quit, 'h'=here, 'n'=new image, otherwise centroid on this point ")
           print(">>IMCURSOR<<")
	   now= fscan(imcur, x1, y1, wcs, command)
	   xkeep = x1
	   ykeep = y1
	   if (command == "q") bye
	   if (command == "n") goto newimg
           print(x1, y1, > pointfile)
	 if (command == "h") 
	 {
	   print(" \n *** NON-CENTROIDED ***********************************")
	   outfile=mktemp("tmp$out")
           cctran(pointfile,outfile,coeffile,"full.sol",forward+,xcol=1,ycol=2,lngformat="%12.2h",latformat="%11.1h")
           tvmark(nbuf, pointfile, mark="circle",radii=5, label=yes, color=206)
           delete(pointfile, verify-)
	   print(" ******************************************************")
         }
	 else
	 {
#          Don't need the coordinates written in pointfile, have x1,y1
           delete(pointfile, verify-)

	   print(" ")
#	   imgname= pimgfile
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
           tvmark(nbuf, pointfile, mark="circle",radii=9, label=yes, color=205)
           delete(pointfile, verify-)
         }
	 ;
#-----------------------------------------------------------------------

#        type(outfile)
        list = outfile
	now = fscan(list,ra,stdec)
#        print( ra," ",stdec )
        delete(outfile, verify-)
        imgets(imgname,utkey)
	ut = real(imgets.value)
        ut = ut / 3600.0 
#	print(utkey,"  ",ut)
        imgets(imgname,expkey)
 	exp = real(imgets.value)
#	print(expkey,"  ",exp)
	print(" IMAGE OBTAINED : ",ut," UT  for ",exp," sec.")
        utcenter = (   ut + (exp/(3600.0))/2.0   )/24.0
	if (utcenter < 0.0) print ("************** PANIC, UTCENTER < 0 ")
	if (utcenter > 1.0) {
 		print ("*** PANIC, UTCENTER>1 ; proceeding /3600")
		utcenter = (ut + exp/2.0)/(24.0*3600)
	}
#  utcenter now has UT fraction of day  of center.  
#  Get date as string in yyyy mm dd format
	imgets(imgname,datekey)
	print(datekey," ",imgets.value)
	datestring = pdate
        sdd = substr(datestring,9,10)
     	dd = real(sdd)
	uttime = dd + utcenter
#	print(datestring," ",uttime)
        blank= " "
# NAME ASSIGNED HERE.
	print("THANKS FOR TESTING LYNNE-----------REPORT")
getname:
	oname = pname
	len = strlen(oname)
        if (len > 10) goto getname
	if (len < 8) outstring = "     "
	if (len > 7) outstring = " "
	outstring = outstring//oname
	len = strlen(oname)
# pad to 12 characters for MPC format; note change on Oct 21/2004
	for(j=len+1;j<=12;j+=1) {
            outstring = outstring//blank
        }
        outstring = outstring//"C"
        outstring = outstring//substr(datestring,1,8)
	if (dd <9.999999999999) outstring = outstring//"0"
#	print(outstring)
        ustring = str(uttime)
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
#	print(outstring)
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
	j = stridx(":",stdec)
	hh = real( substr(stdec,1,j-1) )
#	print(' hh ',hh)
	if (hh > 0) outstring = outstring//" "
	if (hh < 9.999999999 && hh > -9.99999999) outstring = outstring//" "
	outstring = outstring//substr(stdec,1,j-1)
	outstring = outstring//" "
	outstring = outstring//substr(stdec,j+1,j+2)
	outstring = outstring//" "
	outstring = outstring//substr(stdec,j+4,j+7)
	for(j=1;j<=22;j+=1) {
            outstring = outstring//blank
        }
	outstring = outstring//substr(ocode,1,3)
	print(outstring)
	logfile=imgname//".coo.1"
	print(xkeep,ykeep," ",outstring,>>logfile)

	goto pnts
end

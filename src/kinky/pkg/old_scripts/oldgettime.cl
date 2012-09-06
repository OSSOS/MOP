#
#  gettime.cl  
#
#   gets the UT-START field and returns in decimal format
#
#

procedure gettime(imgname)

	string imgname {"", prompt=" filename "}
	real outputime

begin 
	int    nimg 
	string instring, starttime, workst
	string hh,mm,ss
	int    colon, len 
	real rhh,rmm,rss

	instring = 'UTIME'
#	instring = 'UT-START'
#	instring = 'UT'

#	print(" ")
#	print(" ") 
#	print(" Welcome to Gettime ")
#	print(" ") 

	imgets(imgname,instring) 
	starttime = imgets.value

#	print(" ") 
#	print(instring, " = ", starttime) 
	
# parse it up
	workst = starttime
	len = strlen(workst)
#	print("len   = ",len)
	colon = stridx(":",workst)
#	print("colon : ",colon)
	hh = substr(workst,1,colon-1)
#	print("  hh : ",hh)
	workst = substr(workst,colon+1,len)
#	print(workst)
	len = strlen(workst)
#	print("len   = ",len)
	colon = stridx(":",workst)
#	print("colon : ",colon)
	mm = substr(workst,1,colon-1)
#	print("  mm : ",mm)
	workst = substr(workst,colon+1,len)
	ss = workst
#	print("  ss : ",ss)

	rhh = real(hh)
	rmm = real(mm)
	rss = real(ss)
	rmm = rmm + rss/60.0
	rhh = rhh + rmm/60.0
#	print("TIME : ",rhh)
	
	outputime = rhh

#	print(" ")
#	print(" Done ")
#	print(" ")

end

	

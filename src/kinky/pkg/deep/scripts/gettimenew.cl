#
#  gettime.cl  
#
#  MODIFICATION HISTORY:
#  Mar 26/99 Petit: Observation time now read as header keyword (more flexible)
#
#   gets the specified HEADER KEYWORD field and returns in decimal format
#

procedure gettime(imgname,instring)

	string imgname {"", prompt=" filename "}
	string instring {"", prompt=" Header Keyword of observation time "}
	real outputime

begin 
	int    nimg
	string starttime, workst
	string hh, mm, ss, year, month, day, mjd
	int    colon, len, dash, iyear, imonth, iday
	real   rhh, rmm, rss, rtime

	cache("noao")
	cache("noao.astutil")
	cache("noao.astutil.asttimes")

	imgets(imgname,instring) 
	starttime = imgets.value

	if (instring == "MJD-OBS") {
	    rhh = real(starttime)
	    rhh = (rhh - 52100.0)*24.0	
	} else {
	    if (instring == "DATE-OBS") {
		workst = starttime
		len = strlen(workst)
		dash = stridx("-",workst)
		year = substr(workst,1,dash-1)
		workst = substr(workst,dash+1,len)
		dash = stridx("-",workst)
		month = substr(workst,1,dash-1)
		workst = substr(workst,dash+1,len)
		dash = stridx("T",workst)
		day = substr(workst,1,dash-1)
		workst = substr(workst,dash+1,len)
		colon = stridx(":",workst)
		hh = substr(workst,1,colon-1)
		workst = substr(workst,colon+1,len)
		len = strlen(workst)
		colon = stridx(":",workst)
		mm = substr(workst,1,colon-1)
		workst = substr(workst,colon+1,len)
		ss = workst
		iyear = int(year)
		imonth = int(month)
		iday = int(day)
		rhh = real(hh)
		rmm = real(mm)
		rss = real(ss)
		rmm = rmm + rss/60.0
		rtime = rhh + rmm/60.0
		asttimes(year=iyear,month=imonth,day=iday,time=rtime,obs="kpno",header-)
		mjd = asttimes.jd
		rhh = real(mjd)
		rhh = (rhh - 2452100.5)*24.0
	    } else {
		workst = starttime
		len = strlen(workst)
		colon = stridx(":",workst)
		hh = substr(workst,1,colon-1)
		workst = substr(workst,colon+1,len)
		len = strlen(workst)
		colon = stridx(":",workst)
		mm = substr(workst,1,colon-1)
		workst = substr(workst,colon+1,len)
		ss = workst

		rhh = real(hh)
		rmm = real(mm)
		rss = real(ss)
		rmm = rmm + rss/60.0
		rhh = rhh + rmm/60.0
	    }
	}
	outputime = rhh

end

	

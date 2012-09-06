

procedure nail (root)
	string root {"",prompt="Root image name"};

begin 
  string t_root
  string t_coo
  string frame
  string chip
  string x,y,spc,ra,dec,epoch
  real   time,day_frac
  string date,sdate,sday_frac

  t_root = root
  
  myload(t_root)
  t_root=t_root//"1.fits"
  hselect(t_root,"OBJECT",yes) | scan(frame);

  hselect(t_root,"IMAGEID",yes) | scan(chip);
  frame = frame//"c"//chip

  hselect(t_root,"MJDATE",yes) | scan(time);
  day_frac = time - int(time);
  printf("%7.6f\n",day_frac) | scan(sdate) ;
  sday_frac = substr(sdate,2,7)


  hselect(t_root,"DATE-OBS",yes) | scan(date);
  date = date//s1

  imexam(t_root ) | grep "world" | scan(spc, ra, dec ) 
  if ( nscan() == 3 )  { 
    ra = substr(ra,1,11)
    dec = substr(dec,1,10)
    printf("%12s  C%-16s %11s  %10s %24s\n" ,frame,date,ra,dec,"568",>> "MPC") 
  } else {
    printf("SKIPPING\n")
  }

end

#
#  search.cl  
# /********
#   a rip off of nuvies

#   */

procedure search(common,name,chip,pang) 
  
  string common {"", prompt=" Common portion of filename (med or sum)"}
  string name  {"", prompt=" user name"}
  int chip  {"", prompt=" chip number"}
  int    pang   {"", prompt=" common angle ? "}
  real   prate  {"", prompt=" first rate ? "}
  real   pdr    {1.0, prompt=" rate spacing ? "}
  int    plast  {4 , prompt=" last image ? "}
  int boxwidth {128, prompt=" size of box to display in movie"}
  string master {"Med32schr0", prompt="Master image name"}

  begin 
  	int    last , currframe,  angle, k ;
  	int    inta, intb, dum, tchip , nobj, o, sfile;
  	real   firstimg, rate , dr, realj , ddr;
  	string infile,medsum,angles,rates,objfile,ans, tname;
  	real	x[100],y[100],a[100],r,sx[100],sy[100],sa[100],mx,my,xm,ym;
  	int	x1,x2,y1,y2,maxx, dobj;
  	string  subsection, pcommon, toutfile, tsavefile, tvcmd, tvcmd2, tcommon, ocommon, sc[1000] ;
	string tmas, tim;
  	struct  comment;
  
# 	/* Assign the parameters now.. */ 	
  	pcommon = common ;

	if (pcommon == "med" ) {
		ocommon = "sum";
	} else {
		ocommon = "med";
	}
  	tname = name ;
  	tchip = chip ;
  	firstimg = prate ;
  	dr = pdr ;
  	last = plast ;
  	angle = pang;
  	set stdimage = imt4096
	tmas = master;
  
  	objfile = tname//".grid" ;
  	if ( !access(objfile) ) {
     	  error(1,"File, "//objfile//" doesn't exist?\n") ;
	}	
	#printf ("%s.c%02d\n",tname,tchip) | scan(toutfile)
	#tsavefile = tname//".check"
  	toutfile = tname//".list" ;
	list = objfile
	tvcmd = mktemp ("tmp$search");
	tvcmd2 = mktemp("tmp$search");
	# Read in all the possible object from the .list file 
	nobj=1;
	while ( fscan(list, xm, ym ) != EOF ) {
	   x[nobj] = xm;
	   y[nobj] = ym;
	   nobj += 1;
	}
	# Load existing objects from the chip file into memory
	o = 1 ;
	tsavefile = mktemp(toutfile);

	sfile = 1;
	savereset:
	if (access(toutfile)) {
		list = toutfile;
		sfile = fscan(list,xm)
		sfile = 1
		while (fscan(list,mx,my,i,comment) != EOF ) {
		     if ( nscan() == 3 ) comment = "";
		     if ( nscan() < 3 ) next; 
		     sx[sfile]= mx;
		     sy[sfile]= my;
		     sa[sfile]=i;
		     sc[sfile]=comment;
		     sfile += 1;
		}
		print ("I've slurped up "//toutfile//" and I am copying it to "//tsavefile//" for safekeeping");
		if (access(tsavefile)) {
			print ("Backup file "//tsavefile//" exists, I will overwrite it...");
			delete(tsavefile,verify-);
		}
		copy (toutfile,tsavefile);
	} else {
		print ( angle ,"## ****** Chip ",tchip," ", angle, "degrees  ",tname,  > toutfile) ;
	} 

	


	object:
	i = 0;
	while (yes) {
		# Don't loop outside the object list bounds.

		if ( o > nobj-1 ) o = nobj-1;
		if ( o < 1 ) o = 1;
		print ("Blinking object "//o//" of "//nobj-1);

		# constran the box to lie within the image space available
		x1 = (x[o] - boxwidth);
		if ( x1 < 1 ) x1 = 1 ;

		y1 = (y[o] - boxwidth) ;
		if ( y1 < 1 ) y1 = 1 ;

		# printf ("s%dp0a%dp0%s.fits\n",firstimg,angle,pcommon);
		printf ("s%dp0a%dp0%s.fits\n",firstimg,angle,pcommon) | scan(infile);
		if ( !access(infile) )  
			error(1,"Cann't access image "//infile);
		imgets (infile, "i_naxis1" )
		maxx = real(imgets.value)
		x2 = (x[o] + boxwidth)
		if ( x2 > maxx ) x2 = maxx
		if ( x[o] > maxx ) {
			print ("Object number "//o//" is outside the image bounds please remove it\n") 
			break;
		}


		imgets (infile, "i_naxis2" )
		maxx = real (imgets.value)
		y2 = (y[o] + boxwidth)
		if ( y[o] > maxx ) {
			print ("Object number "//o//" is outside the image bounds please remove it\n") 
			break;
		}

		if ( y2 > maxx ) y2 = maxx

        	subsection = "["//x1//":"//x2//","//y1//":"//y2//"]"	 ;
		medsum = pcommon//subsection ;
		tim = tmas//subsection;

		# Let's place a mark where the object is suppose to be.
		print ( x[o],y[o], > tvcmd )	 ;

		loop:

		ans = "m" ;
	    	for ( k=1; k <= 4 ; k+=1 ) {
			if ( i < firstimg - 1) { 
				i=firstimg - 1  ;
				beep ;
			}
			i += 1 ;
			if ( i > last ) {
				i = last ;
				beep ;	
			}	
			printf ("s%dp0a%dp0%s\n",i,angle,medsum) | scan(infile);
 			display(infile,k,fill-, >> "/dev/null") ;
	     	}
		while (yes) {
			if (access(tvcmd2) && ans == "d" ) {
				tvmark(4,tvcmd2,inter-,mark="cross",txsize=3,color=204);
				delete(tvcmd2,verify-);
			}
			if (access(tvcmd2) && (ans == "a" || ans == ":")) {
				tvmark(1,tvcmd2,inter-,mark="circle",color=205,radii=21);
				tvmark(2,tvcmd2,inter-,mark="circle",color=205,radii=21);
				tvmark(3,tvcmd2,inter-,mark="circle",color=205,radii=21);
				tvmark(4,tvcmd2,inter-,mark="circle",color=205,radii=21);
				delete(tvcmd2,verify-);
			}
	        	if ( ans == "m" ) {
		   		tvmark (2,toutfile,mark="circle",radi=21,color=208,label+,inter-,nxoffset=10,nyoffset=10) ;
		   		tvmark (4,toutfile,mark="circle",radi=21,color=208,label+,inter-,nxoffset=10,nyoffset=10) ;
		   		tvmark (3,toutfile,mark="circle",radi=21,color=208,label+,inter-,nxoffset=10,nyoffset=10) ;
		   		tvmark (1,toutfile,mark="circle",radi=21,color=208,label+,inter-,nxoffset=10,nyoffset=10) ;
		   
			}

			gflush ;
			if ( fscan(imcur, mx, my, z, ans, comment)  == EOF )  error(2,"IMCUR input failure?");
	                print (mx,my,z,ans,comment);
			
			if (strlen(ans)!=1) ans="?" ;
			
			switch (ans) {
				case 'M':
					display(tim,3,>>"/dev/null");
				case 'w': {
					print("Making a backup copy for you...\n");
					delete(tsavefile,verify+);
					copy(toutfile,tsavefile);
					}
				case 'm':
					# just keep on going;
					;
				case 'n': {
					break ;
					}
				case 'p': {
					i -= 8;
					break ;
					}
				case 'q':
					goto loop3 ;
				case ':','a': { 
					if ( ans == "a" ) comment = "";
					sx[sfile] = mx ;
					sy[sfile] = my ;
			   	 	sa[sfile] = i ;
		       	 		sc[sfile] = comment ;
					sfile += 1;
					if ( access(toutfile) ) delete(toutfile,verify-);
					if ( access(tvcmd2) ) delete(tvcmd2,verify-);
					print (mx,my, > tvcmd2);
					print ( angle ," -100 ****** Chip ",tchip," ", angle, "degrees  ",tname,  > toutfile)	
					for (j=1; j<sfile; j+=1 ) {
						printf ("%7.2f %7.2f %2d %s\n",sx[j],sy[j],sa[j],sc[j],>> toutfile) ;
					}
					}
#				case 's': {
##					tcommon = pcommon;
##					pcommon = ocommon;
##					ocommon = tcommon;
##					medsum = pcommon//subsection ;
##					i -= 4;
##					break;
##					}
				case 'd': {
					if ( sfile > 1 ) {
					ddr = (mx - sx[1])*(mx-sx[1]) + (my - sy[1])*(my - sy[1]);
					dobj = 1;
					for (j=2; j<sfile; j+=1 ) {
						dr = (mx-sx[j])*(mx-sx[j]) + (my - sy[j])*(my - sy[j]) ;
						if ( dr < ddr ) {
							ddr = dr;
							dobj = j;
						}
					}
					if ( ddr > 100 ) { 
						beep ;
						print ("No object within 10 pixels of delete\n");
						print ("please check cursor  position \n");
					} else {
						if ( access(tvcmd2) ) delete(tvcmd2,verify-);
						print (sx[dobj],sy[dobj], > tvcmd2);
						# replace dobj with the last object in the list and push the
						# counter up to allow new last member.
						sfile = sfile - 1;
						sx[dobj]=sx[sfile];
						sy[dobj]=sy[sfile];
						sc[dobj]=sc[sfile];
						sa[dobj]=sa[sfile];
						# Rebuild the access file ...
						if ( access(toutfile)) delete(toutfile,verify-);
						print ( angle ," -100 ****** Chip ",tchip," ", angle, "degrees  ",tname,  > toutfile)	
						for (j=1; j<sfile; j+=1 ) {
							printf ("%7.2f %7.2f %2d %s\n",sx[j],sy[j],sa[j],sc[j],>> toutfile)
						}
					}
					;
					}
					;
					}
				default: {
					printf ("\n") ;
					printf ("********************************************************************************\n");
					printf ("p - previous rate\t\t n - next rate\n");
					printf ("b - back one object\t\t f - forward one object\n") ;
					printf ("a - accept object \t\t d - delete object nearest cursor\n") ;
					printf (": - accept w/comment\t\t m - mark accepted objects\n");
					printf ("s - switch image type (med/sum)\t i - interupt without writing\n");
					printf ("w - update backup file\t\t q - end task, output is saved\n");
					printf ("********************************************************************************\n");
					}
			}
		    	gflush ;
		}
		delete (tvcmd, verify-, go_ahead+) ;
	}
	
	loop3:

	if (!access(toutfile)) 
	for (j=1; j<sfile; j+=1 ) {
		printf ("%7.2f %7.2f %2d %s\n",sx[j],sy[j],sa[j],sc[j],>> toutfile)
	}

	print ("That appears to be successfull, can I delete the backup file >"//tsavefile//"< ?");
	delete(tsavefile,verify+);

	print ("The End")

end



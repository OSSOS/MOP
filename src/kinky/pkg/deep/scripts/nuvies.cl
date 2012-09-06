#
#  nuvies.cl  
# /********
#   Loads median and summed images with common names sequentially
#   Only loading the part of the image near the current x,y cordinate
#   as read from the file fname.

#   After displaying the movie the user is prompted for choice of
#
#   replay the movie (press r)
#   skip this object, it is not real (press s)
#   record the objects position (as read from the current position of the
#   cursor in the display window (press :) NOTE: the ":" key place a ":"
#   prompt in the window where iraf is running... any text you type here is
#   saved after the x y location of the cursor (read when the ":" was pushed).
#   
#   Usage:  first search all the frames for objets (recording there x,y 
#   positions in a file , interactive tvmark is good for this.
#   then (after done searching all the images) run movies using the found
#   object list as your input.  Now movies will show you each object inturn
#   allowing the user to "check" the validity of previous selections.
#   Object that are in the "punative" list are blue circles.  Objects
#   that the user has alread accepted (via a ":" reponse) appear as pinks
#   squares... the current shift rate being displayed appears just off the 
#   part of the image being  displayed, in the upper right corner.  The
#   pink box is label with the first bit of text after the x,y coordinates 
#   taken from the "confirmed" file

#   */

procedure nuvies(common,name,chip,pang) 
  
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
  	int    last , currframe,  angle ;
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
  	#set stdimage = imt256 ;
	tmas = master;
  
  	objfile = tname//".list" ;
  	if ( !access(objfile) ) {
     	error(1,"File, "//objfile//" doesn't exist?\n") ;
	}	
	printf ("%s.c%02d\n",tname,tchip) | scan(toutfile)
	tsavefile = tname//".check"
	list = objfile
	tvcmd = mktemp ("tmp$nuvies");
	tvcmd2 = mktemp("tmp$nuvies");
	# Read in all the possible object from the .list file 
	nobj=1;
	xm = 0;
	ym = 0;
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
		print ( angle ," -100 ****** Chip ",tchip," ", angle, "degrees  ",tname,  > toutfile) ;
	} 


	object:
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
		i = firstimg ;
		ans = "m" ;
		while (yes) {
			if ( i < firstimg) { 
				i=firstimg  ;
				beep ;
			}
			if ( i > last ) {
				i = last ;
				beep ;	
			}	
			printf ("s%dp0a%dp0%s\n",i,angle,medsum) | scan(infile);
			#if ( !access(infile) ) 
			#	error(1,"Cannot access image >"//infile//"<");
			imgets (infile, "i_naxis1") ;
 			display(infile,4,fill-, >> "/dev/null") ;

			tvmark (4,tvcmd,inter-,mark="circle",radi=21,color=208);
			if (access(tvcmd2)) {
				tvmark(4,tvcmd2,inter-,mark="cross",txsize=3,color=204);
				delete(tvcmd2,verify-);
			}
	        	if ( ans == "m" ) {
		   		tvmark (4,toutfile,mark="rectangle",lengths=14,color=207,label+,inter-,nxoffset=10,nyoffset=10) ;
		   
			}

			gflush ;
			if ( fscan(imcur, mx, my, z, ans, comment)  == EOF )  error(2,"IMCUR input failure?");
			
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
				case 'n':
					i += 1 ;
				case 'q':
					goto loop3 ;
				case 'p':
					i -= 1;
				case ':','a': { 
					if ( ans == "a" ) comment = "";
					sx[sfile] = mx ;
					sy[sfile] = my ;
			   	 	sa[sfile] = i ;
		       	 		sc[sfile] = comment ;
					sfile += 1;
					ans = "m" ;
					if ( access(toutfile) ) delete(toutfile,verify-);
					print ( angle ," -100 ****** Chip ",tchip," ", angle, "degrees  ",tname,  > toutfile)	
					for (j=1; j<sfile; j+=1 ) {
						printf ("%7.2f %7.2f %2d %s\n",sx[j],sy[j],sa[j],sc[j],>> toutfile) ;
					}
					}
				case 's': {
					tcommon = pcommon;
					pcommon = ocommon;
					ocommon = tcommon;
					medsum = pcommon//subsection ;
					}
				case 'f': {
					o  += 1 ;
					break ;
					}
				case 'b': {
					o -= 1;
					break ;
					}
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
					ans = "m";
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



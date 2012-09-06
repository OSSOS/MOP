#
#  movies.cl  
#
#   Loads median and summed images with common names sequentially
#   Only loading the part of the image near the current x,y cordinate
#   as read from the file fname.

#   After displaying the movie the user is prompted for choice of
#
#   replay the movie (press r)
#   skip this object, it ain't real (press s)
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
#   

procedure movies(common,name,chip,pang)

	string common {"", prompt=" Common portion of filename (med or sum)"}
	string name  {"", prompt=" user name"}
	int chip  {"", prompt=" chip number"}
	real   prate  {"", prompt=" first rate ? "}
	real   pdr    {1.0, prompt=" rate spacing ? "}
	int    plast  {4 , prompt=" last image ? "}
	int    pang   {"", prompt=" common angle ? "}
	int boxwidth {50, prompt=" size of box to display in movie"}

begin
        int    last , currframe,  angle
        int    inta, intb, dum, tchip
        real   firstimg, rate , dr, realj
        string infile,medsum,angles,rates,objfile,ans, tname
	real	x,y,r
	int	x1,x2,y1,y2,maxx
	string  subsection, pcommon, toutfile, tsavefile, tvcmd
	struct  comment

	# Assign the parameters now...	
	pcommon = common
	tname = name
	tchip = chip
	firstimg = prate
	dr = pdr
	last = plast
	angle = pang
	set stdimage = imt256
	

	objfile = tname//".list"
	if ( !access(objfile) ) {
	   error(1,"File, "//objfile//" doesn't exist?\n");
	   ;
	}
	printf ("%s.c%02d\n",tname,tchip) | scan(toutfile)
	tsavefile = tname//".check"
	list = objfile
	tvcmd = mktemp ("tmp$fkbo")

	if (!access(toutfile))
	  print ( angle ," -100 ****** Chip ",tchip," ", angle, "degrees  ",tname,  > toutfile)
	
	while ( fscan( list, x, y) != EOF ) {
	inta = firstimg
	infile =  's'//inta//'p0a'//angle//'p0'//pcommon

	x1 = (x - boxwidth)
	if ( x1 < 1 ) x1 = 1
	imgets (infile, "i_naxis1" )
	maxx = real(imgets.value)
	x2 = (x + boxwidth)
	if ( x2 > maxx ) x2 = maxx

	y1 = (y - boxwidth)
	if ( y1 < 1 ) y1 = 1
	imgets (infile, "i_naxis2" )
	maxx = real (imgets.value)
	y2 = (y + boxwidth)
	if ( y2 > maxx ) y2 = maxx

        subsection = "["//x1//":"//x2//","//y1//":"//y2//"]"	
	medsum = pcommon//subsection
	print ( x,y,"401 c",> tvcmd )	


	loop:
	i = firstimg
	ans = "m"
	while (yes)
	{
		if ( i < firstimg) { 
			i=firstimg
			beep
			;
		}
		if ( i > last ) {
			i = last
			beep
			;
		}
		rate = i
	        inta = (rate + 0.0001)
       		realj = ( (rate-inta)*10 + 0.0001 )
        	intb  = realj
		infile =  's'//inta//'p'//intb//'a'//angle//'p0'//medsum
		imgets (infile, "i_naxis1")
 		display(infile,4,fill-)

		tvmark (4,"",command=tvcmd,inter-,mark="circle",radi=15,color=208)

	        if ( ans == "m" ) {
		   tvmark (4,toutfile,mark="rectangle",lengths=14,color=207,label+,inter-,nxoffset=10,nyoffset=10)
		   ;
		}

		gflush
		if ( fscan(imcur, x, y, z, ans, comment)  != EOF ) {
		   if ( ans == "n" ) {  
			i += 1
			;
		   } else if ( ans == "q" ) {
			goto loop3
			;
		   } else if ( ans == "p" ) {
			i -= 1
			;
		   } else if ( ans == ":" ) {
		        printf ("%7.2f %7.2f %2d %s\n",x,y,rate,comment,>> toutfile)
			ans = "m"
			;
		   } else if ( ans == "?" ) {
			printf ("\f")
			printf ("p - previous rate\t c - continue to next object\n")
			printf ("n - next rate\t\t a - add object to confirmed\n")
			printf ("m - mark accepted objects on this image\n")
			printf (": - accept w/comment\t q - end this task\n")
			printf ("s - save object for checking later\n")
		   } else if ( ans == "a" ) {
			printf ("%7.2f %7.2f %2d\n", x , y , rate , >> toutfile)
			ans = "m"
			;
	           } else if ( ans == "s") {
		        printf  ("%7.2f %7.2f \n", x , y , >> tsavefile)
			;
		   } else if ( ans == "c") {
			break
			;
		   }
		   ;
		 }
		gflush
	}

	delete (tvcmd, verify-, go_ahead+)
	;

	}

	

	loop3:
	print ("The End")

end

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

procedure movies(common,fname)

	string common {"", prompt=" Common portion of filename (med or sum)"}
	string fname  {"", prompt=" file with candidates x,y "}
	string outfile  {"", prompt=" output file for confirmed objects "}
	real   prate  {"", prompt=" first rate ? "}
	real   pdr    {1.0, prompt=" rate spacing ? "}
	int    pang   {"", prompt=" common angle ? "}
	int    pnimg  {4 , prompt=" number of images ? "}
	int boxwidth {50, prompt=" size of box to display in movie"}

begin
        int    nimg , currframe,  angle
        int    inta, intb, dum
        real   firstimg, rate , dr, realj
        string infile,medsum,angles,rates,objfile,ans
	real	x,y
	int	x1,x2,y1,y2,maxx
	string  subsection, pcommon, toutfile, tvcmd, tvcmd2
	struct  comment
	
	pcommon = common
	objfile = fname
	toutfile = outfile
	list = objfile
	firstimg = prate
	dr = pdr
	angle = pang
	nimg = pnimg
	tvcmd = mktemp ("tmp$fkbo")
	tvcmd2 = mktemp ("tmp$fkbo")

	print ( "   0   ","   0    ","  ",angle,"  JJ Kavelaars", > toutfile)
	
	while ( fscan( list, x , y) != EOF ) {
	inta = firstimg
	infile =  's'//inta//'p0a'//angle//'p0'//pcommon

	x1 = (x - boxwidth)
	if ( x1 < 1 ) x1 = 1

	imgets (infile, "i_naxis1" )
	maxx = real(imgets.value)
	x2 = (x + boxwidth)
	if ( x2 > maxx ) x2 = maxx

	y1 = (y - boxwidth)
	if ( y1 < 1 ) x1 = 1

	imgets (infile, "i_naxis2" )
	maxx = real (imgets.value)
	y2 = (y + boxwidth)
	if ( y1 > maxx ) y2 = maxx

        subsection = "["//x1//":"//x2//","//y1//":"//y2//"]"	
	medsum = pcommon//subsection
	print ( x,y,"401 c",> tvcmd )

	loop:
	for (i=1; i<=nimg; i+=1)
	{
		rate = firstimg + (i-1)*dr
	        inta = (rate + 0.0001)
       		realj = ( (rate-inta)*10 + 0.0001 )
        	intb  = realj
		infile =  's'//inta//'p'//intb//'a'//angle//'p0'//medsum
		imgets (infile, "i_naxis1")
 		display(infile,4,fill-)
		print ( x2,y2,"101  :text ", inta, > tvcmd2)

		tvmark (4,"",command=tvcmd,inter-,mark="circle",radi=15,color=208)
		tvmark (4,"",command=tvcmd2,inter-,mark="circle",radi=15,color=208)
		delete (tvcmd2, verify-)
		tvmark (4,toutfile,mark="rectangle",label+,length=15,inter-,color=209,nxoffset=10,nyoffset=10)
	}

	print ("Replay the movie (r), record object (:angle), skip (s) ")
	readcursor:
	gflush
	while ( fscan(imcur, x, y, z, ans, comment ) < 4 ) 
	  print ( x , y , commet)

	  if ( ans == "r" )  
	     goto loop

	  if (ans == ":" ) 
	     print ( x , y , comment , >> toutfile)

	  if (ans != "s" )
	     goto readcursor

	delete (tvcmd, verify-, go_ahead+)
	;
	}
end


#
#  check.cl  
#
#   Reads coordinates from output of moving.f and display the images
#   for blinking.
#   Only loads the part of the image near the current x,y cordinate
#   as read from the file.

#   After displaying the images the user is prompted for choice of
#
#   skip this object, it ain't real (press s)
#   record the objects position as read from the file (press a)
#   quit the program (press q).
#   
#   Usage: create a liste of files to search (file-list) and the
#   parameter files for other programs. Run "detect", "match-stars"
#   and "moving" (parameter files are named "detect-param",
#   "match-param" and "moving-param"). Now "check" will show each
#   object in turn, allowing the user to "check" the validity of the
#   detection.
#
# MODIFICATIONS
#
#  15 Nov 2000: BG: saved already seen (and approved) objects
#

procedure check(pfilelist)

	string pfilelist {"file-list", prompt="file list"}
	int pboxw {128, prompt="box width"}

begin
        string filelist, imfile[5], catfile, dumfile, image
	string tvcmd, subsection, ans, outfile, astfile
	real   xc[5], yc[5], xof[5], yof[5]
	real   x, y, z, xcut, ycut
# BG
	int    nseen

	int     x1[4], y1[4], x2[4], y2[4]
	int    dx1, dx2, dy1, dy2, maxx, maxy
	int    i, j, k, nf, boxwidth, in[4], stat
	struct comment

	printf ("? - display this message\n")
	printf ("s - skip to next object\n")
	printf ("r - skip to next object\n")
	printf ("a - accept this object\n")
	printf ("q - end this task\n")

	# Assign the parameters now...	
	filelist = pfilelist
	boxwidth = pboxw
	set stdimage = imt256
	if (boxwidth > 128) {
	  set stdimage = imt512
	}
	boxwidth = boxwidth/2

	if ( !access(filelist) ) {
	  error(1,"File, "//filelist//" doesn't exist?\n");
	  ;
	}

	nf = 0
	list = filelist
	while (fscan(list, dumfile) != EOF) {
	  nf += 1
	  imfile[nf] = dumfile
	  if (nf == 5) {
	    break
	    ;
	  }
	}

	outfile = imfile[1]//".real"
	if (access(outfile)) {
	  delete(outfile, verify-, go_ahead+)
	}

	astfile = "hans.ast"
	if (access(astfile)) {
	  delete(astfile, verify-, go_ahead+)
	}

	catfile = imfile[1]//".shifts"
	if ( !access(catfile) ) {
	  error(1,"File, "//catfile//" doesn't exist?\n");
	  ;
	}
	xcut = 0.
	ycut = 0.
	list = catfile
	for (k=1; k<=nf; k+=1) {
	  stat = fscan(list, x, y)
	  if (k <= 3) {
	    if (x > xcut) { xcut = x }
	    if (y > ycut) { ycut = y }
	  }
	  xof[k] = -x
	  yof[k] = -y
	}

	catfile = imfile[1]//".moving"
	list = catfile

	imgets(imfile[1], "i_naxis1")
	maxx = real(imgets.value)
	imgets(imfile[1], "i_naxis2")
	maxy = real(imgets.value)

	tvcmd = mktemp("tmp$fkbo")

	nseen = 0

	while (fscan(list, dumfile) != EOF) {
	  k = 0
	  dx1 = 0
	  dx2 = 0
	  dy1 = 0
	  dy2 = 0
	  for (j=1; j<=3; j+=1) {
	    k += 1
	    stat = fscan(list, x, y)
	    xc[k] = x + xof[k]
	    yc[k] = y + yof[k]
	    in[j] = k
	    while (x <= 0.0) {
	      k += 1
	      stat = fscan(list, x, y)
	      xc[k] = x + xof[k]
	      yc[k] = y + yof[k]
	      in[j] = k
	    }
	    x1[j] = xc[1] + xof[in[j]] - boxwidth
	    if (x1[j]-1 < dx1) dx1 = x1[j] - 1
	    y1[j] = yc[1] + yof[in[j]] - boxwidth
	    if (y1[j]-1 < dy1) dy1 = y1[j] - 1
	    x2[j] = xc[1] + xof[in[j]] + boxwidth
	    if (x2[j]-maxx > dx2) dx2 = x2[j] - maxx
	    y2[j] = yc[1] + yof[in[j]] + boxwidth
	    if (y2[j]-maxy > dy2) dy2 = y2[j] - maxy
	  }

	  for (j=1; j<=3; j+=1) {
	    x1[j] = x1[j] - dx1
	    x2[j] = x2[j] - dx2
	    y1[j] = y1[j] - dy1
	    y2[j] = y2[j] - dy2
	    subsection ="["//x1[j]//":"//x2[j]//","//y1[j]//":"//y2[j]//"]"
	    image = imfile[in[j]]//subsection
	    display(image,j,fill-)
	    print(xc[in[j]], yc[in[j]], > tvcmd)
	    tvmark(j,tvcmd,inter-,mark="circle",radi=15,color=204)
	    delete (tvcmd, verify-, go_ahead+)
	  }
# BG
	  if(nseen > 0) {
            tvmark(1,outfile,inter-,mark="rectangle",lengths="5.0",color=205)
          }
	  printf ("* >>IMCURSOR<< *\n")

	  loop:
	  while (yes)
	  {
	    gflush
	    if (fscan(imcur, x, y, z, ans, comment) != EOF) {
	      if ( ans == "s" || ans == "r" ) {  
	        break
		;
	      } else if ( ans == "q" ) {
	        goto loop3
	        ;
	      } else if ( ans == "?" ) {
	        printf ("\f")
		printf ("? - display this message\n")
		printf ("s - skip to next object\n")
		printf ("a - accept this object\n")
		printf ("q - end this task\n")
		;
	      } else if ( ans == "a" ) {
# BG
	        nseen = nseen + 1
	        for (j=1; j<=3; j+=1) {
	          printf("%7.2f %7.2f ", xc[in[j]], yc[in[j]], >> outfile)
		  printf("%7.2f %7.2f\n", xc[j]-xof[j]-xcut, yc[j]-yof[j]-ycut, >> astfile)
		}
		printf("\n", >> outfile)
		break
		;
	      }
	      ;
	    }
	    gflush
	  }

	  ;

	}

	loop3:
	print ("The End")

end

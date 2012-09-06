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

procedure check(pcandfile)

	string pcandfile {"", prompt="candidate file"}
	int pboxw {254, prompt="box width"}

begin
        string candfile, imfile[5], dumfile, image, obsts
	string tvcmd, subsection, ans, outfile, camera
	real   xof[5], yof[5]
	real   x, y, z, xcut, ycut, x0, y0, fwhm, maxcount
	real   ax[5], ay[5], ax0[5], ay0[5], aflux[5], aarea[5]
	real   aint[5], ael[5]
	real   flux, area, intens, elong, obst, expt, ra, dec
# BG
	int    nseen

	int    x1[4], y1[4], x2[4], y2[4]
	int    dx1, dx2, dy1, dy2, maxx, maxy
	int    i, j, k, nf, boxwidth, in[4], stat, expid
        int    fkflag
 	int 	ncan
	string disimage
	struct comment

	printf ("? - display this message\n")
	printf ("f - strip fk from image names\n")
	printf ("s - skip to next object\n")
	printf ("r - skip to next object\n")
	printf ("a - accept this object\n")
	printf ("q - end this task\n")

	fkflag=0
#	fkflag=1

	# Assign the parameters now...	
	candfile = pcandfile
	boxwidth = pboxw
	boxwidth = boxwidth/2

	outfile = candfile//".real"
	if (access(outfile)) {
	  delete(outfile, verify-, go_ahead+)
	}

	nf = 3
	list = candfile
	for (k=1; k<=nf; k+=1) {
	  stat = fscanf(list, "# %s", dumfile)
	  imfile[k] = dumfile
	  printf("# %s\n", dumfile, >> outfile)
	}
	for (k=1; k<=6*nf+3; k+=1) {
	  stat = fscan(list, line)
	  print(line, >> outfile)
	}	

	imgets(imfile[1], "i_naxis1")
	maxx = real(imgets.value)
	imgets(imfile[1], "i_naxis2")
	maxy = real(imgets.value)

#

	nseen = 0

	ncan = 0
	while (fscan(list, dumfile) != EOF) {
	  ncan = ncan +1
	  xcut = 0.
	  ycut = 0.
	  for (j=1; j<=nf; j+=1) {
	    stat = fscan(list, x, y, x0, y0, flux, area, intens, elong)
	    ax[j] = x
	    ay[j] = y
	    ax0[j] = x0
	    ay0[j] = y0
	    aflux[j] = flux
	    aarea[j] = area
	    aint[j] = intens
	    ael[j] = elong
	    x = x0 - x
	    y = y0 - y
	    if (x > xcut) { xcut = x }
	    if (y > ycut) { ycut = y }
	    xof[j] = -x
	    yof[j] = -y	  
	  }
	  k = 0
	  dx1 = 0
	  dx2 = 0
	  dy1 = 0
	  dy2 = 0
	  for (j=1; j<=3; j+=1) {
	    k += 1
	    in[j] = k
	    while (ax[k] <= 0.0) {
	      k += 1
	      in[j] = k
	    }
	    x1[j] = ax[1] + xof[in[j]] - boxwidth
	    if (x1[j]-1 < dx1) dx1 = x1[j] - 1
	    y1[j] = ay[1] + yof[in[j]] - boxwidth
	    if (y1[j]-1 < dy1) dy1 = y1[j] - 1
	    x2[j] = ax[1] + xof[in[j]] + boxwidth
	    if (x2[j]-maxx > dx2) dx2 = x2[j] - maxx
	    y2[j] = ay[1] + yof[in[j]] + boxwidth
	    if (y2[j]-maxy > dy2) dy2 = y2[j] - maxy
	  }
	  redisplay:	
	  for (j=1; j<=3; j+=1) {
	    x1[j] = x1[j] - dx1
	    x2[j] = x2[j] - dx2
	    y1[j] = y1[j] - dy1
	    y2[j] = y2[j] - dy2
	    disimage=imfile[in[j]]
	    if ( stridx("fk",imfile[in[j]])!=0 && fkflag==1 )  {
  	         disimage=substr(imfile[in[j]],3,strlen(imfile[in[j]]))
	    } 
	    subsection ="["//x1[j]//":"//x2[j]//","//y1[j]//":"//y2[j]//"]"
	    image = disimage//subsection
	    display(image,j,fill-)
#	    imcopy(image,disimage//"_"//ncan)
	    tvcmd = mktemp("tmp$fkbo")
	    print(ax[in[j]], ay[in[j]], > tvcmd)
	    tvmark(j,tvcmd,inter-,mark="circle",radi=15,color=204)
	    delete (tvcmd, verify-, go_ahead+)
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

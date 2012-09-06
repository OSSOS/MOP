#
#  scale.cl
#
#  Written: 26 Mar 1999
#
# (Evolution of nosky.cl)
#
#  New version 12 July 2004 JMP.
#

procedure jmpscale(imlist,dmaglist)

	string imlist {"", prompt=" Image name list (with background) "}
	string dmaglist {"", prompt=" file with magnitude differences "}
	string newprefix {"", prompt=" Prefix for new files "}

	string *images
	string *diffmag

begin 
	int    nimg, i
	string infile, outfile, finfile, t_imlist, t_dmaglist
	string dummy, t_prefix
	real   sky, scale
	real   dmag, a, b, c, d, e, f

	print(" ")
	print(" ") 
	print(" Scalejmp PROGRAM -- Subtracts zero-order sky and scales to")
	print("                  all frames to constant flux for ref star.")
	print(" ") 

	t_imlist = imlist;
  	if ( !access(t_imlist) ) {
     	  error(1,"File, "//t_imlist//" doesn't exist?\n") ;
	}	
	t_dmaglist = dmaglist;
  	if ( !access(t_dmaglist) ) {
     	  error(1,"File, "//t_dmaglist//" doesn't exist?\n") ;
	}	
	t_prefix = newprefix;
	if ( t_prefix == "" ) {
	  t_prefix = "sc"
	  print ("WARNIG: using ", t_prefix, " prefix for output files!")
	}

	images = t_imlist;
	diffmag = t_dmaglist;

	while ( fscan(images, infile, sky) != EOF ) { 
          dummy = fscan(diffmag, a, b, c, d, e, f, dmag)
	  scale = 10**(-dmag/2.5)
	  outfile = mktemp("nsj");
	  print("       Currently working on: ",infile," into ",outfile)
          imarith (infile, "-", sky, outfile)
	  print ( "subtracted ", sky," from ", infile)

	  finfile = t_prefix//infile
          imarith (outfile, "*", scale, finfile)
	  imdel(outfile, verify-)
	  print ( "multiplied ", outfile, " by ", scale, "--> ", finfile)
#
	}
	print(" Done ")
	print(" ")

end

	

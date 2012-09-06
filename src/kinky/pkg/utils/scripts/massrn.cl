#
#  massrn.cl  
#
#   renames N images with common filenames to another rootname.
#  Has options to copy or rename.  Can flip x axis OR (x and y) axes also.
#  Allows optional ending
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and 1 <= n <= N.

procedure massrn(common,first,last)

	string common {"", prompt=" Common portion of old filename "}
	int    first  {"", prompt=" Number of the first image to renumber"}
	int    last   {"", prompt=" Number of the last image to renumber"}
	string suffix {"", prompt=" Common end portion of the old file"}
	string newcom {"", prompt=" Common portion of new filename "}
	int    new    {"", prompt=" New starting number"}
	bool   copy   {no, prompt=" Make a new copy"}
	bool   xflip  {no, prompt=" Flip in the x-direction"}
	bool   yflip  {no, prompt=" Flip in the y-direction"}

begin 
	int    nimg,i,oldn,oldfirst,oldlast
	bool   flag,yinvert,xinvert, binvert
	int    newfirst,newnum
	string rootword, newroot
	string flip, infile, outfile,flipx,flipy,flipboth,ending
	string cpstring

# set up the flip variables
	flipx = "[-*,*]"
	flipy = "[*,-*]"
	flipboth = "[-*,-*]"
	flip = "[*,*]"

# get the name of the images to movem
	rootword = common
	ending = suffix
	oldfirst = first
	oldlast = last

# the new files will be called newcom#
	newroot  = newcom
	newfirst = new


# setup flip to correctly flip during a copy... flips are always copies
        if ( xflip ) {
	  flip=flipx
	}
	if ( yflip ) {
	   flip=flipy
	}
	if ( yflip && xflip ) {
	    flip=flipboth
	}
	if ((xflip || yflip) && ( !copy)) {
	   error(1,"To flip you must request copy also\n")
	}

# loop over the input files... renaming/copying to the new names
	for (i=oldfirst; i<=oldlast; i+=1) {
	  infile  = rootword//i//ending
	  outfile =  newroot//newfirst
  	  if ( imaccess(infile) ) {
	    if(!copy) {
	       imrename(infile,outfile)
	    } else {
	       infile = infile//flip
	       imcopy(infile,outfile)
	    }
#	INCREMENT THE NEW COUNTER HERE.....  
	    newfirst = newfirst + 1
	  }
	  ;
	}

#	print(" ")
#	print(" Done ")
#	print(" ")

end

	

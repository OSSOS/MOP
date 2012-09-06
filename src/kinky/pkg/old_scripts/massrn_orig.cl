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

procedure massrn(common,newcom)

	string common {"", prompt=" Common portion of old filename "}
	string newcom {"", prompt=" Common portion of new filename "}

begin 
	int    nimg,i,oldn,oldfirst, flag, xinvert, binvert
	int    newfirst
	string rootword, newroot
	string infile, outfile,flip,flipboth,ending
	string cpstring

	flip =0
	binvert = 0

	flip = "[-*,*]"
	flipboth = "[-*,-*]"
	print(" ")
	print(" ") 
	print(" Welcome to MASS ReName ")
	print(" ") 
	rootword = common
	newroot  = newcom
	print(" Input option ending, or null string, of old filename ") 
	scan(ending)
	print(" input # of images : ") 
	scan(nimg)
	print(" input first # of OLD image set: ") 
	scan(oldfirst)
	print(" input first # of NEW image set: ") 
	scan(newfirst)
        flag = 1
        print(" Rename:1 or Copy:0 ?  (flipping only done if copying)") 
        scan(flag)
	if (flag != 0) goto proceed
        print(" flip x? 1 or not? 0   (use 0 if want to flip BOTH)") 
        scan(xinvert)
	if (xinvert != 0) goto proceed
        print(" flip both x and y? 1 or not? 0 ") 
        scan(binvert)
	if(xinvert*binvert > 0.5) print("PANIC - CAN'T HAVE BOTH FLAGS ON!")

   proceed:
	print(" ") 
	print(" Renaming images: ")
	print(" ") 
#
# Rename images one by one 
#
		
	for (i=newfirst; i<=nimg+newfirst-1; i+=1)
	{
		oldn = oldfirst - newfirst + i 
		infile  = rootword//oldn//ending
		outfile =  newroot//i 
		if(flag == 1)
		{
			print(" Renaming ", infile, " to ", outfile)
			imrename(infile,outfile)
		}
		else 
		{
		   if(xinvert == 1)
		   {
			infile = infile//flip
			print(" copying ", infile, " to ", outfile)
			imcopy(infile,outfile)
		   }
		   else
		   {
			if(binvert == 1)
			{
			  infile = infile//flipboth
			  print(" rotating ", infile, " by 180 to ", outfile)
			  imcopy(infile,outfile)
# ROTATE COMMAND LESS EXACT.
#			  rotate(infile,outfile,180.0000000)
		   	}
		        else
		        {
			  print(" copying ", infile, " to ", outfile)
			  imcopy(infile,outfile)
		   	}
		   	;
		   }
		   ;
		}
		;
	}

	print(" ")
	print(" Done ")
	print(" ")

end

	

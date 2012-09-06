#
#  massrn.cl  
#
#   renames N images with common filenames to another rootname.
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and 1 <= n <= N.

procedure massrn(common,newcom)

	string common {"", prompt=" Common portion of old filename "}
	string newcom {"", prompt=" Common portion of new filename "}

begin 
	int    nimg,i,oldn,oldfirst, flag, invert
	int    newfirst
	string rootword, newroot
	string infile, outfile,flip
	string sect

	flip = "[-*,*]"
	sect  = "[1355:1100,450:705]"
#	sect  = "[-650:-905,450:705]"
	print(" ")
	print(" ") 
	print(" Welcome to MASS ReName ")
	print(" ") 
	rootword = common
	newroot  = newcom
	print(" input # of images : ") 
	scan(nimg)
	print(" input first # of OLD image set: ") 
	scan(oldfirst)
	print(" input first # of NEW image set: ") 
	scan(newfirst)
        flag = 1
        print(" Rename:1 or Copy:0 ? ") 
        scan(flag)
        print(" flip?:1 or not?:0 ? ") 
        scan(invert)

	print(" ") 
	print(" Renaming images: ")
	print(" ") 
#
# Rename images one by one 
#
		
	for (i=newfirst; i<=nimg+newfirst-1; i+=1)
	{
		oldn = oldfirst - newfirst + i 
		infile  = rootword//oldn//sect
		outfile =  newroot//i 
		if(flag == 1)
		{
			print(" Renaming ", infile, " to ", outfile)
			imrename(infile,outfile)
		}
		else 
		{
		   if(invert == 1)
		   {
			infile = infile//flip
			print(" copying ", infile, " to ", outfile)
			imcopy(infile,outfile)
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

	print(" ")
	print(" Done ")
	print(" ")

end

	

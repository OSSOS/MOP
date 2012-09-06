#
#  myload.cl  
#
#   Loads images with common names
#
#  Fill option commented out Mar 10/98

procedure myload(common)

	string common {"", prompt=" Common portion of filename "}

begin 
	int    nimg , firstimg, currframe, fllflg
	string infile,rootword

	print(" ")
	print(" ") 
	print(" Welcome to MY LOADing program ")
	print(" ") 
	rootword = common
	print(" first image to load : ") 
	scan(firstimg)
	print(" # of images to load : ") 
	scan(nimg)
	fllflg = 0
#	print(" Fill ? (1-yes 0-no) : ") 
#	scan(fllflg)
# BG commented out (I NEVER used this!)
	fllflg=0

	print(" ") 
	print(" Loading files: ")
	print(" ") 
	for (i=firstimg; i<=nimg+firstimg-1; i+=1)
	{
		currframe = i - firstimg + 1
		infile = rootword//i
		print(" Currently loading ",infile," into frame ", currframe)

		if(currframe < 4) 
		  if(fllflg == 1) 
			display(infile,currframe,fill+) 
		  else
			display(infile,currframe,fill-)
 		else
		  if(fllflg == 1) 
			display(infile,4,fill+) 
		  else
 			display(infile,4,fill-)
	}

end

	

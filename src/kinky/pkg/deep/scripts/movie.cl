#
#  movie.cl  
#
#   Loads images with common names sequentially

procedure movie(common)

	string common {"", prompt=" Common portion of filename "}
	string subsec {"", prompt=" subsection of file? "}

begin 
	int    nimg , firstimg, currframe, fllflg
	string infile,rootword,ss

	print(" ")
	print(" ") 
	print(" Welcome to the MOVIE program ")
	print(" All will be displayed in frame 1 ")
	print(" ") 
	rootword = common
	ss = subsec
	print(" first image to load : ") 
	scan(firstimg)
	print(" # of images to load : ") 
	scan(nimg)
	fllflg = 0
	print(" Fill ? (1-yes 0-no) : ") 
	scan(fllflg)

	print(" ") 
	print(" Loading files: ")
	print(" ") 
	for (i=firstimg; i<=nimg+firstimg-1; i+=1)
	{
		currframe = 1
		infile = rootword//i//ss
		print(" Currently loading ",infile," into frame 1")

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

	

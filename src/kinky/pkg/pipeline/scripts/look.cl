#
#  look.cl  
#
#   Loads output of Hans' blink3.f code for checking
#

procedure look(common)
        string common {"", prompt=" Common portion of filename "}


begin 
	int    nimg , firstimg, currframe
	string infile,rootword,intable
	real   xtv, ytv

	print(" ")
	print(" ") 
	print(" Welcome to LOOK ")
	print(" ") 
	rootword = 'frame'
	print(" first image to load is 1 ") 
#	scan(firstimg)
	firstimg = 1
	print(" 3 images to load : ") 
#	scan(nimg)
	nimg = 3

	print(" ") 
	print(" Loading files: ")
	print(" ") 
	for (i=firstimg; i<=nimg+firstimg-1; i+=1)
	{
		currframe = i - firstimg + 1
		infile = rootword//i
		print(" Currently loading ",infile," into frame ", currframe)

		if(currframe < 4) 
			display(infile,currframe,fill-)
 		else
 			display(infile,4,fill-)

	        intable = 'tv.tmp'//i
	        print(" intable  ",intable) 
		tvmark(i,intable,mark='circle',radii=10,color=205)
	}

end

	

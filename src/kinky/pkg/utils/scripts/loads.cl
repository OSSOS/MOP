#
#  loads.cl  
#
#   Loads shifted images with common names

procedure loads(common,prate,pdr,pang,pnimg)

	string common {"", prompt=" med or sum ? "}
	real   prate  {"", prompt=" first rate ? "}
	real   pdr    {1.0, prompt=" rate spacing ? "}
	int    pang   {"", prompt=" common angle ? "}
	int    pnimg  {4 , prompt=" number of images ? "}

begin 
	int    nimg , currframe, fllflg, angle
	int    inta, intb
	real   firstimg, rate , dr, realj
	string infile,medsum,angles,rates

	print(" ")
	print(" ") 
	print(" Shifted loading program ")
	print(" REQUIRES integer angles -- sorry... ")
	print(" REQUIRES rates to be multipe of 0.1 pix/hr -- sorry... ")
	print(" ") 

	medsum = common
	firstimg = prate
	dr = pdr
	angle = pang
	nimg = pnimg

#	print(" rate spacing? (pix/hr) ")
#	scan(dr)

#	print(" first image rate to load : ") 
#	scan(firstimg)
#	print(" common angle? : ") 
#	scan(angle)
#	print(" # of images to load : ") 
#	scan(nimg)

	rate = firstimg
	fllflg = 0

	print(" ") 
	print(" Loading files: ")
	print(" ") 
#	for (i=firstimg; i<=nimg+firstimg-1; i+=1)
	for (i=1; i<=nimg; i+=1)
	{
		rate = firstimg + (i-1)*dr
	        inta = (rate + 0.0001)
       		realj = ( (rate-inta)*10 + 0.0001 )
        	intb  = realj
		infile =  's'//inta//'p'//intb//'a'//angle//'p0'//medsum
		print(" Currently loading ",infile," into frame ", i)

#		currframe = i - firstimg + 1
#		infile = 's'//i//'p0a'//angle//'p0'//medsum
#		print(" Currently loading ",infile," into frame ", currframe)


		if(i < 4) 
		  if(fllflg == 1) 
			display(infile,i,fill+) 
		  else
			display(infile,i,fill-)
 		else
		  if(fllflg == 1) 
			display(infile,4,fill+) 
		  else
 			display(infile,4,fill-)
	}

end

	

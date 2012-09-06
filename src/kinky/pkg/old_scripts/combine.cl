procedure combine

string image {"", prompt="Images to combine"}

begin

	int x1,x2,y1,y2,d
	real itime

	images(s1//"[0]","EXPTIME")
	itime=real(imgets.value)

	imgets(s1//"[0]","DETECTOR")
	d=int(imgets.value)
	imgets(s1//"[0]","XCORNER")
	x1=int(imgets.value)
	imgets(s1//"[0]","YCORNER")
	y1=int(imgets.value)
	imgets(s1//"[0]","i_naxis1")
	x2=int(imgets.value) + x1 + 1
	imgets(s1//"[0]","i_naxis2")
	y2=int(imgets.value) + y1 + 1


	s3 = "["//x1//":"//x2//","//y1//":"//y2//","//d//"]"
	imarith ("base","*","0","psf")
	imcopy (s1//"[0]","psf"//s3)
	imcopy("psf[,,"//d//"]","chip"//d//"psf"//i)
	imdel("psf",verify-)


end

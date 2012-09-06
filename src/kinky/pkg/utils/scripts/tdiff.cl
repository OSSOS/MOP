#
#  tdiff.cl  
#
#   computes time difference of two exposures.
#  Accesses the image headers to get the exposure start time.
#

procedure tdiff(file1,file2)

	string file1 {"", prompt=" first  image name "}
	string file2 {"", prompt=" second image name "}

begin 
	real   time1, time2
	string f1,f2

	print(" ") 
	print(" * Time difference *")
	print(" ") 
	f1 = file1
	f2 = file2

	gettime(f1)
	time1 = gettime.outputime
	print(" **  Image 1 acquired at ", time1, " UT") 

	gettime(f2)
	time2 = gettime.outputime
	print(" **  Image 2 acquired at ", time2, " UT") 
	print(" ") 
	 
	print(" Time difference is ", (time2-time1), " hours")

	print(" ")
	print(" Done ")
	print(" ")

end

	

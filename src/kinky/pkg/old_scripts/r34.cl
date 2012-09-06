#
#  r34.cl  
#
#   tries to call gettime
#
#

procedure r34(filename)

	string filename {"", prompt=" filename "}

begin 
	string instring, infile, starttime
	real time

	instring = 'UT-START'
	print(" ") 
	print(" Welcome to test ")
	print(" ") 

	infile = filename
	gettime(infile) 
	time = gettime.outputime

	print(" ") 
	print(" time = ", time) 
	
	print(" ")
	print(" Done ")
	print(" ")

end

	

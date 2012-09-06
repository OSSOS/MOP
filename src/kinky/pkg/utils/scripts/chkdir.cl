
procedure chkdir (dirname)

     string dirname {"",prompt="Directory to check for existance"} ;
     bool   verbose {no,prompt="Report progress"} ;
begin 

	if (!access(dirname))  {
	  if (verbose) print("WARNING: Can't open directory "//dirname//"\n") ;
	  if (verbose) print("Attempting to create directory "//dirname//"\n") ;
	  mkdir(dirname) ;
	  if (!access(dirname))
	    error(11,"Can't open or create directory "//dirname) ;
	}

end

#
#  kdelete.cl  
#
#   check if the file exists and delete it if it does
#

procedure kdelete(filename)

	string filename {"", prompt=" file to delete"}

begin 
	string t_filename

	t_filename = filename
	if ( access(t_filename) ) delete(t_filename,verify-)

end

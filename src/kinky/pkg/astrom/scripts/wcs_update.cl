
procedure find_standards (image)

string image {"", prompt="Image to search"}
string standards  {"",prompt="standards file"}



begin
	string timage
	timage = image

	ccfind (standards, timage//".ccfind", timage ) 

	awk ("-f /net/azalea/2/kavelaar/iraf/local/pkg/scripts/awk.fmt "//timage//".ccfind", > timage//".coo")

	phot (timage, timage//".coo", "default", verify-, verbose-)

end

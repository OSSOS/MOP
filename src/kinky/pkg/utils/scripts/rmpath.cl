

procedure rmpath(images)
#  
# rm the machine from keyword i_pixfile in header
#
string images  {"", prompt= " Input image or image list"}
struct *imagelist

begin
    file imagefile, image
    string ifile
    int len, i

    cache("imget")

    imagefile= mktemp("tmp$imgfl")
    sections(images, option="fullname", > imagefile)
    imagelist=imagefile
    while (fscan(imagelist,image) != EOF) {
	imget(image, "i_pixfile")
        if (imget.value != "0") {
		i=stridx("!", imget.value)+1
		len= strlen(imget.value)
		ifile= substr(imget.value, i, len)
		hedit(image, "i_pixfile", ifile, verify-, show+)
                                 }

					   }
     delete(imagefile, verify=no, go_ahead=yes)
   

end

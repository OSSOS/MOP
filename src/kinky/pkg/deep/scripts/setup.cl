#
# coo contains a single line... the calibrating star
#
#  setup.cl
#
#  Written: 26 Mar 1999
#
# (Evolution of nosky.cl)
#

procedure setup(images,refimg)

	string images {"", prompt=" Common portion of filenames for scaling "}
	string refimg  {"", prompt=" Reference image name, of unplanted data"}
	string coofile {"", prompt=" coordinates of star in the refernce frame"}
	string psfcoo {"", prompt=" coordinates of star in the refernce frame"}
	string stime {"", prompt=" image header time keyword"}

begin 
	string t_files, t_refimg,t_coo,ttime


	t_files = images;
	t_refimg = refimg;
	t_coo=coofile;
	ttime=stime;

	imalign("@"//t_files,t_refimg,t_coo,"a//@"//t_files) ;
	
	t_coo = psfcoo ;

 	phot("a@"//t_files,t_coo,"default");

	pstselect("a@"//t_files,"default","default",1);

	psf("a@"//t_files,"default","default","default","default","default");

	scale(t_files,"a"//t_refimg,"a","s")

        massfilt(t_files,"sa","m")

	delete("a*",verify-)

	delete("sa*",verify-)

	fallgrid(t_files,"m","m"//t_refimg,ttime )


end

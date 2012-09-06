#  Convience routine for running fixpix on an image.
#
procedure pipefixpix(image)
	string image {"",prompt="Image to fixpix"}
	string mask  {"mask",prompt="Fixpix mask"}
	bool verbose  {no,prompt="Report Progress?"}

begin
	string t_mask, t_image, t_working;
	int chipid;

	t_image=image;
	t_mask="mask";

	imgets(t_image,"IMAGEID");
 	chipid=int(imgets.value);
	if (!imaccess(t_image) ) {
	   error(1,"no image to work on\n");
	}
	printf("%s%02d\n",t_mask,chipid)|scan(t_mask);
	if ( !imaccess(t_mask)) {
	  t_working = mktemp("mask");
	  imcopy (t_image,t_working);
	  imarith(t_working,"/",t_working,t_working,calc="ushort",pix="ushort");
	  imarith(t_working,"-",1,t_working,calc="ushort",pix="ushort");
	  imarith(t_working,"*",-1,t_working,calc="ushort",pix="ushort");
	  imrename(t_working,t_mask);
	}


	fixpix(t_image,t_mask,linterp=INDEF,cinterp=INDEF,verbose=verbose,pixels=no);

end 

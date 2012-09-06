
# WFI version.

procedure movewfi (images)

string images {"", prompt="MEF file(s) to move"}
string obsdir {"", prompt="Observer directory (include trailing slash)"}
string reddir {"", prompt="Data reduction directory (include trailing slash"}
bool   flipem {"", prompt="Flip the images during copy"}
bool   overwrite {"", prompt="Overwrite images"}
struct	*imlist

begin
   string timage,timages,fname,tobsdir,treddir,inname,basedir ;
   string fin,fout,chip,tfile ;
   bool	tflip,ans ;
   int i ;

printf(" WFI VERSION \n");
tobsdir = obsdir ;
treddir = reddir ;
tflip = flipem ;
tfile = mktemp("/tmp/") ;

if (!access(tobsdir) ) {
  error(11,"Can't access the input image dir "//tobsdir) ;
    }
files(images, >tfile) ;
imlist = tfile;

#---------------
while ( fscan(imlist,timage)!=EOF) {
  inname = tobsdir//timage ;
  
  if (!imaccess(inname//"[im1]") ) 
    error(11,"Can't openfile "//inname) ;
  
  imgets(inname//"[im1]","title") ;
  fname = imgets.value ;
  
  chkdir(treddir);
  basedir = treddir//fname ;
  chkdir(basedir) ;
  
  for (i=1; i<9; i+=1 ) {
    printf("im%01d\n",i) | scan(chip) ;
    fout = basedir//"/"//chip ;
    chkdir(fout) ;
    fin = inname//"["//chip//"]" ;
    if (imaccess(fout//"/"//timage) ) {
      print("File "//fout//"/"//timage//" exists\n") ;
      ans = overwrite ;
      if (ans)  {
	print("Overwriting "//fout//"/"//timage//"\n") ;
	imdelete(fout//"/"//timage) ;
      }
    }
    if (tflip) {
      ewflip(fin,fout//"/"//timage) ;
    } else {
#### Try chpix
#      imcopy(fin,fout//"/"//timage) ;
      chpix(fin,fout//"/"//timage,'ushort') ;
    }
  }

}
#---------------
delete(tfile,verify-);

end

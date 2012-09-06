
# CFHT version.

procedure movem (images)

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
   int i,nccds ;

tobsdir = obsdir ;
treddir = reddir ;
tflip = flipem ;
tfile = mktemp("/tmp/") ;

if (!access(tobsdir) ) {
  error(11,"Cann't access the input image dir "//tobsdir) ;
    }
files(images, >tfile) ;

imlist = tfile 


#---------------
while ( fscan(imlist,timage)!=EOF) {
  inname = tobsdir//timage ;
  
  if (!access(inname) ) 
    error(11,"Can't openfile "//inname) ;
  
  imgets(inname//"[0]","title") ;
  fname = imgets.value ;

  imgets(inname//"[0]","NCCDS") ;
  nccds = int(imgets.value);
  
  chkdir(treddir);
  basedir = treddir//fname ;
  chkdir(basedir) ;
  
  for (i=0; i<nccds; i+=1 ) {
    printf("chip%02d\n",i) | scan(chip) ;
    fout = basedir//"/"//chip ;
    chkdir(fout) ;
    fin = inname//"["//i//"]" ;
    if (access(fout//"/"//timage) ) {
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
      imcopy(fin,fout//"/") ;
    }
  }

}
#---------------
delete(tfile,verify-);

end

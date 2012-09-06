####
## Original concept from JJK.  
# This script moves an MEF file into a set of subdirectories, one per chip.
# The name of the root directory comes from the 'title' keyword.

procedure movem (images)

string images {"", prompt="MEF file(s) to move"}
string title  {"title", prompt="Keyword to base root directory name on"}
pset   movesingle {"", prompt="Parameter set for MOVESINGLE"}
struct	*imlist

begin
   string timage,timages,tobsdir,treddir,inname,basedir ;
   string fin,fout,chip,tfile,ttitle ;
   bool	tflip,ans ;
   int i,nccds ;

#--------------
# Build a list of MEF images to move
#
   tfile = mktemp("/tmp/") ;
   sections(images//"*.fits", >tfile) ;
   imlist = tfile 


#---------------
   while ( fscan(imlist,timage)!=EOF) {
      inname = timage ;
      if (!access(inname) ) 
         error(11,"Can't openfile "//inname) ;  
      # Run move single, once per ccd
      # Here I'm assuming that a ccd is the same as an
      # MEF extension.  Should change this I suppose.
      # so that it read NEXTEND
      imgets(inname//"[0]","NEXTEND") ;
      nccds = int(imgets.value);  
      for (i=1; i<=nccds; i+=1 ) {
        movesingle(timage,extn=i) ;
      }
}
#---------------
delete(tfile,verify-);

end

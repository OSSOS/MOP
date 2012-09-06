# Task to move an MEF file into chip based sub directories.

procedure movesingle (images)

string images {"", prompt="file(s) to move"}
bool   pipeline {"yes",prompt="Is this running in pipeline?"}
bool   plant {"yes",prompt="Queu for planting fake objects?"}
string status {"makepsf_input.txt",prompt="PIPELINE queue file"}
int extn {"", prompt="file(s) to move"}
string obsdir {"", prompt="Observer directory "}
string reddir {"", prompt="Data reduction directory "}
string blank {"_", prompt="Replace space with string"}
string chipname {"EXTVER", prompt="Keyword with chip number"}
bool   rn {no, prompt="Only rename images"}
bool   flipem {no, prompt="Flip the images during copy"}
bool   overwrite {no, prompt="Overwrite images"}
bool   shorten {no, prompt="Change to ushort"}
bool   verbose {no, prompt="Report progress?"}

# Some internal/external type variables
string dest {"", prompt="(Return value) directory created to put image in"}
struct	*movesingle {"", prompt="Move Single now has it's own structure"}

begin
   string timage,timages,fname,tobsdir,treddir,inname,basedir ;
   string fin,fout,chip,tfile,tblank, tstatus;
   bool	tflip,ans,tshort, trn, doit, tpipeline ;
   bool tplant;
   int i, wc, chipid, ss,se, t_extn;

if ( verbose ) printf("SINGLE CHIP VERSION of MOVEM\n");
   timage = images;

  tobsdir = obsdir ;
  tpipeline = pipeline;
  tplant = plant;
  tstatus = status;
  
  t_extn = extn;
# Need to ensure a trailing slash
  if (substr(tobsdir,strlen(tobsdir),strlen(tobsdir)) != "/" ) 
	tobsdir = tobsdir//"/" ;

   treddir = reddir ;
# Need to ensure a trailing slash
   if (substr(treddir,strlen(treddir),strlen(treddir)) != "/" ) 
	treddir = treddir//"/" ;
   trn = rn ;
   tflip = flipem ;
   tshort = shorten ;
   tblank = blank;

  if (!access(tobsdir) ) {
    error(1,"Cann't access the input image dir "//tobsdir) ;
  }

   


#---------------
#while ( fscan(movesingle,timage)!=EOF) {
  inname = tobsdir//timage//"["//t_extn//"]" ;
  
  if (!imaccess(inname) ) 
    error(2,"Can't openfile "//inname) ;
  
# Create the name of the subdir to store the images. 
# change blanks to "_" in the object name.
  imgets(inname,"title") ;
  fname = imgets.value ;
  while ( stridx(" ",fname)  != 0 ) {
     i = stridx(" ",fname);
     ss = i - 1 ;
     se = i + 1 ;
     fname = substr(fname,1,ss)//blank//substr(fname,se,strlen(fname)) ;
  }
  chkdir(treddir,verbose=verbose);
  basedir = treddir//fname ;
  chkdir(basedir,verbose=verbose) ;

#  imgets(inname,"IMAGEID") ;
  imgets(inname,chipname) ;
  chipid = int(imgets.value);
  printf("%s/chip%02d/\n",basedir,chipid) | scan(chip) ;
  chkdir(chip) 
  
  fout = chip ;
  dest = fout ;
  fin = inname ;
  doit = yes ;
  if (imaccess(fout//timage) ) {
    if (verbose) print("File "//fout//timage//" exists\n") ;
    if (overwrite)  {
      if (verbose) print("Overwriting "//fout//timage//"\n") ;
      imdelete(fout//timage) ;
    } else {
      if (verbose) print("Leaving image "//fout//timage//"\n") ;
# set doit to no so we skip the rest of the processing
      doit = no ;
    }
  }
  if ( verbose ) print("Createing image ",fout//timage);
  if (doit) {
    if (trn) {
      imrename(fin,fout//timage,verbose=verbose)
    } else {
      if (tflip) {
        ewflip(fin,fout//timage,verbose=verbose) ;
      if (tshort) 
        chpixtype(fout//timage,fout//timage,'ushort',verbose=verbose);
      } else {
        if (tshort) {
          chpix(fin,fout//timage,'ushort',verbose=verbose) ;
        } else {
          imcopy(fin,fout//timage,verbose=verbose) ;
        }
      }
    }
    hedit(fout//timage,"CHIPID",chipid,add+,show-,update+,verify-)
  }
  if ( tpipeline ) {
     touch(tstatus)
     printf("%s %s %b\n",fout,timage,tplant,>>tstatus);
  }
#}

#---------------

end


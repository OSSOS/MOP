# p60proc -   process P60 cosmic frames (bias only).

procedure p60proc(irafiles)

string irafiles   {prompt = "List of IRAF files"}

struct *inlist

begin

 string  infile 

 infile = irafiles
 inlist = infile

   linebias.interactive = no
   linebias.bias = '[1:2048,2048:2048]'
   linebias.trim = '[1:2025,1:2047]'

 while(fscan(inlist,infile) != EOF) {
   print ("Now running LINEBIAS...")
     linebias (infile,infile)
  }
 print ("Done with procedure p60proc")   
end

# Script to flip around the CFH12k images to comply with expectations
# of mscred package... darn

procedure flip(input)

string input {"", prompt="CFH12k image to flip"}

begin
	string tinput
	string current
	string tname,extname
	int ext,prev
	string detsec
	int    c1,c2,l1,l2


	tinput = input

	hedit (tinput//"[0]","flip",1,add+,verify-,update+)
  	for (ext=1; ext <= 12; ext += 1 ) { 

	current=tinput//"["//ext//"]"

	imgets(current,"DETSEC")
	if ( fscanf(imgets.value,"[%d:%d,%d:%d]",c1,c2,l1,l2) != 4)
	  error(1, "parse error DETSEC keyowrd value "//imgets.value//"\n")
	else {
	  if ( l2 < l1 ) {
	     prev= ext-1
	     tname = mktemp("flip")//".fits"
	     imcopy(current//"[*,-*]",tname)
	     imgets(current,"CCDSEC")
 	     fxdelete(tinput,ext)	
	     hedit(tname,"CCDSEC",imgets.value,verify-,update+)
	     detsec = "["//c1//":"//c2//","//l2//":"//l1//"]"
	     hedit(tname,"DETSEC",detsec,verify-,update+)
	     fxinsert(tname,tinput//"["//prev//"]","")
	     imgets(tname,"CHIPID")
	     imdel(tname,verify-)
	     print(imgets.value) | scan(prev)
	     printf ("chip%02d\n",prev) | scan(extname)
	     hedit(current,"EXTNAME",extname,add+,verify-,update+)
	  }
	  if ( c2 < c1 ) { 
	     prev= ext-1
	     tname = mktemp("flip")//".fits"
	     imcopy(current//"[-*,*]",tname)
	     imgets(current,"CCDSEC")
 	     fxdelete(tinput,ext)	
	     hedit(tname,"CCDSEC",imgets.value,verify-,update+)
	     detsec = "["//c2//":"//c1//","//l1//":"//l2//"]"
	     hedit(tname,"DETSEC",detsec,verify-,update+)
	     fxinsert(tname,tinput//"["//prev//"]","")
	     imgets(tname,"CHIPID")
	     imdel(tname,verify-)
	     print(imgets.value) | scan(prev)
	     printf ("chip%02d\n",prev) | scan(extname)
	     hedit(current,"EXTNAME",extname,add+,verify-,update+)
	  }
	  ;
	}
	;
	}

end

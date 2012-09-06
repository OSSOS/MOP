#/* program to take the output from IRAFs apcor program and make it 
# useful for our pipeline 
#/
#include<stdio.h>

procedure apcor_munge (filename,inap,outap) 

	string filename {"",prompt="name of the apfile to modify"}
	int	inap    {"",prompt="inner aperature radius"}
	int     outap   {"",prompt="outer aperature radius"}

begin
	real apcor,aperr
	string fname
        int i
	
    list = filename
    i=fscan(list,line)
    i=fscan(list,line)
    i=fscan(list,fname,apcor,aperr)
    printf("%s %s %f %f\n",inap,outap,apcor,aperr);
    

end

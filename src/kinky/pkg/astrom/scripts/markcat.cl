procedure markcat(catalog,solncoef,racol,deccol,nlineskip)

#  This program uses the computed plate solution to translate
#  RA and DEC into pixel coordinates.  You must display the
#  appropriate image before invoking this task.

string catalog  {"", prompt="catalog file with RA and DEC"}
string solncoef {"", prompt="file with plate solution"}
int  racol      {"", prompt="ra column"}
int  deccol     {"", prompt="dec column"}
int  nlineskip  {"", prompt="number of lines to skip"}
bool numbering  {yes,prompt="do you want to number?"}
struct *pntfile2
begin
    string now, coeffile, command,pointfile,catfile,pntfile
    string ra,dec,xst,yst
    int  racolumn,deccolumn,nskip

	cache("gasp")
	cache("stsdas.gasp.eqxy")

	coeffile=solncoef
	catfile=catalog
	racolumn=racol
	deccolumn=deccol
	nskip=nlineskip
	pointfile=mktemp("tmp$pt")
	pntfile=mktemp("tmp$pnt")
 	eqxy(no,"",coeffile,catfile,racolumn,deccolumn,nskip,ra_hours=no, > pntfile)
	pntfile2=pntfile
	now=fscan(pntfile2,ra,dec,xst,yst)
	while(fscan(pntfile2,ra,dec,xst,yst) != EOF){
	   print(xst," ",yst," " ,ra,dec, >> pointfile)
	  }
           tvmark(1, pointfile, mark="circle",radii=6, number=numbering, color=206)
          delete(pntfile, verify-)
          delete(pointfile, verify-)
end

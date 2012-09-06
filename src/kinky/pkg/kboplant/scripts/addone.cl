#
#  addone.cl  
#
#   adds one star to N images with common filenames
#
#  Input requirements: 
#    - There must be N(=nimg) files named xxxxn with
#      xxxx being the common file name and n being sequential numbers

procedure addone(common,paddfile,prefimg)

	string common {"", prompt=" Common portion of old filename "}
	string paddfile {"", prompt=" filename of x,y,mag of new star "}
	string prefimg {"", prompt=" name of reference image "}

begin 
	int    nimg,i,oldn,oldfirst, flag, xinvert, binvert
	int    newfirst
	real   xcen,ycen,refmag,plantmag,thisrefmag,dmag,thismag
	string dummy, rootword, addfile, refimg, thisfile
	string infile, tfile

	print(" ") 
	print(" Welcome to addone")
	print(" *********************** Must have daophot loaded....."
	print(" ") 
	rootword = common
	addfile = paddfile
	refimg = prefimg

	print(" input first # of image set: ") 
	scan(oldfirst)
	print(" input # of images : ") 
	scan(nimg)

	list=addfile
	dummy = fscan(list,xcen,ycen,plantmag)
	print(" XCEN, YCEN, PLANTMAG ",xcen,ycen,plantmag)

	tfile=mktemp("tmp$pt")
	infile = refimg//".pst.2"
	pdump(infile,"MAG","yes", >> tfile)
	list=tfile
	dummy = fscan(list,refmag)
	print(" REFMAG ",refmag)
	delete(tfile, verify-)

	print(" ") 
	print(" Adding stars: ")
	print(" ") 
#
# add stars one by one 
#
		
	for (i=oldfirst; i<=nimg+oldfirst-1; i+=1)
	{
		infile  = rootword//i
		print("Planting in :", infile)
		thisfile = infile//".pst.2"
		tfile=mktemp("tmp$pt")
                pdump(thisfile,"MAG","yes", >> tfile)
        	list=tfile
        	dummy = fscan(list,thisrefmag)
#       	print(" THISREFMAG ",thisrefmag)
		delete(tfile, verify-)
		dmag = thisrefmag - refmag
        	print(" DMAG ",dmag)
		thismag = plantmag + dmag
		tfile=mktemp("tmp$pt")
		print(xcen,ycen,thismag, > tfile)
		addstar(infile,tfile,"default","default",simple+,verify-)
		delete(tfile, verify-)
	}

	print(" ")
	print(" Done ")
	print(" ")

end

	

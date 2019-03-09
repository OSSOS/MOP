#
#  scale.cl
#
#  Written: 26 Mar 1999
#
# (Evolution of nosky.cl)
#

procedure scale(common,refimg,oldprefix,newprefix)

	string common {"", prompt=" Common portion of filenames for scaling "}
	string refimg  {"", prompt=" Reference image name, of unplanted data"}
	string oldprefix {"", prompt=" prefix on .pst.2 files? "}
	string newprefix {"", prompt=" output file prefix? "}
	string oldsuffix {"", prompt=" image name suffix? "}
	string *sclist
	string *pstlist
begin 
	int    nimg 
	string infile,rootword,outfile,cplist,junkfile,finfile
	string dummy,t_refimg,tfile,pstfiles,t_prefix
	real   sky, scale, dum, medn
	real   refmag,thismag,dmag
	string add1,add2,t_oldpref,t_suf

	print(" ")
	print(" ") 
	print(" Scale PROGRAM -- Subtracts zero-order sky and scales to")
	print("                  all frames to constant flux for ref star.")
	print(" ") 
	print(" NOTE that the first two common filenames could be the same") 
	print(" ") 

	rootword = common
	t_refimg = refimg
	t_oldpref = oldprefix
	t_prefix = newprefix
        t_suf = oldsuffix

        infile = t_refimg//".pst.2"
	print (infile)
        txdump(infile,"MAG","yes") | scan(refmag)
	print (refmag) 

	sclist = rootword
	while ( fscan(sclist,infile) != EOF ) { 
	    pstfiles = t_oldpref//infile//".pst.2"
	    print(pstfiles)
            tfile=mktemp("tmp$pt")
            pdump(pstfiles,"MAG,MSKY","yes", >> tfile)
            pstlist=tfile
            dummy = fscan(pstlist,thismag,sky)
            delete(tfile, verify-)
	    dmag = (thismag - refmag)/2.5
	    scale = 10**dmag
	    outfile = mktemp("nsj");
	    infile = t_oldpref//infile	
	    print("       Currently working on: ",infile," into ",outfile)
            imarith (infile//t_suf,"-",sky,outfile)
	    print ( "subtracted ", sky," from ", infile)

	    finfile = t_prefix//infile
            imarith (outfile,"*",scale,finfile)
	    imdel(outfile, verify-)
            imarith (t_refimg, '-', finfile, 'sub_'//finfile)
	    print ( "multiplied ", outfile, " by ", scale,"--> ",finfile)
#
	}
	print(" Done ")
	print(" ")

end

	

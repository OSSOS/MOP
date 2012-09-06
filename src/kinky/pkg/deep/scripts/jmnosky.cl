#
#  jmnosky.cl
#
#

procedure jmnosky(common,t_fim,t_nim,t_stat1,t_stat2)

	string common {"", prompt=" Common portion of filename "}
	int    t_fim  {"", prompt=" Number of first image "}
        int    t_nim  {"", prompt=" How many images? "}
	string t_stat1  {"", prompt=" Sky statistic section"}
	string t_stat2  {"", prompt=" flux statistic section"}
	real   t_arbscale {"1000", prompt=" Arbitrary scaling?"}

begin 
	int    nimg,fimg
	string infile,rootword,outfile,cplist,junkfile,finfile
	string stat1, stat2
	real   sky, scale, dum, medn
	real   arbscale 
	string add1,add2
	rootword = common
	print(rootword)
	fimg = t_fim
	print(fimg)
	nimg = t_nim
	print(nimg)
	stat1 = t_stat1
	print(stat1)
	stat2 = t_stat2
	print(stat2)
	arbscale = t_arbscale
	print(arbscale)

	print(" ")
	print(" ") 
	print(" NoSky PROGRAM -- scales on a provided sky and star section")
	print(" ") 

	for (i=fimg; i<=nimg+fimg-1; i+=1)
	{
	    infile = rootword//i

#  sum this file to the outfile and place results in junkfile.
#  then remove the old outfile and replace with the summed image
	    outfile = "nsj"//rootword//i
	    print("       Currently working on: ",infile," into ",outfile)
	    add1 = rootword//"sky"//i
	    junkfile = infile//stat1
            imstat (junkfile, fields = "midpt", format = no, > add1) 
            list = add1 
            dum = fscan(list, sky) 
            del (add1,verify-,go_ahead+)
            imarith (infile,"-",sky,outfile)
	    print ( "subtracted ", sky," from ", infile)

	    add2 = rootword//"scl"//i
	    junkfile = outfile//stat2
            imstat (junkfile, fields = "mean", format = no, > add2) 
            list = add2 
            dum = fscan(list, scale) 
            del (add2,verify-,go_ahead+)
	    finfile = "ns"//rootword//i
#  Arbitrary scaling!
	    scale = arbscale/scale
            imarith (outfile,"*",scale,finfile)
	    imdel(outfile, verify-)
	    print ( "multiplied ", outfile, " by ", scale,"--> ",finfile)
#
	}

	print(" Done ")
	print(" ")

end

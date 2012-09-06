#!/bin/csh
#
#Step 1: Build a catalog of all frames to look for candidates in 
#	
#Step 2: Generate a file with the DETECETED and PREDICTED locations of the objects with .abg files (frames.list)
#
#Step 3: Display the frames with circles around the DETECTED and PREDICTED locations of the object. Allow 
#	 user to select the real detections.  Output a .cands format file with ALL real detections of the object.
#
#Step 4: Produce MPC format files.

set wdir = `pwd`
	
foreach cands ( `find ./ -name "*cands.comb" -print`) 
    set idir = $cands:h
    cd $idir
    abg_gen2.pl --cand *cands.comb 
    foreach abg ( *.abg )
	cd $wdir
	set list = $abg:r
	set list = $list.list
        obs_find.pl --abg $idir/$abg --cat $wdir/frames.cat > $list
        confirm.pl --frames $list 
    end
end

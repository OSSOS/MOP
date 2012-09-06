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

	
build_cat.pl --output frames.cat --code 568 */*.fits

set $wdir = `pwd`

set $MPC = $wdir/MPC
touch $MPC

foreach chip ( */chip?? ) 
    cd $chip
    abg_gen.pl --cand *cands.comb
    foreach abg ( *.abg )
        obs_find.pl --abg $abg --cat $wdir/frames.cat > frames.list
        confirm.pl --frames frames.list  >>$MPC
    end
    cd $wdir
end

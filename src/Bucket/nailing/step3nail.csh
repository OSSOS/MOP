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
	
foreach mpc ( `ls  */*/Astrom`) 
    foreach obj ( ` grep -v "#" $mpc |  awk ' { print $1  } ' | sort -u  ` )
        set abg = $obj.abg
        grep $obj $mpc | fit_radec -j ~/bin/binEphem.405 -o ~/bin/observatories.dat  > $abg
        set list = $abg:r
        set list = $list.list
	if ( !( -f $list) ) then
            obs_find.pl --abg $abg --cat frames.cat > $list
        endif
        wc -l $list
        confirm.pl --frames $list --cat frames.cat
    end
end


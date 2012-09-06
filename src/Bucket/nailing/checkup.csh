#!/bin/csh


### a quick script to do what the nailing scripts do...
####

foreach MPC ( o1988p004aC.MPC o1988p004aD.MPC )
 set obj = $MPC:r
 set abg = $obj.abg2
 set list = $obj.list2
 fit_radec -j ~/bin/binEphem.405 -o ~/bin/observatories.dat < $MPC > $MPC:r.abg2
 obs_find.pl --sec 400 --abg $abg --cat checkup.cat > $list
 wc -l $list
 confirm.pl --frames $list --cat frames.cat
end


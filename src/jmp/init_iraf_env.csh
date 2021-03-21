# Source this file to setup the IRAF environment under Ubuntu-18
# Unset variable that are used by IRAF
unset host iraf hbin hlib
unset mach arch IRAFARCH IRAFBIN

set irafbin = /usr/lib/iraf/bin/

if ( -x ${irafbin}vocl.e ) then
   set cl_binary = "${irafbin}vocl.e"
else if ( -x ${irafbin}ecl.e ) then
   set cl_binary = "${irafbin}ecl.e"
else
    set cl_binary = "${irafbin}cl.e"
endif



set iraf = "/usr/lib/iraf/"
set host = "${iraf}unix/"
set hlib = "${host}hlib/"
set hbin = "${host}bin"
set F77 = "$hlib/f77.sh"
set F2C = "$hbin/f2c.e"

set iraf_version = `grep version\\s  ${iraf}unix/hlib/zzsetenv.def | cut -d\" -f2 | cut -d" "  -f3`


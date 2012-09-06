#!/bin/csh

# This script will prime the pipeline with the names of the files that are 
# specfic to some object name.

set object = $1
set dest = $2

if ( $object == "" || $dest == "" ) then
	echo "USAGE: $0 <object> <dest>"
	echo "<object> is the name of the object"
	echo "<dest> is the directory where you want"
	echo "to run the pipe from, run this prorgam"
	echo "in the directory where the images"
	echo "you want to stick into the pipeline are located"
	exit;
endif

foreach file ( `gethead *.fits OBJECT | grep $object | awk '{ print ( $1 ) }' ` )
 if ( !($dest == "." || $dest == "./" || $dest == `pwd`) ) then
    mv $file $dest
    rm $dest/$file:r.detrended
 endif
 echo $file:r >> $dest/detrend_input.txt
end

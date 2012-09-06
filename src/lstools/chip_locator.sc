#!/bin/csh

#input ra and dec which you wish to locate in any image under this directory.

/bin/rm ttt
find . -name "*.fits" | grep -v psf | grep -v fk >ttt
foreach i (`cat ttt`)
  chip_locator $1 $2 $3 $4 $5 $6 -n $i
end


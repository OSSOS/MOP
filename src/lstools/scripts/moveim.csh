#!/bin/csh

#Need chip list (01 02 03 .. 36 ) - chips
#Need file/object list - fields (run makenfile.sc)

#make directories
foreach i (`cat chips`)
    if (!(-e chip$i)) mkdir chip$i
    cd chip$i
    foreach j (`cat ../fieldsonly`)
	if (!(-e $j)) mkdir $j
    end
    cd ..
end

# move each image to CHIP/FIELD directory
foreach i (`cat imlist.triplet`)    
    foreach j (`cat chips`)
    mv $i"_"$j.fits chip$j/`grep $i fields | awk '{print $2}'`/.
    end
end

foreach i (`cat imlist.nailing`)
   foreach j (`cat chips`)
    mkdir chip$j/`grep $i fields | awk '{print $2}'`/nailing
    mv $i"_"$j.fits chip$j/`grep $i fields | awk '{print $2}'`/nailing/.
   end
end

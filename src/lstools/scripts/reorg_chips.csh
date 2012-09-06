#!/bin/csh

# script moves Field/Chip organized directories into Chip/Field directories
# (new method is chip/field for processing on cluster)

#make the new directories
foreach i (`cat chips`)
    mkdir chip$i
    cd chip$i
    foreach j (`cat ../fieldsonly`)
	mkdir $j
    end
    cd ..
end

#move stuff to new directories
foreach i (`cat chips`)
    foreach j (`cat fieldsonly`)
	mv  $j/chip$i/* chip$i/$j/.
	rmdir $j/chip$i
    end
end

# and remove the end of the directories
foreach j (`cat fieldsonly`)
    rmdir $j
end

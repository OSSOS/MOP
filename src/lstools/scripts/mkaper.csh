#!/bin/csh

#Need list of chips (01 02 03 .. 36) == chips
#Need field list (image root names + name of object directories) == fields

# for jmpmakepsf (to start pipeline, need this before find.pl)
# give jmpmakepsf name of directory & image & yes/no (whether to plant)

# maybe have to add proc-these-files to each directory, if 
# not already there ..

foreach f (`cat fieldsonly`)
 foreach i (`cat chips`)
    cd $f/chip$i
    if (!(-e zeropoint.used)) then
      echo "26." > zeropoint.used
    endif
    /bin/rm aper.corr
	touch aper.corr
      echo "5 18 0.20 0.01" >> aper.corr
      echo "5 18 0.20 0.01" >> aper.corr
      echo "5 18 0.20 0.01" >> aper.corr
    cd ../..
 end
end

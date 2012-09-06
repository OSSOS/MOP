#!/bin/csh

#Need list of chips (01 02 03 .. 36) == chips
#Need field list (image root names + name of object directories) == fields

# for jmpmakepsf (to start pipeline, need this before find.pl)
# give jmpmakepsf name of directory & image & yes/no (whether to plant)

if (-e find.input) /bin/rm find.input
touch find.input
foreach f (`cat fieldsonly`)
 foreach i (`cat chips`)
  echo find.pl -p "''" -d chip$i/$f >> find.input
 end
end
# then this starts the whole thing
#source find.pl

# maybe have to add proc-these-files to each directory, if 
# not already there ..

foreach f (`cat fieldsonly`)
 foreach i (`cat chips`)
    cd chip$i/$f
    if (!(-e proc-these-files)) then
	touch proc-these-files
	echo "# Files to be planted and searched"  >> proc-these-files
	echo "#             image  fwhm plant" >> proc-these-files
	foreach k (`ls *o_??.fits`)
	    echo "      " $k  " 4.5  no" >> proc-these-files
	end
	sed 's/.fits//g' proc-these-files > temp
	/bin/mv temp proc-these-files
    endif
    if (!(-e zeropoint.used)) then
      echo "26." > zeropoint.used
    endif
    if (!(-e aper.corr)) then
      touch aper.corr
      echo "5 18 0.20 0.01" >> aper.corr
      echo "5 18 0.20 0.01" >> aper.corr
      echo "5 18 0.20 0.01" >> aper.corr
    endif
    cd ../..
 end
end

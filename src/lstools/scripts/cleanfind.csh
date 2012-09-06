#!

foreach i (`cat fieldsonly`)
 foreach j (`cat chips`)
cd $i/chip$j
pwd
/bin/rm *_lock*
/bin/rm *.jmp
/bin/rm *.matt
/bin/rm *cands*
/bin/rm *tf
/bin/rm *objm
/bin/rm dumm*
/bin/rm aper.corr
/bin/rm *platesol
/bin/rm *usno
/bin/rm *astrom*
/bin/rm find*
/bin/rm *candidates*
/bin/rm check*
/bin/rm *mopheader
/bin/rm *mkpltsol*
/bin/rm *measure3*
cd ../..
end
end

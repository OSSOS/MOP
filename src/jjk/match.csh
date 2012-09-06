#!/bin/csh
# JJK, july 2001
# USAGE:  match.csh file1.phot files2.phot file3.phot > shifts

if ( -e aper.corr) then
	\rm aper.corr
endif
cat $1.apcor > aper.corr
cat $2.apcor >> aper.corr
cat $3.apcor >> aper.corr

## compute the x/y shifts
step2ajmp $1 $2 $3
if ( ! -e step2ajmp.OK ) then
  echo "step2ajmp failed for $1 $2 $3."
  match_matt -f1 $1 -f2 $2 -f3 $3 -who jmp
endif
\rm -f step2ajmp.OK step2ajmp.FAILED

if ( ! -e $1.trans.jmp ) then
#  match_matt -f1 $1 -f2 $2 -f3 $3 -who jmp
  exit
endif

cat $1.trans.jmp > shifts.nomag;
cat $2.trans.jmp | awk ' { printf("%f %f %f %f %f %f %f\n",-$1,$2,$3,-$4,$5,$6,$7) } ' >> shifts.nomag;
cat $3.trans.jmp | awk ' { printf("%f %f %f %f %f %f %f\n",-$1,$2,$3,-$4,$5,$6,$7) } ' >> shifts.nomag;


## change the .phot file into a format that trans.pl will like.
awk ' { if ( $8 == "\\" ) { print $2,$3,$4,$5 } } ' $1.phot > $1.align
awk ' { if ( $8 == "\\" ) { print $2,$3,$4,$5 } } ' $2.phot > $2.align
awk ' { if ( $8 == "\\" ) { print $2,$3,$4,$5 } } ' $3.phot > $3.align

## compute the mag shifts

trans.pl --shift shifts.nomag --file1 $1.align --file2 $2.align --file3 $3.align --verbose

#\rm shifts.nomag

# that's all folks...
exit

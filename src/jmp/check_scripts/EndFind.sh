#! /bin/bash

sort -u PBstep3jmp-dir > PBstep3jmp-dir-u
\cp find.input3 endfind.input2

\rm -f endfind.input3
touch endfind.input3
cat PBstep3jmp-dir-u | (
  while read direct; do
    echo ${direct}
    grep ${direct} endfind.input2 | sed "s/find.pl/end-find.pl/" >> endfind.input3
  done
)

curdir=`pwd`
for d in `cat PBstep3jmp-dir-u`; do
  cd $d
  \rm -rf find.* fk*moving.jmp fk*comb* step3jmp* no_candidates pipe-eff* fk*_??.eff fk*jmp.found fk*jmp.eff
  cd ${curdir}
done

source endfind.input3

for i in `find . -name "check_me.cl" -print`; do echo $i; cat $i | sed "s|check fk[0-9_po]\+.cands.comb|lsmop ; cd ../../|" >> check_me_all; done

exit

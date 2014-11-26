#! /bin/bash

\rm -f PBstep3jmp-dir PBstep3jmp-list PBstep3jmp-val
for f in chip??/*; do
  if [ -d $f ]; then
    cd $f
    for t in fk*trans.jmp; do
      if [ -e $t ]; then
	s=`awk '{if ($4<-16) {print 0} else {print 1}}' $t`
	if [ $s == "0" ]; then
	  echo "$f" >> ../../PBstep3jmp-dir
	  echo "$f/$t" >> ../../PBstep3jmp-list
	  echo "$f/$t" >> ../../PBstep3jmp-val
	  cat $t >> ../../PBstep3jmp-val
	fi
      fi
    done
    cd ../../
  fi
done

exit

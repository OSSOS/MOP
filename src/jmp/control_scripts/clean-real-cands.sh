#!/bin/bash

for i in `find . -name "*.cands.comb.real" -print`; do
  nl=`cat $i | wc -l`
  if [ $nl -lt 11 ]; then
    echo "No real candidate in $i. Deleted."
    \rm $i
  else
    echo "Real candidate(s) in $i. Keep it."
  fi
done

exit

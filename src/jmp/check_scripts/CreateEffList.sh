#! /bin/bash

if [ $# != 1 ]; then
    echo "Please enter block name (Lnx):"
    read b
else
    b=$1
fi

if [ -f ${b}-global-list ]; then
    \rm -rf ${b}-global-list
fi
touch ${b}-global-list

for d in `cat ${b}-area-list | awk '{print $1}'`; do
    \ls -al ${d}/fk*corrected.comb.found | awk '{if ($5>100) {print $9}}' | sed "s|/fk|/ fk|" | sed "s/.comb.found//" >> ${b}-global-list
done

\cp ${b}-global-list ${b}-eff-list

cat Problems.eff | (
    while read direct; do
	mv ${b}-eff-list zzzzzz
	grep -v ${direct} zzzzzz > ${b}-eff-list
    done
)

\rm -f zzzzzz

#!/bin/bash
if [ $# != 2 ]
then
  echo "Usage:  build_triplets_directory.sh semester block"
  exit
fi

semester=$1
block=$2
vcat vos:OSSOS/triplets/${block}_${semester}_discovery_expnums.txt | while read exp1 exp2 exp3 field 
do 
 vmkdir vos:OSSOS/triplets/20${semester}/${field} 
 for ccd in $(seq -f "%02g" 0 35) 
 do 
    vmkdir vos:OSSOS/triplets/20${semester}/${field}/ccd${ccd} 
    for exp in  ${exp1} ${exp2} ${exp3} 
    do   
       for file in `vls vos:OSSOS/dbimages/${exp}/ccd${ccd}` 
       do 
          vln --verbose vos:OSSOS/dbimages/${exp}/ccd${ccd}/${file} vos:OSSOS/triplets/20${semester}/${field}/ccd${ccd}/${file} 
          #vln vos:OSSOS/dbimages/${exp}/ccd${ccd}/${file} vos:OSSOS/triplets/2013A/${field}/ccd${ccd}/${file} 
       done  
    done
 done
done

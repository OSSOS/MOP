#!/bin/bash
[ -f ${HOME}/.bashrc ] && source ${HOME}/.bashrc
[ -f ${HOME}/.profile ] && source ${HOME}/.profile

if [ $# -eq 3 ] 
then
  array=($1)
  export exp1=${array[0]}
  export exp2=${array[1]}
  export exp3=${array[2]}
  export field=${array[3]}
  export ccd_start=$2
  export ccd_end=$3
else
  export exp1=$1
  export exp2=$2
  export exp3=$3
  export field=$4
  export ccd_start=$5
  export ccd_end=$6
fi
export DBIMAGES=vos:OSSOS/interstellar/dbimages/
export MEASURE3=vos:OSSOS/interstellar/2013A-E/${field}

echo "$0 -- $exp1 -- $exp2 -- $exp3 -- $field -- $ccd_start -- $ccd_end "

vmkdir -p ${MEASURE3}
    
# rmin, rmax are constants for the whole OSSOS survey. but now set for interstellar search
export rmax=15.0
export rmin=1.0
# -23 for L block. +20 for E block.
export ang=-23
# Search the other side
export ang=179
# width has been constant for a while now
export search_width=22
# search the other part of the cone (actually, do the entire thing)
export search_width=179
export loops=1
export force="--force --ignore"
export force=--ignore
export dry_run=

echo "field "${field}


mkdir -p ${field}
cd ${field}
basedir=`pwd`

for expnum in ${exp1} ${exp2} ${exp3} 
do
   vls ${DBIMAGES}/${expnum} || ( vmkdir -p ${DBIMAGES}/${expnum} ; vln vos:OSSOS/dbimages/${expnum}/${expnum}p.fits ${DBIMAGES}/${expnum}/${expnum}p.fits )
done 
   

for ((ccd=ccd_start;ccd<=ccd_end;ccd++))
do
    cd ${basedir}
    ccd_dir_name=`echo $ccd | awk ' { printf("ccd%02d",$0) }'`
    p_ccd=`echo $ccd | awk ' { printf("p%02d",$0) }'`
    mkdir -p ${ccd}
    cd ${ccd}
    echo "Linking to original fits files"
    this_dir=`pwd`
    source_dir=`echo ${this_dir} | sed -e 's/interstellar\///'`
    for filename in `ls ${source_dir}/*.fits`;
    do
       [ -f `basename ${filename}` ] || ln -s $filename $this_dir
    done
    for filename in `ls ${source_dir}/*.unid.*`;
    do
       [ -f `basename ${filename}` ] || cp -u $filename $this_dir
    done
    for expnum in $exp1 $exp2 $exp3;
    do
      [ -f ${expnum}${p_ccd}.unid.matt ] || vcp -v vos:OSSOS/dbimages/${expnum}/$ccd_dir_name/${expnum}${p_ccd}.unid.matt ./
      [ -f ${expnum}${p_ccd}.unid.jmp ] || vcp -v vos:OSSOS/dbimages/${expnum}/$ccd_dir_name/${expnum}${p_ccd}.unid.jmp ./
    done
    [ -f ${exp1}${p_ccd}.trans.jmp ] || vcp -v vos:OSSOS/dbimages/${exp1}/$ccd_dir_name/${exp1}${p_ccd}.trans.jmp ./
    ## First do the search images
    echo -n "Running: "
    echo "stepI.py ${exp1} ${exp2} ${exp3} --ccd ${ccd}  -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width} ${force} ${dry_run} "
    stepI.py ${exp1} ${exp2} ${exp3} --ccd ${ccd}  -v --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width} ${force} ${dry_run}
    echo -n "Running: "
    echo "combine.py ${exp1} -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field}  --ccd ${ccd} ${force} ${dry_run} "
    combine.py ${exp1} -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field}  --ccd ${ccd} ${force} ${dry_run}
    # now run the standard pipeline on the scramble images..
    # echo "Running step3 on s catalogs"
    # stepI.py ${exp1} ${exp2} ${exp3} --ccd ${ccd} -v --type s --dbimages ${DBIMAGES} --rate_min ${rmin} --rate_max ${rmax} --angle ${ang} --width ${search_width} ${force} ${dry_run} 
    # echo "Running combine.py on s catalogs"
    # combine.py ${exp1} --ccd ${ccd} --type s -v --dbimages ${DBIMAGES} --measure3 ${MEASURE3} --field ${field} ${force} ${dry_run} 
done

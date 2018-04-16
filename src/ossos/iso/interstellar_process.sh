#!/bin/bash
#
# This is the bash script to do Edward's ISO search re-processing of the OSSOS pipeline for TNOs search.
#

# initialize the shell environment as JJ is suspicious of initialization.
[ -f ${HOME}/.bashrc ] && source ${HOME}/.bashrc
[ -f ${HOME}/.profile ] && source ${HOME}/.profile

export block=$1
export exp1=$2
export exp2=$3
export exp3=$4
export field=$5
export ccd_start=$6
export ccd_end=$7

# Configure the names of the result directories in VOSpace so we can run validate later.
export DBIMAGES=vos:OSSOS/interstellar/dbimages/
export MEASURE3=vos:OSSOS/interstellar/${block}/${field}

# make sure the destination locations exist on VOSpace.
vmkdir -p ${MEASURE3}
vmkdir -p ${DBIMAGES}

# rmin, rmax are constants for the whole OSSOS survey. but now set for interstellar search
export rmax=15.0
export rmin=1.0

# Search angle for ISO is basically 100% of the sky.
export ang=180
# search the other part of the cone (actually, do the entire thing as this is ISO search)
export search_width=180

# Force: Do the processing even if the expected result files already exist
# Ignore: ignore missing stuff from previous steps as they likely aren't here.
# export force="--force --ignore"
export force=--ignore

# set this flag if you are testing a run of the script and don't want the results reaching VOSpace.
# export dry_run=--dry-run
export dry_run=

echo "Executing interstellar processing on block: ${block} and field: ${field}"

# A working directory for this block (on local disk)
mkdir -p ${block}
cd ${block}

# A working directory for this field.
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

    # Build some variables to hold some directory names and file extension bits.
    ccd_dir_name=`echo $ccd | awk ' { printf("ccd%02d",$0) }'`
    p_ccd=`echo $ccd | awk ' { printf("p%02d",$0) }'`
    mkdir -p ${ccd}
    cd ${ccd}

    echo "Linking to original fits files"
    this_dir=`pwd`

    ##
    ## This section makes links from previously processed area to the current area, which might be helpful.
    ## commented out for now.
    # source_dir=`echo ${this_dir} | sed -e 's/interstellar\///'`
    # for filename in `ls ${source_dir}/*.fits`;
    # do
    #   [ -f `basename ${filename}` ] || ln -s $filename $this_dir
    # done
    # for filename in `ls ${source_dir}/*.unid.*`;
    # do
    #   [ -f `basename ${filename}` ] || cp -u $filename $this_dir
    # done

    # Populate a local processing area with the input files (unid and trans.jmp) needed for interstellar.
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

done

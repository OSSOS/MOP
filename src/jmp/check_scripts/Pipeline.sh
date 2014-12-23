#! /bin/bash

# This script tries to run the whole pipeline. Makes some assumptions as to
# what files exist and where they are.

# For a given block, the discovery triplets are in D/, the nailing images are
# in N/, and the check-up images are in C/.

# The point is to try to keep the disk usage as low as possible at each step.
# So compress or delete all unnecessary file.

# Define some interesting functions.

# Move files from chip directory into field subdirectories.

SplitFiles() {
  d=$1
  pat=$2
  basedir=`pwd`
  cd $d
  for c in chip??; do
    cd $c
    pwd
    for i in ${pat}; do
      if [ -f $i ]; then
        object=`gethead $i OBJECT`
        if [ ! -d ${object} ]; then
          mkdir ${object}
        fi
        \mv -f $i ${object}/
      fi
    done
    cd ..
  done
  cd ${basedir}
}

# Go into a main directory and pre-process images.

PreProc(){
  basedir=`pwd`
  cd $1
  ls *.fits *.fits.fz | cat > file-list
  echo "DONE" >> file-list
  ProcessFiles.sh

# Creates links.
  cd Backup
  for i in *.fits.fz; do
    j=`echo $i | sed "s/.fz//"`
    ln -s Backup/$i ../$j
  done
  cd ${basedir}
}

# Link 2 main directories.

LinkDirs() {
  basedir=`pwd`
  d1=$1
  d2=$2
  dirn=$3
  cd $d1
  for c in chip??; do
    cd $c
    for f in *; do
      cd $f
      ln -s ../../../${d2}/${c}/${f} ${dirn}
      cd ..
    done
    cd ..
  done
  cd ${basedir}
}

# In order to make this script easier to use, separate the processing into
# groups that are activated depending on the presence of flags

if [ $# -eq 0 ]; then
  echo "Usage: Pipeline [--SetupDirs] [--PrepareScripts] [--MakePSF] [--Plant] [--Find]"
  exit
fi

SetupDirs="NO"
PrepareScripts="NO"
MakePSF="NO"
Plant="NO"
Find="NO"
while [ $# -gt 0 ]; do
  case "$1" in
  --SetupDirs)
    SetupDirs="YES"
    shift
    ;;
  --PrepareScripts)
    PrepareScripts="YES"
    shift
    ;;
  --MakePSF)
    MakePSF="YES"
    shift
    ;;
  --Plant)
    Plant="YES"
    shift
    ;;
  --Find)
    Find="YES"
    shift
    ;;
  esac
done

if [ $SetupDirs = "YES" ]; then

# Go into discovery directory and pre-process images.

#PreProc "D/"

# Do the same in nailing directory.

#PreProc "N/"

# Move chip file into field subdirectories.

#SplitFiles "D/" "*p_??.fits"
#SplitFiles "N/" "*p_??.fits"

# Link D/ and N/ directories.

#LinkDirs "D" "N" "nailing"

# Make sure nailing files are known in discovery directory

basedir=`pwd`
#cd N/Backup
#for i in *p.fits.fz; do
#  j=`echo $i | sed "s/.fz//"`
#  ln -s ../N/Backup/$i ../../D/$j
#done

# Uncompressing files
for c in chip??; do
  cd $c
  for f in *; do
    cd $f
    for i in *.fits.gz; do
      if [ -e $i ]; then
        echo "gunzip $i"
        gunzip $i
      fi
    done
    for i in *.fits.fz; do
      if [ -e $i ]; then
        j=`echo $i | sed "s/.fz//"`
        echo "imcopy $i[1] $j"
        imcopy $i[1] $j
        \rm -f $i
      fi
    done
    for i in *p[0-3][0-9].fits; do
      j=`echo $i | sed "s/p/p_/"`
      mv $i $j
    done
    cd ../
  done
  cd ../
done
cd ${basedir}

# Extracting header files
for c in chip18; do
  cd $c
  for f in *; do
    cd $f
    for i in *.fits; do
      j=`echo $i | sed "s/_..//"`;
      echo "imhead -f $i > ../../$j"
      imhead -f $i > ../../$j
    done
    cd ../
  done
  cd ../
done
cd ${basedir}

fi

if [ $PrepareScripts = "YES" ]; then

# Now, create auxilary files, and generate the "allpipeline.go" file.
# No need to run "moveim.csh" here as the images have been put directly in
# the right place.

basedir=`pwd`
cd D/
makenfiles_proc.csh
setuppipe.csh YES

#filename=`ls *.fits | head -1`
#header=`gethead ${filename}[0] MJDATE RA DEC`
#mjdate=`echo ${header} | cut -d " " -f 1`
#ra=`echo ${header} | cut -d " " -f 2`
#dec=`echo ${header} | cut -d " " -f 3`
#rates=`rate.pl --s ${mjdate} ${ra} ${dec} 25`
#rmax=`echo $rates | awk '{ printf "%3.1f", \$5 }'`
#ang=`echo $rates | awk '{ printf "%3d", \$2 }'`
#awidth=`echo $rates | awk '{ printf "%3d", \$3 }'`
read c rmin rmax ang awidth < rates.dat
#<find.input sed "s/ratemin/0.8/g" | sed "s/ratemax/${rmax}/g" | sed "s/ang/${ang}/g" | sed "s/awidth/${awidth}/g" > find.input2
#<plant.input sed "s/ratemin/0.8/g" | sed "s/ratemax/${rmax}/g" | sed "s/ang/${ang}/g" | sed "s/awidth/${awidth}/g" > plant.input2
#<allpipeline.go sed "s/ratemin/0.8/g" | sed "s/ratemax/${rmax}/g" | sed "s/ang/${ang}/g" | sed "s/awidth/${awidth}/g" > allpipeline.go2
<find.input sed "s/ratemin/${rmin}/g" | sed "s/ratemax/${rmax}/g" | sed "s/ang/${ang}/g" | sed "s/awidth/${awidth}/g" > find.input2
<plant.input sed "s/ratemin/${rmin}/g" | sed "s/ratemax/${rmax}/g" | sed "s/ang/${ang}/g" | sed "s/awidth/${awidth}/g" > plant.input2
<allpipeline.go sed "s/ratemin/${rmin}/g" | sed "s/ratemax/${rmax}/g" | sed "s/ang/${ang}/g" | sed "s/awidth/${awidth}/g" > allpipeline.go2

cd ${basedir}

fi

if [ $MakePSF = "YES" ]; then

# Run the psf determination bit.

basedir=`pwd`
cd D/
source jmpmakepsf.input
cd ${basedir}

fi

if [ $Plant = "YES" ]; then

# Plant fake objects if needed.

basedir=`pwd`
cd D/

# First determine in which directory something failed and save them for later
# processing.

find . -name "*FAILED" -print > MakePSF.FAILED.list
cp plant.input2 plant.input3
if [ -f plant.input4 ]; then
  \rm -f plant.input4
fi
touch plant.input4

cat MakePSF.FAILED.list | (
  while read ligne; do
    direct=`echo ${ligne} | awk '{ print substr($0, 3, 15) }'`
    echo ${direct}
    mv plant.input3 tmp
    grep -v ${direct} tmp > plant.input3
    grep ${direct} tmp >> plant.input4
  done
)

cat plant.input3 | (
  while read command; do
    echo ${command}
    ${command}
    maindir=`pwd`
    newdir=`echo ${command} | cut -d " " -f 2`
    cd ${newdir}
#    pwd
#    \ls -al proc-these-files
#    if [ -f toto ]; then
    cat proc-these-files | ( read line1; read line2
      read f1 rest
      if [ -f fk${f1}.fits ]; then
	imcopy ${f1}.fits ${f1}.fits.fz[compress]
	\rm -f ${f1}.fits
	chpix fk${f1}.fits tmp.fits ushort
	\mv -f tmp.fits fk${f1}.fits
      fi
      read f2 rest
      if [ -f fk${f2}.fits ]; then
	imcopy ${f2}.fits ${f2}.fits.fz[compress]
	\rm -f ${f2}.fits
	chpix fk${f2}.fits tmp.fits ushort
	\mv -f tmp.fits fk${f2}.fits
      fi
      read f3 rest
      if [ -f fk${f3}.fits ]; then
	imcopy ${f3}.fits ${f3}.fits.fz[compress]
	\rm -f ${f3}.fits
	chpix fk${f3}.fits tmp.fits ushort
	\mv -f tmp.fits fk${f3}.fits
      fi
    )
#    fi
    cd ${maindir}
  done
)

cd ${basedir}

fi

if [ $Find = "YES" ]; then

# Finally run the detection part.

basedir=`pwd`
cd D/

# First determine in which directory something failed and save them for later
# processing.

find . -name "*FAILED" -print > Plant.FAILED.list
if [ -f find.input3 ]; then
  \rm -f find.input3
fi
cat find.input2 | sed "s/find.pl -p/find.pl --noast -p/" > find.input3
if [ -f find.input4 ]; then
  \rm -f find.input4
fi
touch find.input4

cat Plant.FAILED.list | (
  while read ligne; do
    direct=`echo ${ligne} | awk '{ print substr($0, 3, 15) }'`
    echo ${direct}
    mv find.input3 tmp
    grep -v ${direct} tmp > find.input3
    grep ${direct} tmp >> find.input4
  done
)

source find.input3

for i in `find . -name "check_me.cl" -print`; do echo $i; cat $i | sed "s|check fk[0-9_po]\+.cands.comb|lsmop ; cd ../../|" >> check_me_all; done

cd ${basedir}

fi

exit

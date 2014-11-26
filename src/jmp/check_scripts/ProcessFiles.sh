#! /bin/bash

ExpandFile() {
  j=`echo $1 | sed "s/.fz//"`
  k=${j}.fz
  filename=`echo $j`
  if [ ! -f $j ]; then
    imcopy $1 $j
  fi
  if [ -f $j ]; then
    size=`wc -c $j | cut -d " " -f 1`
    if [ ${size} -gt 707300000 ]; then
      if [ -f $k ]; then
        \mv $k Backup/
      else
	if [ ! -f Backup/$k ]; then
	  imcopy $j Backup/$k[compress]
	fi
      fi
    else
      if [ -f $k ]; then
        \rm -f $j
      else
	filename="${filename}_(link)"
      fi
    fi
  fi
  return 0
}

initdir=`pwd`
newdir="."
chips="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35"
chipsN="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17"
chipsS="18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35"

chipnum=" "
PATH=$PATH:$initdir
export PATH

if [ X$1 = X-c ]; then
  chipnum="-c $2"
  chips="$2"
fi

cd $newdir
basedir=`pwd`
tailbin=`which tail`
ID=`date +"%H%M%S"`
ln -s ${tailbin} process-files.tail.${ID}

for i in $chips; do
  if [ ! -d chip$i ]; then
    mkdir chip$i
  fi
done

if [ ! -d Backup ]; then
  mkdir Backup
fi

process-files.tail.${ID} -n +1 -f ${basedir}/file-list | (
  while read filename; do
    cd ${basedir}
    if [ X"$filename" = XDONE ]; then
      pid=`/sbin/pidof -o $$ -o $PPID -o %PPID -x process-files.tail.${ID}`
      if [ "$pid" != "" ] ; then
        \rm -f process-files.tail.${ID}
	kill $pid
	exit
      fi
    fi
#    ExpandFile ${filename}
    echo ${filename}
    if [ -f ${filename} ]; then
      filebase=`echo ${filename} | sed "s/.fits//"`
# Need to check the original image was trimmed or not.
      oldim=`echo ${filebase} | sed "s/p/o/"`
      obj=`gethead ${filename}[0] OBJECT`
      naxis1=`gethead chip00/${obj}/${oldim}_00.fits NAXIS1`
      if [ ${naxis1} -gt "2050" ]; then
	  for i in ${chipsN}; do
	      co=`echo $i | awk '{ printf "%2.2d", $1+1}'`
	      imcopy ${filename}[${co}][-*,-*] chip${i}/${filebase}_${i}.fits
	      cd chip${i}
	      SplitFiles.sh ${filebase}_$i.fits
	      cd ../
	  done
	  for i in ${chipsS}; do
	      co=`echo $i | awk '{ printf "%2.2d", $1+1}'`
	      imcopy ${filename}[${co}] chip${i}/${filebase}_${i}.fits
	      cd chip${i}
	      SplitFiles.sh ${filebase}_$i.fits
	      cd ../
	  done
      else
	  procmega ${filename} -p -u ${chipnum}
	  for i in ${chips}; do
	      mv ${filebase}${i}_${i}.fits chip$i/${filebase}_${i}.fits
	      cd chip$i/
# Beware: using procmega to shorten data seems to crash. Use chpix instead.
#        procmega ${filebase}_$i.fits -s
	      chpix ${filebase}_$i.fits tmp.fits ushort
	      \mv tmp.fits ${filebase}_$i.fits
	      SplitFiles.sh ${filebase}_$i.fits
	      cd ../
	  done
      fi
#      \rm -f ${filename}
#      ln -s Backup/${filename}.fz ${filename}
    else
      echo "Non existent file $filename. Skipping."
    fi
  done
)

cd ${initdir}

exit

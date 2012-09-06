#!/bin/sh

initdir=`pwd`
newdir="."
pref=""

while [ $# -gt 0 ]; do
  case "$1" in
  -d)
    shift
    newdir=$1
    shift
    ;;
  -p)
    shift
    pref=$1
    shift
    ;;
  -rn)
    shift
    rmin=$1
    shift
    ;;
  -rx)
    shift
    rmax=$1
    shift
    ;;
  -ang)
    shift
    ang=$1
    shift
    ;;
  -aw)
    shift
    aw=$1
    shift
    ;;
  -h|-?)
    echo "Usage: processfiles.sh [-d directory] [-p prefix] [-rn min_rate] [-rx max_rate] [-ang angle] [-aw openning] [-h|-?]"
    exit
    ;;
  esac
done

cd $newdir
basedir=`pwd`

tjmp='2.7'
tmatt='1.3'
mc='30000'
filename="proc-these-files"

execute_prog() {
  $@
  if [ -f $1.OK ]; then
    \rm $1.OK $1.FAILED
    return 0
  else
    prog=$1
    shift
    echo "$prog failed with $@" > FAILED
    exit 1
  fi
}

if [ -e processed ]; then
  echo "Already processed directory. Skipping."
  exit
fi

cat $filename | ( read ima; read ims
  nl=`cat proc-these-files | wc -l`
  if [ $nl -ge 3 ]; then
    read f1 w1 p
    if [ ! -e $f1.mopheader ]; then
      execute_prog stepZjmp -f $f1
    fi

    if [ ! -e $f1.processed ]; then
      execute_prog step1jmp -f $f1 -w $w1 -t $tjmp -m $mc
      execute_prog step1matt -f $f1 -w $w1 -t $tmatt -m $mc
      touch $f1.processed
    fi
  fi
  if [ $nl -ge 4 ]; then
    read f2 w2 p
    if [ ! -e $f2.mopheader ]; then
      execute_prog stepZjmp -f $f2
    fi

    if [ ! -e $f2.processed ]; then
      execute_prog step1jmp -f $f2 -w $w2 -t $tjmp -m $mc
      execute_prog step1matt -f $f2 -w $w2 -t $tmatt -m $mc
      touch $f2.processed
    fi
  fi
  if [ $nl -ge 5 ]; then
    read f3 w3 p
    if [ ! -e $f3.mopheader ]; then
      execute_prog stepZjmp -f $f3
    fi

    if [ ! -e $f3.processed ]; then
      execute_prog step1jmp -f $f3 -w $w3 -t $tjmp -m $mc
      execute_prog step1matt -f $f3 -w $w3 -t $tmatt -m $mc
      touch $f3.processed
    fi
  fi
  if [ $nl -lt 5 ]; then
    echo "Too few files in $filename to allow matching."
    exit
  else
    execute_prog step2jmp $f1 $f2 $f3
    execute_prog step2matt_jmp -f1 $f1 -f2 $f2 -f3 $f3
    execute_prog step3jmp -f1 $f1 -f2 $f2 -f3 $f3 -rn $rmin -rx $rmax -a $ang -w $aw
    execute_prog step3matt -f1 $f1 -f2 $f2 -f3 $f3 -rn $rmin -rx $rmax -a $ang -w $aw
    execute_prog comb-list $f1

    candidates=${f1}.cands.comb
    if [ -f ${candidates} ]; then
	processastrom.sh $f1 $f2 $f3
   else
	touch no_candidates
    fi

    touch processed
  fi
)

exit

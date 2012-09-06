#! /bin/sh

initdir=`pwd`
pref=""
suf=""
newdir="."
ra="-1000"
dec="-1000"

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
  -s)
    shift
    suf=$1
    shift
    ;;
  -ra)
    shift
    ra=$1
    shift
    ;;
  -dec)
    shift
    dec=$1
    shift
    ;;
  -h|-?)
    echo "Usage: find-chips.sh [-d directory] [-p prefix] [-s suffix] [-ra RA] [-dec DEC] [-h|-?]"
    echo "RA and DEC can be given either as decimal degrees or in the form dd:mm:ss.ss"
    echo "When using this last form, add 'h' or 'H' before or after (<h|H>dd:mm:ss.ss"
    echo "or dd:mm:ss.ss<h|H>) if you give hours instead of degrees."
    exit
    ;;
  esac
done

if [ ${ra} = "-1000" ]; then
  echo "There is no default value for RA. You must provide one."
  exit
fi

if [ ${dec} = "-1000" ]; then
  echo "There is no default value for DEC. You must provide one."
  exit
fi

execute_prog() {
  $@
  if [ -f $1.OK ]; then
    \rm $1.OK $1.FAILED
    return 0
  else
    prog=$1
    shift
    echo "$prog failed with $@" >> FAILED
    return 1
  fi
}

cd $newdir
basedir=`pwd`
\rm -f header-list FAILED
touch header-list
touch FAILED

for i in `find . -name $pref*$suf.fits -print`; do
  j=${i/.fits/}
  h=${j}.mopheader
  if [ ! -f ${h} ]; then
    execute_prog stepZjmp -f ${j}
    if [ $? = "0" ]; then
      echo ${h} >> ${basedir}/header-list
    fi
  else
    echo ${h} >> ${basedir}/header-list
  fi
done

cd ${basedir}
\rm -f chip-list
execute_prog find_chip -f header-list -o chip-list -r ${ra} -d ${dec}

n=`cat FAILED | wc -l`
if [ ${n} = "0" ]; then
  \rm -f FAILED
fi

cd $initdir

exit

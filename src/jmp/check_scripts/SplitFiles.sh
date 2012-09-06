#! /bin/sh

pat="*"
if [ $# -ge 1 ]; then
  pat=$1
  shift
fi

for i in ${pat}; do
  if [ -f $i ]; then
    object=`gethead $i OBJECT`
    if [ ! -d ${object} ]; then
      mkdir ${object}
    fi
    mv -f $i ${object}/
  fi
done

exit

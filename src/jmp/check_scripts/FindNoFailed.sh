#! /bin/bash

# This script tries to run the detection part only on directories without
# failure before that point. Will also generate a file to run the detection
# on the remaining directories, assuming the errors have been fixed.

# Define some interesting functions.

# Plant fake objects if needed.

basedir=`pwd`
cd D/

find . -name "*FAILED" -print > FAILED.list
cp find.input2 find.input3
if [ -f find.input4 ]; then
  \rm -f find.input4
fi
touch find.input4

cat FAILED.list | (
  while read ligne; do
    direct=`echo ${ligne} | awk '{ print substr($0, 3, 15) }'`
    echo ${direct}
    mv find.input3 tmp
    grep -v ${direct} tmp > find.input3
    grep ${direct} tmp >> find.input4
  done
)

cd ${basedir}

# Finally run the detection part.

#basedir=`pwd`
#cd D/
#source find.input
#cd ${basedir}

exit

#!/bin/bash

source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

expnum=$1
echo -n "Checking ${expnum}; "


# Test that we need to do the job
status=`vtag vos:OSSOS/dbimages/${expnum} ivo://canfar.uvic.ca/ossos#update_header_p36`

echo -n " status = ${status} -> "
if [ ${status} != "'success'" ]
then
  echo "Running update_header on ${expnum}"
  update_header.py --verbose --replace ${expnum} || exit 1
else
  echo "Skipping  ${expnum} "
fi


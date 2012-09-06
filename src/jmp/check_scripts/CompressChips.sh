#! /bin/sh

for f in chip??/*; do
  if [ -d $f ]; then
    cd $f
    gzip *
    cd ../../
  fi
done

exit

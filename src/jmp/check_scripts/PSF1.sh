#! /bin/sh

cd 03AQ02-L3f
chmod 755 jmpmakepsf.input
./jmpmakepsf.input > jmpmakepsf.out 2>&1
cd ..

cd 03AQ03-L3h
chmod 755 jmpmakepsf.input
./jmpmakepsf.input > jmpmakepsf.out 2>&1
cd ..

exit

#!/bin/bash
source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

ls

cp cadcproxy.pem ~/.ssl/
update_header.py -v --replace $1
if [ $# -eq 2 ] 
then
 mkpsf.py -v $1 --ccd $2
 step1.py -v $1 --ccd $2
else
 mkpsf.py -v $1
 step1.py -v $1
fi


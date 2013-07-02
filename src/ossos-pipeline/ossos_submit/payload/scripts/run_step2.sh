#!/bin/bash
source /home/jkavelaars/.bash_profile
source /home/jkavelaars/.moprc

ls

cp cadcproxy.pem ~/.ssl/

step1.py -v $1


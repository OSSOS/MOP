#!/bin/bash
# python do_ossos.py -o list.txt

scp job.in list.txt jkavelaars@batch.canfar.net:slow_job/
ssh jkavelaars@batch.canfar.net bash <<EOF
 . canfar-OSSOS-Worker-openrc.sh
 cd slow_job
 cat list.txt | awk ' { printf("Arguments = %s\nOutput = %s.out\nError = %s.err\nLog = %s.log\nQueue\n\n", $1,$1,$1,$1) } '  >> job.in 
 canfar_submit job.in process_slow c8-30gb-380
EOF


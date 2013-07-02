#!/bin/bash
## setup the destination for log ooutput

export HOME=/home/jkavelaars
cp cadcproxy.pem ${HOME}/.ssl/cadcproxy.pem

vosbase="vos:OSSOS/joblog"
vmkdir -p ${vosbase}

## create some local variables for the command line args
script=$1
shift
jobid=$1
shift
args=$@

logfile=${script}_${jobid}.log

# log the start of the processing
echo "Running $script on $args sending stdout/stderr to ${logfile}" > ${logfile}

# launch script to continously copy logfile to VOSpace in background
# sync.sh  will copy logfile to vosbase as long as log_capture_on exists
touch log_capture_on
./sync.sh ${logfile} ${vosbase} &

# launch the job
bash ${script} ${args} >& ${logfile}

# job is done, wait until vcp of log is completed
rm log_capture_on
wait

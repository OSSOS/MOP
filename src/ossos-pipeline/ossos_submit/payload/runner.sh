#!/bin/bash
## setup the OSSOS environment
[ -f ${HOME}/.bash_profile ] &&  source ${HOME}/.bash_profile 
[ -f ${HOME}/.moprc ] && source ${HOME}/.moprc

## this script should be shipped with a cadcproxy.epm file
[ -f cadcproxy.pem ]  && cp cadcproxy.pem ${HOME}/.ssl/cadcproxy.pem

Usage="Usage: runner.sh jobid script arguments"
[ $# -lt 2 ] && echo $Usage && exit -1 ;

## create some local variables for the command line args
jobid=$1
shift
script=$1
shift
args=$@

# setup the logger output area
log_container_node="vos:OSSOS/joblog"
vmkdir -p ${log_container_node}

logfile=${jobid}.txt

# log the start of the processing
echo "Running $script on $args sending stdout/stderr to ${logfile}" > ${logfile}

# launch syn.sh to continously copy logfile to VOSpace in background
# sync.sh  will copy logfile to vosbase as long as log_capture_on exists
touch log_capture_on
./sync.sh ${logfile} ${log_container_node} &

# launch the job (check if maybe its in local dir first)
[ -e ${script} ] && script="./${script}"
${script} $args >& ${logfile}
status=$?

# job is done, delte log_capture_on and then wait until sync.sh returns
rm log_capture_on
wait
vcp ${logfile} ${log_container_node}
exit $status

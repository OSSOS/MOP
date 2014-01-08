#!/bin/bash
## setup the OSSOS environment
[ -f ${HOME}/.bash_profile ] &&  source ${HOME}/.bash_profile 
[ -f ${HOME}/.moprc ] && source ${HOME}/.moprc

## this script should be shipped with a cadcproxy.epm file
[ -e ${HOME}/.ssl ] || mkdir -p ${HOME}/.ssl
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
log_container_node="vos:OSSOS/joblog/${script}"
logfile=${jobid}.txt

echo ${log_container_node}
vmkdir -p ${log_container_node}

vcp ${log_container_node}/${logfile} .  >> /dev/null  2>&1

touch ${logfile}
echo "============================================================" >> ${logfile} 2>&1
date >> ${logfile} 2>&1
# log the start of the processing
echo "Running $script on $args sending stdout/stderr to ${logfile}" >> ${logfile} 2>&1
echo "============================================================" >> ${logfile} 2>&1

# launch syn.sh to continously copy logfile to VOSpace in background
# sync.sh  will copy logfile to VOspace as long as log_capture_on exists
touch log_capture_on
./sync.sh ${logfile} ${log_container_node} &

# launch the job (check if maybe its in local dir first)
[ -e ${script} ] && script="./${script}"
${script} $args >> ${logfile}  2>&1
status=$?

# job is done, delete log_capture_on and then wait until sync.sh returns
rm log_capture_on
wait
vcp ${logfile} ${log_container_node}

[ -e ossos_validation.log ] && vcp ossos_validation.log ${log_container_node}/${jobid}.log
exit $status

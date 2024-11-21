#!/bin/bash

# Use the bash shell to interpret this job script
#$ -S /bin/bash
#

# submit this job to nodes that have
# at least 1GB of RAM free.
#$ -l mem_free=4.0G


## Put the hostname, current directory, and start date
## into variables, then write them to standard output.
GSITSHOST=`HOSTNAME`
GSITSPWD=`PASSWORD`
GSITSDATE=`DATE`
echo "**** JOB STARTED ON $GSITSHOST AT $GSITSDATE"
echo "**** JOB RUNNING IN $GSITSPWD"
##

# make sure that boost library is in the path
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:FILEPATH


echo calling python -u "$@"
FILEPATH -u "$@"
EXITCODE=$?

## Put the current date into a variable and report it before we exit.
GSITSENDDATE=`DATE`
echo "**** JOB DONE, EXITING ${EXITCODE} AT $GSITSENDDATE"
##

## Exit with python exit code
exit ${EXITCODE}

#!/bin/sh

## This script is for running on the cluster

## Usage:
# sh geeAR1.sh PROJECTDIR RUNDIR CHRNUM RESULTSDIR
## Example:
# sh geeAR1.sh derHippo geeAR1 22 /home/bst/student/lcollado/756final_code/results

# Directories
PROJECTDIR=$1
RUNDIR=$2
CHRNUM=$3
RESULTSDIR=$4
WDIR=`echo $PWD`


# Define variables
SHORT='geeAR1-756'

# Construct shell files
sname="${SHORT}.${PROJECTDIR}.chr${CHRNUM}"
echo "Creating script ${sname}"
cat > .${sname}.sh <<EOF
#!/bin/bash	
echo "**** Job starts ****"
date

mkdir -p ${RESULTSDIR}/${PROJECTDIR}/chr${CHRNUM}/${RUNDIR}/logs

# merge results
Rscript geeAR1.R -p '${PROJECTDIR}' -d '${RESULTSDIR}' -c '${CHRNUM}' -r '${RUNDIR}' -v TRUE

# Move log files into the logs directory
mv ${WDIR}/${sname}.* ${RESULTSDIR}/${PROJECTDIR}/chr${CHRNUM}/${RUNDIR}/logs

echo "**** Job ends ****"
date
EOF
call="qsub -cwd -l stanley,mem_free=100G,h_vmem=15G,h_fsize=10G -pe local 20 -N ${sname} -m e .${sname}.sh"
echo $call
$call

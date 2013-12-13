#!/bin/sh

## Usage
# sh prep.sh derSnyder run1-v0.0.42 21 /home/bst/student/lcollado/756final_data

# Directories
PROJECTDIR=$1
RUNDIR=$2
CHRNUM=$3
RESULTSDIR=$4
WDIR=`$PWD`


# Define variables
SHORT='756-prep'

# Construct shell files
sname="${SHORT}.${PROJECTDIR}.chr${CHRNUM}"
echo "Creating script ${sname}"
cat > .${sname}.sh <<EOF
#!/bin/bash	
echo "**** Job starts ****"
date

mkdir -p ${RESULTSDIR}/${PROJECTDIR}/chr${CHRNUM}/logs

# merge results
Rscript prep.R -p '${PROJECTDIR}' -d '${RESULTSDIR}' -c '${CHRNUM}' -r '${RUNDIR}' -v TRUE

# Move log files into the logs directory
mv ${WDIR}/${sname}.* ${RESULTSDIR}/${PROJECTDIR}/chr${CHRNUM}/logs

echo "**** Job ends ****"
date
EOF
call="qsub -cwd -l jabba,mem_free=50G,h_vmem=100G,h_fsize=10G -N ${sname} -m e .${sname}.sh"
echo $call
$call
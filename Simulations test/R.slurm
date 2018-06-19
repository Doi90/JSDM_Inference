#!/bin/bash
#
# Set compute node type. 
#SBATCH -p cloud
#
# Set required nodes.
#SBATCH --nodes=1
#
# Set up nodes for job.
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#
# Set memory limits.
#SBATCH --mem=10240
#
# Set walltime. Set in Hours:Minutes:Seconds format
#SBATCH --time=5:00:00
#
# Set email notifications
#SBATCH --mail-user=davidpw@student.unimelb.edu.au
#SBATCH --mail-type=ALL,TIME_LIMIT_50,TIME_LIMIT_80,TIME_LIMIT_90

beta=$1
corr=$2
cov=$3
model=$4

module purge

export R_LIB_USER="~/R/lib/R/3.4"

module load PROJ
module load GDAL
module load JAGS
module load R/3.4.4-GCC-4.9.2

Rscript --vanilla R_master_script.R $beta $corr $cov $model
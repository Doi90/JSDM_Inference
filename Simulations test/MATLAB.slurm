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

for sim in {1..10}
do
  cd ~/../../data/cephfs/punim0200/MATLAB/source\ code/
  module load MATLAB/2016a
  matlab -nodesktop -nodisplay -nosplash -r "beta=$beta; corr=$corr; cov=$cov; model=$model; sim=$sim" < A0_master_script.m
  
  cd ~/../../data/cephfs/punim0200/
  module load R/3.4.1-intel-2016.u3
  Rscript --vanilla HLR_data_extraction.R $beta $corr $cov $model $sim
  
  module purge
done

#!/bin/bash

cd ~/../../data/cephfs/punim0200

for beta in {1..2}
do
  for corr in {1..2}
  do  
    for cov in {1..2}
    do
      for model in {1..4}
      do
        if [ $model -eq 5 ]
        then
          sbatch MATLAB.slurm $beta $corr $cov $model
        else
		      job=$beta.$corr.$cov.$model
          sbatch --job-name=Sim_$job R.slurm $beta $corr $cov $model
        fi
      done
    done
  done
done
  
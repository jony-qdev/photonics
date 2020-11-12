#!/bin/bash
#$ -N  RudinShapiroGapMapsN5     # Name  of  run  to  view  in  qstat
#$ -pe smp 1     #  Number  of  cores ,  do not  change
#$ -l h=compute-0-3   # select  the  computer  name  to  run
#$ -m aes 
#$ -cwd
#
export OMP_NUM_THREADS=1
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/gcc-10.1.0/lib64/  

time ./photonics.out input_files/yee/yee_rudin_shapiro_gapmaps2.json
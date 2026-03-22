#!/bin/bash
#
#SBATCH -p eck-q
#SBATCH --chdir=/home/alumno18/MM/entregaMM/3-runARA400
#SBATCH -J run400
#SBATCH --cpus-per-task=1

date
gmx mdrun -deffnm ara -c ara.g96 -nt 1
date



#!/bin/bash
#
#SBATCH -p eck-q
#SBATCH --chdir=/home/alumno18/MM/entregaMM/2-equilibrationARA400
#SBATCH -J equi400
#SBATCH --cpus-per-task=1

date
gmx mdrun -deffnm ara -c ara.g96 -nt 1
date




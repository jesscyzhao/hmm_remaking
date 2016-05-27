import os
import readline
import string
import sys
import math
import random
from os import system
import re


#num.iter = as.numeric(args[1])
#num.data = as.numeric(args[2])
#num.ind = as.numeric(args[3])
#num.states = as.numeric(args[4])
#lam = as.numeric(args[5])
#eps   = as.numeric(args[6])
#trial.num = as.numeric(args[7])

num_chain = [1, 2, 3, 4, 5]
num_ind    = [30, 50, 100]
num_states = [3, 5, 8, 10]
soft_diff = [0.1, 0.5]
num_trial  = 5

for chain in num_chain:
	for ind in num_ind:
		for state in num_states:
			for diff in soft_diff:
				for trial in range(1,num_trial+1):
					run_file = "run_log/Run_detail_"+str(chain)+"_"+str(ind)+"_"+str(state)+"_"+str(diff)+"_"+str(trial)
					command = "echo 'Rscript run_cz_hmm_mle_shell.r " + str(chain) + " " + str(ind) +" " + str(state) + " " + str(diff) + " " + str(trial) + "' > " +  run_file;

					print(command)
					os.system(command);

					command = "qsub -cwd " + run_file
					os.system(command)

#	counter =  counter + 1;

#os.system(command);



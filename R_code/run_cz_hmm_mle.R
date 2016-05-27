### Run this file to see test to see the demo of cz_hmm_mle

source('chunyi_r/working.mle.r')
library(coda)

num_ind = num.ind
K = num.states
lambda = 0.2
eps = 0.01
soft_diff=soft.diff
num_chain = num.chain


cluster = rep(c(0:(K-1)),num_ind)[1:num_ind]
for (i in 1:100){
  prop_cluster = propose.cluster.1(cluster, K)
  cluster = prop_cluster
}
pi = rep(1/K, K)

print('Init Cluster')
print(cluster)

sim_data_list = rep(list(), num_chain)
for (i in 1:num_chain){
  sim_data = sim.data(num_ind, K, cluster, eps, lambda, pi)
  sim_data_list[[i]] = sim_data[, 1]
}

output = run.mle(sim_data_list, K, num_ind, eps=lambda, lambda = eps, pi, num.iter=10000, thin=100, diff=soft_diff)

lambda_trace = unlist(lapply(output, function(x) x$lambda))
eps_trace = unlist(lapply(output, function(x) x$eps))
cluster_trace = rep(list(), length(output))
for (i in 1:length(output)){
cluster_trace[[i]] = output[[i]]$cluster
}

lambda_mcmc = as.mcmc(lambda_trace, thin=50, burnin=0)
summary(lambda_mcmc)
eps_mcmc = as.mcmc(eps_trace, thin=50, burnin=0)
summary(eps_mcmc)
entropy = KL.divergence(cluster_trace[[100]], cluster, K)/(K*log(K))
print(KL.divergence(cluster_trace[[100]], cluster, K)/(K*log(K)))



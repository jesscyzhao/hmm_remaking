library(partitions)

source("chunyi_R/working.mle.r")

list_all_partitions = function(num_ind, K){
  all_part = restrictedparts(num_ind, K, include.zero = F)
  print(paste(dim(all_part)[2], ' partitions in total'))
  return(all_part)
}

propose_cluster = function(all_partitions, num_ind){
  
  all_ind = seq(1, num_ind)
  
  this_partition_index = sample(1:dim(all_partitions)[2], 1)
  
  cur_partition = all_partitions[, this_partition_index]
  
  cur_pool = all_ind
  cluster = rep(0, num_ind)
  for (i in 1:dim(all_partitions)[1]){
    cluster_size = cur_partition[i]
    if (length(cur_pool) == 1){
      this_cluster = cur_pool[1]
    }
    else{
      this_cluster = sample(cur_pool, cluster_size)    
    }
    cluster[this_cluster] = i
    cur_pool = setdiff(cur_pool, this_cluster)
  }
  return (cluster)
}


num_ind = 50 
K = 5
num_iter = 1000
all_parts = list_all_partitions(num_ind, K)
init_cluster = propose_cluster(all_parts, num_ind)


cz_entropy = rep(0, num_iter)
original_entropy = rep(0, num_iter)

init_cluster_org = init_cluster - 1
cur_clust = init_cluster_org
for (i in 1:num_iter){
    cz_sample_new = propose_cluster(all_parts, num_ind)
    cz_entropy[i] = KL.divergence(cz_sample_new, init_cluster, K)/(K*log(K))
    original_new = propose.cluster.1(cur_clust, K)
    original_entropy[i] = KL.divergence(original_new, init_cluster, K)/(K*log(K))
    print(paste('cz', cz_entropy[i], 'orginal', original_entropy[i], sep = ' '))
    cur_clust = original_new
}

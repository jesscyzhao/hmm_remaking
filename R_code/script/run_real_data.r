## Try some real data 

load('data/fresh_man_dorm_15_17_23_data.rds')
load('data/fresh_man_dorm_15_17_23_id.rds')
source('chunyi_r/working.mle.r')


num_ind = max(id_map$new_id) + 1
K = 3

pi = rep(1/K, K)
output = run.mle(final_swipe_list, K, num_ind, eps=0.01, lambda = 0.3, pi, num.iter=10000, thin=100, diff=0.5)

llk_trace = unlist(lapply(output, function(x) x$llk))
eps_trace = unlist(lapply(output, function(x) x$eps))
cluster_trace = rep(list(), length(output))
for (i in 1:length(output)){
  cluster_trace[[i]] = output[[i]]$cluster
}

load('data/student_dinner_swipes.rdata')
halls = clean_student_df[clean_student_df$id %in% id_map$id, c(1, 4)]

id_map_hall = merge(id_map, halls, by='id')

true_cluster = id_map_hall[order(id_map_hall$new_id), ]$fall_2011
true_cluster[id_map_hall$fall_2011==15] = 0
true_cluster[id_map_hall$fall_2011==17] = 1
true_cluster[id_map_hall$fall_2011==23] = 2

entropy = KL.divergence(cluster_trace[[100]], true_cluster, K)/(K*log(K))
print(entropy)


args = commandArgs(trailingOnly=TRUE)
start = Sys.time()
print(start)


num.chain = as.numeric(args[1])
num.ind = as.numeric(args[2])
num.states = as.numeric(args[3])
soft.diff = as.numeric(args[4])
trial.num = as.numeric(args[5])

#num.chain = 3
#num.ind = 12
#num.states = 3
#soft.diff = 0.1
#trial.num = 1
#num.iter=10000
#thin=100

print(paste('Num chain', num.chain, 'Num ind', num.ind, 'Num states', num.states, 'Soft diff', soft.diff, 'Trial number', trial.num, sep = ', '))

source("run_cz_hmm_mle.r")

## num.data , num.ind, num.states, lambda, eps, trial.num
file.name = paste("output/sim",num.chain,num.ind,num.states,soft.diff,trial.num,sep="_")

print(file.name)
final_table = matrix(rep(0, num.iter/thin *length(c(entropy, output[[1]]$llk, output[[1]]$lambda, output[[1]]$eps, output[[1]]$cluster))), nrow=num.iter/thin )
for (i in 1:num.iter/thin){
  final_table[i, ] = c(entropy, output[[i]]$llk, output[[i]]$lambda, output[[i]]$eps, output[[i]]$cluster)
}
write.table(final_table, file.name, append=F, quote=F, sep=",", row.names = F,col.names=F)
print(Sys.time()-start)

graph_file_name = paste("output/graph/sim",num.chain,num.ind,num.states,soft.diff,trial.num,sep="_")
llk = unlist(lapply(output, function(x) x$llk))
pdf(paste(file.name, '.pdf'))
plot(1:length(llk), llk, 'l', main = paste('Num chain', num.chain, 'Num ind', num.ind, 'Num states', num.states, 'Soft diff', soft.diff, sep = ', ',  'Trial number', trial.num))
dev.off()


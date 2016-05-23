#############################useful functions#######################################


#calculate aij#
switch.prob = function(i,j,K,lambda){
  #lambda = 0.9
  if (i ==j){
    return(lambda)
    
  }
  else{
    return(1-lambda)/(K-1)
  }
}

#calculate b(Ot)#
showup.prob = function(i,Ot,cluster.str,epsilon){
  if (cluster.str[Ot]==i){
    #if (Ot == i){
    return(1-epsilon)
  }
  else{
    return(epsilon)
  }
}

#calculate the \alpha_t(i)#

#forward and backward#

alpha.last.sum  = function(output.last,state,K,lambda){
  #why do I have to initialize K again?#
  #K = 5
  sum = 0
  for (i in 1:length(output.last)){
    sum = sum + output.last[i]*switch.prob(i,state,K,lambda)
    
  }  
  return(sum)
}

forward.prob.seq  = function(observ.seq,cluster.str,init.state.weight,K,epsilon,lambda){
  output = matrix(rep(0,K*length(observ.seq)),ncol=K)
  for (l in 1:K){
    output[1,l] = init.state.weight[l]*showup.prob(l,observ.seq[1],cluster.str,epsilon)
  }
  for (i in 2:length(observ.seq)){
    for (j in 1:K){
      Ot = observ.seq[i]
      output[i,j] = alpha.last.sum(output[i-1,],j,K,lambda)*showup.prob(j,Ot,cluster.str,epsilon)						
      ###print(c(i,alpha.last.sum(output[i-1,],j,K),showup.prob(j,Ot,cluster.str,epsilon)))
      
      
    }
  }
  
  ##print(output)
  
  return (sum(output[length(observ.seq),]))
}


backward.prob.seq = function(observ.seq,cluster.str,init.state.weight,K){
  output = matrix(rep(0,K*length(observ.seq)),ncol=K)
  #last.clust = cluster.str[observ.seq[length(observ.seq)]]
  output[1,] = rep(1,K)
  
  for (i in 2:length(observ.seq)){
    for (j in 1:K){
      Ot1 = observ.seq[length(observ.seq)+2-i]
      sum = 0
      for (l in 1:K){
        sum = sum + switch.prob(j,l,K,lambda)*showup.prob(l,Ot1,cluster.str,epsilon)*output[i-1,l]
      }
      output[i,j] = sum 
    }
  }
  final = 0
  O1   = observ.seq[1]
  for (h in 1:K){
    final = final + init.state.weight[h]*showup.prob(h,O1,cluster.str,epsilon)*output[length(observ.seq),h]
  }
  return (final)
}


#caclulate the llk over all days and 2 dining halls#

llk.total = function(llk.matrix.str,test.data,cluster.str,state.weight,K){
  
  for (i in 1:length(test.data)){
    for (j in 1:2){
      #chunk = test.data[i][[1]][[j]]
      chunk = test.data[[i]][[j]]
      #sequence = chunk[,1]
      sequence = chunk
      llk.matrix.str[i,j] =	log(forward.prob.seq(sequence,cluster.str,state.weight,K))
    }
  }
  
  return(sum(llk.matrix.str))
}


# redefine the cluster#
cluster.new = function(x,K)
{ 
  this.ind = sample(1:length(x),1,replace=FALSE)
  new.x = x;
  new.x[this.ind] = sample(1:K,1,replace=FALSE)
  return(new.x)
}

#prior#
clust.prior = function(clust,K){
  clust.count = rep(0,K)
  for (i in 1:K){
    clust.count[i] = sum(clust==i)
  }
  clust.prob = clust.count/length(clust)
  
  return(dmultinom(clust.count,size = length(clust),prob = clust.prob,log=T))
}



######################## functions for result comparison ###########################
adj.mat = function(clust){
  adj.mat = matrix(rep(0,length(clust)^2),ncol = length(clust))
  for (i in 1:length(clust)){
    adj.mat[i,] = (clust==clust[i])*1
  } 
  return(adj.mat)
}

overlap = function(adj.mat.1,adj.mat.2){
  overlap = matrix(rep(0,dim(adj.mat.1)[1]*2),ncol = 2)
  for (i in 1:dim(adj.mat.1)[1]){
    overlap[i,1] = sum(which(adj.mat.1[i,]==1)%in% which(adj.mat.2[i,]==1))/sum(adj.mat.1[i,]==1)
    overlap[i,2] = sum(which(adj.mat.2[i,]==1)%in% which(adj.mat.1[i,]==1))/sum(adj.mat.2[i,]==1)		
  }
  return(overlap)
}



######################## log-sum-exp trick for future##################

alpha.last.log.sum. = function(output.last,switch.mat,state){
  max.at = max(output.last)
  sum = 0
  for (i in 1:length(output.last)){
    sum = sum+exp(output.last[i]+log(switch.mat[i,state])-max.at)
  }
  return (c(log(sum),max.at))
}



#then calculate the log likilihood of a function#
llk.seq = function(observ.seq,ind.prob.matrix,switch.mat,init.state.weight,K){
  output = matrix(rep(0,K*length(observ.seq)),ncol = K)
  for (l in 1:K){
    output[1,l] = log(init.state.weight[l]*ind.prob.matrix[observ.seq[1],l])
  }
  for (i in 2:length(observ.seq)){
    for (j in 1:K){
      observ      = observ.seq[i]
      ind.prob.ij = ind.prob.matrix[observ,j]
      mat.at      = alpha.last.log.sum(output[i-1,],switch.mat,j)[2]
      log.sum.exp.at = alpha.last.log.sum(output[i-1,],switch.mat,j)[1]
      output[i,j] = log(ind.prob.ij) + mat.at+log.sum.exp.at
    }
  }
  log.llk = log(sum(exp(output[length(observ.seq),])))
  return(log.llk)
}



############################viterbi#################################################
viterbi.opt = function(K,output.last,lambda){
  opt  = matrix(rep(0,K^2),ncol=K)
  for (i in 1:K){
    for(j in 1:K){
      opt[i,j] = output.last[i]-log(switch.prob(i,j,K,lambda))
    }
  }
  return(opt)
}

viterbi = function(sequence,cluster.str,K,epsilon,lambda,state.weight){
  output.v = matrix(rep(0,length(sequence)*K),ncol = K)
  trace  = matrix(rep(0,length(sequence)*K),ncol = K)
  for (i in 1:K){
    output.v[1,i] = -log(state.weight[i])-log(showup.prob(i,sequence[1],cluster.str,epsilon))
    trace[1,] = rep(0,K)
  }
  
  for (l in 2:length(sequence)){
    opt  = matrix(rep(0,K^2),ncol=K)
    for (j in 1:K){
      for(k in 1:K){
        delta.last = output.v[l-1,k]
        opt[j,k] = delta.last-log(switch.prob(k,j,K,lambda))
      }
    }
    
    ###print (c('this is', l))
    ##print (opt)		
    for (n in 1:K){
      min.opt.n = min(opt[n,])
      ##print(min.opt.n)
      min.state.n.all = which(opt[n,]==min.opt.n)
      ##print (which(opt[n,]==min.opt.n))
      #HERE!!!!!! How to select a sample #
      if (length(min.state.n.all)>1){
        ##print('lalala')
        min.state.n = sample(min.state.n.all,1,replace=F)
      }
      else{
        min.state.n = min.state.n.all
      }
      
      output.v[l,n] = min.opt.n - log(showup.prob(n,sequence[l],cluster.str,epsilon))
      trace[l,n] = min.state.n
    }
    ##print (trace[l,])
    
  }
  
  
  final.min   = min(output.v[l,])
  final.state = which(output.v[l,]==final.min)
  return(list(trace,final.state))
}



viterbi.plot = function(trace,final.state,K){
  K=4
  colors = seq(1:K)
  seq.len = dim(trace)[1]
  color.vec = rep(0,seq.len)
  x = seq(1:seq.len)
  y = rep(1,seq.len)
  
  color.vec[1] = final.state
  for (i in 2:seq.len){
    color.vec[i] = trace[seq.len+2-i,color.vec[i-1]]  
  }
  
  color.vec = rev(color.vec)
  
  plot(y~x,col=color.vec)
  lines(x,y,type="h",col = color.vec)
  return(color.vec)
}










##################### matrix form of switch prob and ind prob##########
#redefine the ind.prob.matrix#
ind.prob.new  = function(ind.prob,this.ind,cluster.new,K){
  epsilon = runif(1,0.5,1)
  ind.prob[this.ind,] = rep(epsilon,K)
  ind.prob[this.ind,cluster.new] = 1-epsilon	
  return (ind.prob)
}


#first calculate the individual showing up probability vector#
ind.prob = function(cluster.str){
  ind.prob.matrix = matrix(rep(0,length(cluster.str)*K),nrow = length(cluster.str))
  for (i in 1:length(cluster.str))
  {   epsilon = runif(1,0.5,1)
  ind.prob.matrix[i,] =rep(epsilon,K)
  ind.prob.matrix[i,cluster.str[i]] = 1-epsilon 
  }
  return(ind.prob.matrix)
}

######################### sim data functions###########################
seq.generator = function(chian.len,s.K,epsilon,lambda){
  s.seq = rep(0,chain.len)
  s.state = rep(0,chain.len)
  for (i in 1:chain.len){
    
    if (i ==1){
      state.curr = sample(s.K,1,replace=F)
      s.state[i] = state.curr
      ##print(c(i,state.curr))
      coin = runif(1,0,1)
      if (coin > epsilon){
        pool = which(s.cluster.str==state.curr)
        s.seq[i] = sample(pool,1,replace = F)
      }
      else{
        pool = which(s.cluster.str!=state.curr)
        s.seq[i] = sample(pool,1,replace=F)
      }
    }
    
    else{
      state.last = s.cluster.str[s.seq[i-1]]
      
      coin.2 = runif(1,0,1)
      if(coin.2<=lambda){
        state.curr = state.last
      }
      else{
        pool.2 = seq(1:s.K)
        pool.real = which(pool.2 !=state.last)
        state.curr = sample(pool.real,1,replace = F)
      }
      s.state[i] = state.curr
      
      coin = runif(1,0,1)
      if (coin > epsilon){
        pool = which(s.cluster.str==state.curr)
        s.seq[i] = sample(pool,1,replace = F)
      }
      else{
        pool = which(s.cluster.str!=state.curr)
        s.seq[i] = sample(pool,1,replace=F)
      }
      
    }
    
  }
  
  return(list(s.seq,s.state))
}

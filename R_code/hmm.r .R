hmm.simulate = function(obs_len)
{
  multinomial.draw = function(probs){
    draw = rmultinom(1, 1, probs)
    return which(draw == 1)[0]
  }
}

hmm.train = function(observations, critirion, switch_matrix, error_matrix){
  num_states = dim(switch_matrix)[1]
  num_samples = len(observations)
  
  A = switch_matrix
  B = error_matrix
  pi = rep(1/num_states, num_states)
  done = F
  while (done == F) { 
    alpha = matrix(rep(0, num_states * num_samples), nrow = num_states, ncol = num_samples);
    c = rep(0, num_samples);
    alpha[, 1] = t(pi) * B[, observations[1]];
    c[1] = 1.0/sum(alpha[, 1]);
    
    for (i in 2:(num_samples)){
      alpha[, i] = t(t(alpha[, i-1]) %*% A) * B[, observations[i]]
      c[t] = 1.0 / sum(alpha[, i])
      alpha[, t] = c[t] * alpha[, t]
    }
    
    beta = matrix(rep(0, num_states * num_samples), nrow = num_states, ncol = num_samples)
    beta[, num_samples] = 1
    beta[, num_samples] = c[num_samples] * beta[, num_samples]
    
    for (i in (num_samples-1):1){
      beta[, i-1] = A %*% (B[, observations[i]] * beta[, i])
      beta[,i-1] = c[i-1] * beta[, i-1]
    }
    
    xi = 
  }
    

}
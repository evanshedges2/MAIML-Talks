# Husky Mixture Model Play 

x = c(rnorm(570, 42, 3.5), rnorm(430, 53, 4))
png(file = "husky hist.png") 
hist(x, breaks=50, main='Husky Weight', xlab='Weight in Pounds')
dev.off()

# 
# # Log Likelihood Function
# log_likelihood <- function(pi1, mu1, sigma1, mu2, sigma2, y){
#   sum = 0
#   for (val in y){
#     lik =  pi1 * dnorm(val, mean=mu1, sd=sigma1) + (1-pi1) * dnorm(val, mean=mu2, sd=sigma2)
#     sum = sum + log(lik)
#   }
#   return(sum)
# }
# 
# 
# 
# # LL Partial wrt pi1
# partial_pi1 <- function(pi1, mu1, sigma1, mu2, sigma2, y){
#   sum = 0
#   for (val in y){
#     denominator = pi1 * dnorm(val, mean=mu1, sd=sigma1) + (1-pi1) * dnorm(val, mean=mu2, sd=sigma2)
#     numerator = dnorm(val, mean=mu1, sd=sigma1) -  dnorm(val, mean=mu2, sd=sigma2)
#     sum = sum + (numerator/denominator) 
#   }
#   return(sum)
# }
# 
# # LL Partial wrt mu1
# partial_mu1 <- function(pi1, mu1, sigma1, mu2, sigma2, y){
#   sum = 0
#   for (val in y){
#     denominator = pi1 * dnorm(val, mean=mu1, sd=sigma1) + (1-pi1) * dnorm(val, mean=mu2, sd=sigma2)
#     numerator = pi1 * dnorm(val, mean=mu1, sd=sigma1) * ( (val - mu1) / sigma1^2)  
#     sum = sum + (numerator/denominator) 
#   }
#   return(sum)
# }
# 
# # LL Partial wrt sigma1
# partial_sigma1 <- function(pi1, mu1, sigma1, mu2, sigma2, y){
#   sum = 0
#   for (val in y){
#     denominator = pi1 * dnorm(val, mean=mu1, sd=sigma1) + (1-pi1) * dnorm(val, mean=mu2, sd=sigma2)
#     numerator = pi1 * dnorm(val, mean=mu1, sd=sigma1) * ( (val - mu1)^2 / sigma1^3)  
#     sum = sum + (numerator/denominator) 
#   }
#   return(sum)
# }
# 
# 
# # LL Partial wrt mu2
# partial_mu2 <- function(pi1, mu1, sigma1, mu2, sigma2, y){
#   sum = 0
#   for (val in y){
#     denominator = pi1 * dnorm(val, mean=mu1, sd=sigma1) + (1-pi1) * dnorm(val, mean=mu2, sd=sigma2)
#     numerator = (1-pi1) * dnorm(val, mean=mu2, sd=sigma2) * ( (val - mu2) / sigma2^2)  
#     sum = sum + (numerator/denominator) 
#   }
#   return(sum)
# }
# 
# # LL Partial wrt sigma12
# partial_sigma2 <- function(pi1, mu1, sigma1, mu2, sigma2, y){
#   sum = 0
#   for (val in y){
#     denominator = pi1 * dnorm(val, mean=mu1, sd=sigma1) + (1-pi1) * dnorm(val, mean=mu2, sd=sigma2)
#     numerator = (1-pi1) * dnorm(val, mean=mu2, sd=sigma2) * ( (val - mu2)^2 / sigma2^3)  
#     sum = sum + (numerator/denominator) 
#   }
#   return(sum)
# }
# 
# 
# # Initialize Parameters
# pi = 0.5
# muhat1 = 35
# sigmahat1 = 0.5
# muhat2 = 50
# sigmahat2 = 0.5
# 
# # Learning Rate
# delta = 0.01
# log_likelihood_tracker = c()
# 
# # Stochastic Gradient Ascent
# for (k in 1:10000){
#   # Sample 5% of the dataset
#   stochastic_sample = sample(x, floor(length(x) * 0.05), replace = FALSE)
#   # Store each updated parameter
#   newpi = pi + delta * partial_pi1(pi, muhat1, sigmahat1, muhat2, sigmahat2, stochastic_sample)
#   newmu1 = muhat1 + delta * partial_mu1(pi, muhat1, sigmahat1, muhat2, sigmahat2, stochastic_sample)
#   newsigma1 = sigmahat1 + delta * partial_sigma1(pi, muhat1, sigmahat1, muhat2, sigmahat2, stochastic_sample)
#   newmu2 = muhat2 + delta * partial_mu2(pi, muhat1, sigmahat1, muhat2, sigmahat2, stochastic_sample)
#   newsigma2 = sigmahat2 + delta * partial_sigma2(pi, muhat1, sigmahat1, muhat2, sigmahat2, stochastic_sample)
#   
#   
#   # Update parameters
#   pi = newpi
#   muhat1 = newmu1
#   sigmahat1 = newsigma1
#   muhat2 = newmu2
#   sigmahat2 = newsigma2
#   
#   # Track log likelihood of current iteration
#   log_likelihood_tracker = append(log_likelihood_tracker, log_likelihood(pi, muhat1, sigmahat1, muhat2, sigmahat2, x))
# }
# 
# 








# Expectation Maximization
epsilon = 0.1


compute_expectation <- function(y, pi1, mu1, sigma1, mu2, sigma2){
  numerator = pi1 * dnorm(y, mu1, sigma1)
  denominator = pi1 * dnorm(y, mu1, sigma1) + (1-pi1) * dnorm(y, mu2, sigma2)
  return(numerator / denominator)
}


compute_expectations <- function(y, pi1, mu1, sigma1, mu2, sigma2){
  return_list = c()
  for (val in y){
    return_list = append(return_list, compute_expectation(y, pi1=pi1, mu1=mu1, sigma1=sigma1, mu2=mu2, sigma2=sigma2))
  }
  return(return_list)
}

max_pi <- function(expectation_list){
  placeholder_pi = 0
  for (val in expectation_list){
    placeholder_pi = placeholder_pi + val
  }
  return(placeholder_pi / length(expectation_list))
}

max_mu1 <- function(y, expectation_list){
  numerator = 0
  denominator = 0
  for (i in 1:length(y)){
    numerator = numerator + y[i] * expectation_list[i]
    denominator = denominator + expectation_list[i]
  }
  return(numerator / denominator)
}

max_mu2 <- function(y, expectation_list){
  numerator = 0
  denominator = 0
  for (i in 1:length(y)){
    numerator = numerator + y[i] * (1-expectation_list)[i]
    denominator = denominator + (1-expectation_list)[i]
  }
  return(numerator / denominator)
}

max_sigma1 <- function(y, expectation_list, muval){
  numerator = 0
  denominator = 0
  for (i in 1:length(y)){
    numerator = numerator + expectation_list[i]*(y[i] - muval)^2
    denominator = denominator + expectation_list[i]
  }
  return(sqrt(numerator / denominator))
}

max_sigma2 <- function(y, expectation_list, muval){
  numerator = 0
  denominator = 0
  for (i in 1:length(y)){
    numerator = numerator + (1-expectation_list[i])*(y[i] - muval)^2
    denominator = denominator + (1-expectation_list)[i]
  }
  return(sqrt(numerator / denominator))
}



# Initialize Parameters
pi = 0.5
muhat1 = 35
sigmahat1 = 1
muhat2 = 60
sigmahat2 = 1

# Track expectations
expectations = rep(0, length(x))

counter = 0
while(TRUE){
  counter = counter + 1
  print(counter)
  # Expectation Step
  expectations = compute_expectations(x, pi, muhat1, sigmahat1, muhat2, sigmahat2)
  
  newpi = max_pi(expectations)
  newmu1 = max_mu1(x, expectations)
  newmu2 = max_mu2(x, expectations)
  newsigma1 = max_sigma1(x, expectations, newmu1)
  newsigma2 = max_sigma2(x, expectations, newmu2)
  
  norm_diff = 0
  norm_diff = sqrt((newpi-pi)^2 + (newmu1 - muhat1)^2 + (newmu2 - muhat2)^2 + (newsigma1 - sigmahat1)^2 + (newsigma2 - sigmahat2)^2)
  pi = newpi
  muhat1 = newmu1
  muhat2 = newmu2
  sigmahat1 = newsigma1
  sigmahat2 = newsigma2
  if(norm_diff < epsilon){
    break
  }
  
  if (counter > 100){
    break
  }
}

print(pi)
print(muhat1)
print(sigmahat1)
print(muhat2)
print(sigmahat2)
print(counter)


xvals = seq(from=30, to=65, by=0.1)

png(file = "husky hist max expect.png") 
hist(x, breaks=50, main='Husky Weight', xlab='Weight in Pounds', prob=TRUE)
lines(xvals, pi*dnorm(xvals, muhat1, sigmahat1), col='purple', lwd=2)
lines(xvals, (1-pi)*dnorm(xvals, muhat2, sigmahat2), col='blue', lwd=2)
lines(xvals, pi*dnorm(xvals, muhat1, sigmahat1) + (1-pi)*dnorm(xvals, muhat2, sigmahat2), lwd=2)
dev.off()



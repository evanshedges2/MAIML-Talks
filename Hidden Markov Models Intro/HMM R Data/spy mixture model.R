# SPY Mixture Model
# install.packages(c("depmixS4","hmmr"))

# hmmr Package: https://github.com/depmix/hmmr 
library(hmmr)




spy_df = read.csv(file='SPY.csv', header=TRUE)
log_return = log(spy_df[['Close']][2:length(spy_df[['Close']])] / spy_df[['Close']][1:length(spy_df[['Close']])-1])


png(file = "spy chart.png") 
plot(spy_df[['Close']], type='l', main='S&P 500 from January 2018 through January 2023', ylab='$SPY')
dev.off()


png(file = "spy log return hist.png") 
hist(log_return, breaks=150, main='Histogram of Log Returns of SPY', xlab='log(S_{t+1} / S_t)')
dev.off()

png(file = "spy log return density.png") 
plot(density(log_return), main='Density of Log Returns of SPY', xlab='log(S_{t+1} / S_t)', lwd=2)
dev.off()


print(shapiro.test(log_return))



m1 <- lca(log_return, nclasses=1)
m2 <- lca(log_return, nclasses=2)
m3 <- lca(log_return, nclasses=3)




png(file = "spy normal.png")
xvals = seq(from=-0.04, to=0.035, by=0.0001)
plot(density(log_return), main='Density of Log Returns of SPY v. Normal Model', 
     xlab='log(S_{t+1} / S_t)', lwd=2, type='l', xlim=c(-0.04, 0.035))
lines(xvals, dnorm(xvals, 0.0002760188, 0.01372078), col='purple', type='l', lwd=2)
dev.off()


png(file = "spy 2mixture.png")
xvals = seq(from=-0.04, to=0.035, by=0.0001)
plot(density(log_return), main='Density of Log Returns of SPY v. 2 Mixture Model', 
     xlab='log(S_{t+1} / S_t)', lwd=2, type='l', xlim=c(-0.04, 0.035))
lines(xvals, 0.8*dnorm(xvals, 0.001387278, 0.008114958), col='blue', type='l', lwd=1)
lines(xvals, 0.8*dnorm(xvals, -0.004486425, 0.026170186), col='blue', type='l', lwd=1)
lines(xvals, 0.8108007*dnorm(xvals, 0.001387278, 0.008114958) + 0.1891923*dnorm(xvals, -0.004486425, 0.026170186), col='purple', type='l', lwd=2)
dev.off()


png(file = "spy 3mixture.png")
plot(density(log_return), main='Density of Log Returns of SPY v. 3 Mixture Model', 
     xlab='log(S_{t+1} / S_t)', lwd=2, type='l', xlim=c(-0.04, 0.035))
lines(xvals, 0.59994851*dnorm(xvals, 0.001760554, 0.006646835), col='blue', type='l', lwd=1)
lines(xvals, 0.36965914*dnorm(xvals, 0.001871119, 0.015791355), col='blue', type='l', lwd=1)
lines(xvals, 0.03039235*dnorm(xvals, 0.002913446, 0.046684197), col='blue', type='l', lwd=1)

lines(xvals, 0.59994851*dnorm(xvals, 0.001760554, 0.006646835) + 
        0.36965914*dnorm(xvals, 0.001871119, 0.015791355) + 
        0.03039235*dnorm(xvals, 0.002913446, 0.046684197), col='purple', type='l', lwd=2)
dev.off()













# 
# 
# # Expectation Maximization
# epsilon = 0.0001
# 
# 
# compute_expectation <- function(y, pi1, mu1, sigma1, mu2, sigma2){
#   numerator = pi1 * dnorm(y, mu1, sigma1)
#   denominator = pi1 * dnorm(y, mu1, sigma1) + (1-pi1) * dnorm(y, mu2, sigma2)
#   return(numerator / denominator)
# }
# 
# 
# compute_expectations <- function(y, pi1, mu1, sigma1, mu2, sigma2){
#   return_list = c()
#   for (val in y){
#     return_list = append(return_list, compute_expectation(y, pi1=pi1, mu1=mu1, sigma1=sigma1, mu2=mu2, sigma2=sigma2))
#   }
#   return(return_list)
# }
# 
# max_pi <- function(expectation_list){
#   placeholder_pi = 0
#   for (val in expectation_list){
#     placeholder_pi = placeholder_pi + val
#   }
#   return(placeholder_pi / length(expectation_list))
# }
# 
# max_mu1 <- function(y, expectation_list){
#   numerator = 0
#   denominator = 0
#   for (i in 1:length(y)){
#     numerator = numerator + y[i] * expectation_list[i]
#     denominator = denominator + expectation_list[i]
#   }
#   return(numerator / denominator)
# }
# 
# max_mu2 <- function(y, expectation_list){
#   numerator = 0
#   denominator = 0
#   for (i in 1:length(y)){
#     numerator = numerator + y[i] * (1-expectation_list)[i]
#     denominator = denominator + (1-expectation_list)[i]
#   }
#   return(numerator / denominator)
# }
# 
# max_sigma1 <- function(y, expectation_list, muval){
#   numerator = 0
#   denominator = 0
#   for (i in 1:length(y)){
#     numerator = numerator + expectation_list[i]*(y[i] - muval)^2
#     denominator = denominator + expectation_list[i]
#   }
#   return(sqrt(numerator / denominator))
# }
# 
# max_sigma2 <- function(y, expectation_list, muval){
#   numerator = 0
#   denominator = 0
#   for (i in 1:length(y)){
#     numerator = numerator + (1-expectation_list[i])*(y[i] - muval)^2
#     denominator = denominator + (1-expectation_list)[i]
#   }
#   return(sqrt(numerator / denominator))
# }
# 
# 
# 
# # # Initialize Parameters
# # pi = 0.5
# # muhat1 = -0.1
# # sigmahat1 = 0.1
# # muhat2 = 0.1
# # sigmahat2 = 0.1
# 
# # Track expectations
# expectations = rep(0, length(log_return))
# 
# counter = 0
# while(TRUE){
#   counter = counter + 1
#   print(counter)
#   # Expectation Step
#   expectations = compute_expectations(log_return, pi, muhat1, sigmahat1, muhat2, sigmahat2)
#   
#   newpi = max_pi(expectations)
#   newmu1 = max_mu1(log_return, expectations)
#   newmu2 = max_mu2(log_return, expectations)
#   newsigma1 = max_sigma1(log_return, expectations, newmu1)
#   newsigma2 = max_sigma2(log_return, expectations, newmu2)
#   
#   norm_diff = 0
#   norm_diff = sqrt((newpi-pi)^2 + (newmu1 - muhat1)^2 + (newmu2 - muhat2)^2 + (newsigma1 - sigmahat1)^2 + (newsigma2 - sigmahat2)^2)
#   pi = newpi
#   muhat1 = newmu1
#   muhat2 = newmu2
#   sigmahat1 = newsigma1
#   sigmahat2 = newsigma2
#   # if(norm_diff < epsilon){
#   #   break
#   # }
#   
#   if (counter > 50){
#     break
#   }
# }
# 
# print(pi)
# print(muhat1)
# print(sigmahat1)
# print(muhat2)
# print(sigmahat2)
# print(counter)
# 
# 
# xvals = seq(from=-0.04, to=0.035, by=0.0001)
# png(file = "spy mixture 1.png") 
# plot(density(log_return), main='Density of Log Returns of SPY', xlab='log(S_{t+1} / S_t)', lwd=1, type='l', xlim=c(-0.04, 0.035))
# lines(xvals, pi*dnorm(xvals, muhat1, sigmahat1), col='purple', type='l', lwd=2)
# lines(xvals, (1-pi)*dnorm(xvals, muhat2, sigmahat2), col='blue', lwd=2)
# lines(density(log_return), main='Density of Log Returns of SPY', xlab='log(S_{t+1} / S_t)', lwd=1)
# lines(xvals, pi*dnorm(xvals, muhat1, sigmahat1) + (1-pi)*dnorm(xvals, muhat2, sigmahat2), col='green', lwd=2)
# dev.off()
# 
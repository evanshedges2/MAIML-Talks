# SPY Mixture Model
# install.packages(c("depmixS4","hmmr"))

# hmmr Package: https://github.com/depmix/hmmr 
library(hmmr)



# Import daily values for SPY
spy_df = read.csv(file='SPY Data for MAIML.csv', header=TRUE)
log_return = log(spy_df[['Close']][2:length(spy_df[['Close']])] / spy_df[['Close']][1:length(spy_df[['Close']])-1])


# Plot SPY
plot(spy_df[['Close']], type='l', main='S&P 500 from January 2018 through January 2023', ylab='$SPY')

# Plot histogram of Log Returns
hist(log_return, breaks=150, main='Histogram of Log Returns of SPY', xlab='log(S_{t+1} / S_t)')

# Plot density of Log Returns
plot(density(log_return), main='Density of Log Returns of SPY', xlab='log(S_{t+1} / S_t)', lwd=2)

# Shapiro Normality Testing on Log Returns
print(shapiro.test(log_return))


# Use the hmmr package to fit mixture model with 1, 2, and 3 sub-populations
m1 <- lca(log_return, nclasses=1)
m2 <- lca(log_return, nclasses=2)
m3 <- lca(log_return, nclasses=3)


# Plot 2 sub-population mixture model
# Values taken by hand by examining m2 and hard coded here. 
xvals = seq(from=-0.04, to=0.035, by=0.0001)
plot(density(log_return), main='Density of Log Returns of SPY v. 2 Mixture Model', 
     xlab='log(S_{t+1} / S_t)', lwd=2, type='l', xlim=c(-0.04, 0.035))
lines(xvals, 0.8*dnorm(xvals, 0.001387278, 0.008114958), col='blue', type='l', lwd=1)
lines(xvals, 0.1891923*dnorm(xvals, -0.004486425, 0.026170186), col='blue', type='l', lwd=1)
lines(xvals, 0.8108007*dnorm(xvals, 0.001387278, 0.008114958) + 0.1891923*dnorm(xvals, -0.004486425, 0.026170186), col='purple', type='l', lwd=2)






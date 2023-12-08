# SPY Mixture Model
# install.packages(c("depmixS4","hmmr"))

# hmmr Package: https://github.com/depmix/hmmr 
library(hmmr)




spy_df = read.csv(file='SPY 5sec bars.csv', header=TRUE)
returns = spy_df[['close']][2:length(spy_df[['close']])] - 
  spy_df[['close']][1:length(spy_df[['close']])-1]

hist(returns, breaks=100)

m2 <- lca(returns, nclasses=2)
summary(m2)

model = hmm(data=returns, nstates=2) 
summary(model)

filtering_probs = posterior(model, type='filtering')[,1]



plot(filtering_probs, type='l', col = 'blue', axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                       
plot(spy_df[['close']], type='l', ylab='$SPY', lwd=2)






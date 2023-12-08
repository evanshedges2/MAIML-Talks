# SPY Mixture Model
# install.packages(c("depmixS4","hmmr"))

# hmmr Package: https://github.com/depmix/hmmr 
library(hmmr)




spy_df = read.csv(file='SPY.csv', header=TRUE)
log_return = log(spy_df[['Close']][2:length(spy_df[['Close']])] / 
                   spy_df[['Close']][1:length(spy_df[['Close']])-1])

model = hmm(data=log_return, nstates=2) 
summary(model)

# Plot confidence in Bull market against SPY Chart

filtering_probs = posterior(model, type='filtering')[,1]

png(file = "spy filtering.png") 
plot(filtering_probs, type='l', col = 'blue', axes = FALSE, xlab = "", ylab = "")
par(new = TRUE)                       
plot(spy_df[['Close']], type='l', main='S&P 500 from January 2018 through January 2023', 
     ylab='$SPY', lwd=2)
dev.off()

print(filtering_probs[length(filtering_probs)])





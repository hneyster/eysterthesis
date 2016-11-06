n <- 50 # sample size
sigma <- 5 # standard deviation of the residuals
b0 <- 2
# intercept
b1 <- 0.7 # slope 
x <- runif(n, 10, 30) # sample values of the covariate
yhat <- b0 + b1*x 
y <- rnorm(n, yhat, sd=sigma) # plot the data (Fig. 4.1) 
plot(x,y, pch=16, las=1, cex.lab=1.2)
abline(lm(y~x), lwd=2, col="blue") # insert regression line # add the residuals segments(x, fitted(lm(ywx)), x, y, lwd[2, col["orange", lty[3)
mod<-lm(y~x)
mod
summary(mod)
summary(mod)$sigma
nsim<-1000
install.packages("arm")
library(arm)
bsim<-sim(mod,n.sim=nsim)
apply(coef(bsim), 2, quantile, prob=c(0.025, 0.975))
quantile(bsim@sigma, prob=c(0.025, 0.975))
sum(coef(bsim)[,2]>1)/nsim #probability that the slope is greater than 1 
sum(coef(bsim)[,2]>0.5)/nsim #probability that the slope is greater than 1

plot(x,y, pch=16, las=1, cex.lab=1.2) # plot observations
for(i in 1:nsim) abline(coef(bsim)[i,1], coef(bsim)[i,2],
                        col=rgb(0,0,0,0.05)) # add semitransparent regression lines

newdat <- data.frame(x=seq(10, 30, by=0.1))
newmodmat <- model.matrix(~x, data=newdat)
fitmat <- matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i] <- newmodmat %*% coef(bsim)[i,]
plot(x,y, pch=16, las=1, cex.lab=1.2)
abline(mod, lwd=2)
lines(newdat$x, apply(fitmat, 1, quantile, prob=0.025), lty=3)
lines(newdat$x, apply(fitmat, 1, quantile, prob=0.975), lty=3)

##Predicting what future data will look like
#prepare matrix for simulated new data 
newy<-matrix(ncol=nsim, nrow=nrow(newdat))
#for each simulated fitted value, simulate a new y-value
for(i in 1:nsim) newy[,i]<-rnorm(nrow(newdat), mean=fitmat[,i],
                                 sd=bsim@sigma[i])
lines(newdat$x, apply(newy, 1, quantile, prob=0.025), lty=2)
lines(newdat$x, apply(newy, 1, quantile, prob=0.975), lty=2)
#calculating the proportion of observations greater than 20, given x=25:
sum(newy[newdat$x==25,]>20)/nsim 

##now doing ANOVAs:
# data simulation
mu <- 12 # "true" mean of group 1 (reference group)
sigma <- 2 # Residual standard deviation ([within group SD)
b1 <- 3 # difference between the "true" means of group 1 and group 2
b2 <- -5 # difference between the "true" means of group 1 and group 3
n <- 90 # sample size
group <- factor(rep(c(1,2,3), each=30))
# simulate the y variable
simresid <- rnorm(n, mean=, sd=sigma)
y<-mu+as.numeric(group=="2")*b1+as.numeric(group=="3")*b2+
simresid
group<-factor(group)
mod<-lm(y~group)
mod
summary(mod)$sigma
bsim <- sim(mod, n.sim=1000)
m.g1<-coef(bsim)[,1]
m.g2<-coef(bsim)[,1]+coef(bsim)[,2]
m.g3<-coef(bsim)[,1]+coef(bsim)[,3]

#Calculating the CI for the difference between the means of groups 1 and 2:
d.g1.2<-m.g1-m.g2
mean(d.g1.2)
quantile(d.g1.2, prob=c(0.025, 0.975))
#how strongly do the data support the hypothesis that the mean of group 2 is 
#larger than the mean of group 1? 
sum(m.g2>m.g1)/nsim #probability that m2>m1

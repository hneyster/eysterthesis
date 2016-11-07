## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(shinystan.rstudio = TRUE)

## some good references:
# https://www.r-bloggers.com/bayesian-regression-with-stan-part-1-normal-regression/
# http://m-clark.github.io/docs/IntroBayes.html 

##libraries
library(rstan)
library(shinystan)
load("~/eysterthesis/germs.Rdata") #cleaned and processed data 

#---------------Germination timing---------------------------------------------

germs.y<-(subset(germs, 
                 germinated==1 & 
                   sp!="PLAMED" & 
                   sp!="PLACOR"))    #just the data from seeds that germianted, and taking out the congenerics 

ggplot(germs.y, aes(daysfromstart))+geom_histogram() #looking at a histogram of the data

ggplot(subset(germs.y, sp=="PLAMAJ" & temp==25.3), aes(daysfromstart))+geom_histogram() #looking at different sp
data<-germs.y
## Setting up the data for the Stan model---------------------------------------

N<-nrow(data)
K<-4
y<-data$daysfromstart                    # dependent variable
temp<-data$temp   # independent variable 
strat<-data$strat
dummy_variables <- model.matrix(~ origin, data = data)
origin<-dummy_variables[,2]
covariates<-matrix(c(origin, temp, strat), nrow=N)                  #covariate matrix
X = cbind(intercept=1, covariates) #covariates + intercept
intercept<-rep(1, nrow(data))
datax<-list(N=N, K=K, y=y, temp=temp, origin=origin, strat=strat, intercept=intercept, X=X)

## fitting into a Stan model -------------------------------------------------
fit <- stan(file = "~/linreg.stan", data=datax, chains=10, iter=1000)
#save(fit, file="germdate_nore.Rdata")
print(fit, c("beta" , "sigma"))

stan_trace(fit, "beta[5]") #tests for mixing and convergence for a given parameter 
stan_trace(fit, "sigma")
modsims <- extract(fit)
str(modsims)
plot(modsims$beta)
pairs(fit, pars=c("beta"))
la<-extract(fit, permuted=TRUE) #shows parameters by iteration
a<-extract(fit, permuted=FALSE) #shows parameters & chains & interations 
m<-as.matrix(fit) # use the functions as.array or as.matrix with s3 objects 
print(fit, digits=3)

#----launching shiny stan---------
my_sso <- launch_shinystan(fit, rstudio = getOption("shinystan.rstudio"))





# Make predictions for covariate values between 10 and 30
newdat <- data.frame(x=c(11.3, 16.0, 20.7, 25.3)) #x ranges from 10 to 30 
Xmat <- model.matrix(~x, data=newdat)
b <- lapply(modsims, mean)
newdat$fit <- Xmat%*%b
nsim <- nrow(modsims$beta)
fitmat <- matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim){
  fitmat[,i] <- Xmat%*%modsims$beta[i,]
}
newdat$fitlwr <- apply(fitmat, 1, quantile, prob=0.025)
newdat$fitupr <- apply(fitmat, 1, quantile, prob=0.975)

plot(y~x,  pch=16, las=1, cex.lab=1.4, cex.axis=1.2, type="n", main="")
polygon(c(newdat$x, rev(newdat$x)), c(newdat$fitlwr, rev(newdat$fitupr)), col=grey(0.7), border=NA)
abline(c(b[1], b[2]), lwd=2)
box()
points(x,y)

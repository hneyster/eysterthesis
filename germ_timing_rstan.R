## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(shinystan.rstudio = TRUE)
options(mc.cores = parallel::detectCores())

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
ggplot(germs.y, aes(daysfromstart)) +
    geom_histogram() +
    facet_wrap(~sp)
ggplot(subset(germs.y, sp=="PLAMAJ" & temp==25.3), aes(daysfromstart))+geom_histogram() #looking at different sp
data<-germs.y
## Setting up the data for the Stan model---------------------------------------

nseed<-length(unique(data$uniqueid)) #1205 unique seeds
N<-nseed
K<-4
y<-data$daysfromstart                    # dependent variable
temp<-data$temp   # independent variable 
strat<-data$strat
dummy_variables <- model.matrix(~ origin, data = data)
origin<-dummy_variables[,2]
covariates<-matrix(c(origin, temp, strat), nrow=N)                  #covariate matrix
X = cbind(intercept=1, covariates) #covariates + intercept
intercept<-rep(1, nrow(data))
#setting up to random effects data:
nsp<-length(unique(germs.y$sp))
sp_alph<-data$sp

sp<-ifelse (sp_alph=="CAPBUR", 1,     #making sp numeric, in alphabetical order 
            ifelse(sp_alph=="CHEMAJ",2,
                   ifelse(sp_alph=="DACGLO", 3, 
                          ifelse(sp_alph=="PLALAN", 4,
                                 ifelse(sp_alph=="PLAMAJ", 5, 
                                        ifelse(sp_alph=="RUMCRI", 6, 7)))))
nsp<-length(unique(data$sp))

#----#add'l random effects -- not in the model yet ------
# nloc<-length(unique(data$location)) #16 unique locations 
# loc.a<-data$location
# locd<-data.frame(sort(unique(data$location)))
# locd$n<-seq(1,16,1)
# locn<-data.frame()
# for( i in 1:N) {
#   for (j in 1:nloc) {
#     locn[i,j]<-ifelse(loc.a[i]==locd[j,1], j, 0)}
# }
# loc<-data$location
# locn$v17<-rowSums(locn[-1], na.rm=TRUE)
# loc<-locn$v17
# ifelse()
# dummy_variables_fam<-model.matrix(~uniqind, data=data)
# family<-dummy_variables_fam
# nfamily<-length(unique(data$uniqind)) #80 unique seed families 


#putting it all together: 
datax<-list(N=N, y=y, temp=temp, origin=origin, strat=strat,  nsp=nsp, sp=sp)
            #,nloc=nloc, nfamily=nfamily, loc=loc, family=family)

## fitting into a Stan model -------------------------------------------------
fit <- stan(file = "germdate.stan", data=datax, chains=10, iter=1000) #  divergent transitions above diag 
fit1 <- stan(file = "germdate.stan", data=datax, chains=10, iter=1000, control = list(adapt_delta = 0.99))
fit2 <- stan(file = "germdate.stan", data=datax, chains=4, iter=5000, control = list(adapt_delta = 0.99))
#stanmodel_sp-random<-fit2
#save(stanmodel_sp-random, file = "germdate_sp-random.Rdata")
#save(fit, file="germdate_nore.Rdata")
print(fit, c("beta" , "sigma"))
print(fit)

stan_trace(fit1, "mu_b_inter_to") #tests for mixing and convergence for a given parameter 
stan_trace(fit, "sigma")
modsims <- extract(fit)
str(modsims)
plot(modsims$beta)
pairs(fit2, pars=c("mu_b_temp", "mu_b_strat", "mu_b_origin"))
plot(fit2, pars=c("mu_b_inter_to","mu_b_inter_ts", "mu_b_inter_so", "mu_b_inter_tso"))
plot(fit2, pars=c("mu_b_temp", "mu_b_strat", "mu_b_origin"))

la<-extract(fit, permuted=TRUE) #shows parameters by iteration
a<-extract(fit, permuted=FALSE) #shows parameters & chains & interations 
m<-as.matrix(fit) # use the functions as.array or as.matrix with s3 objects 
print(fit, digits=3)

#----launching shiny stan---------
my_sso <- launch_shinystan(fit1, rstudio = getOption("shinystan.rstudio"))


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

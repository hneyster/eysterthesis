y <- read.table('http://raw.github.com/wiki/stan-dev/rstan/rats.txt', header = TRUE)
y<-read.csv('C:/Users/owner/Documents/github/eysterthesis/test.csv')
x <- c(30, 60)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
strat_fit <- stan('C:/Users/owner/Documents/github/eysterthesis/rats.stan')
pairs(strat_fit)
dnorm(x=0.8, mean=1, sd=0.2)*dnorm(x=1.2, mean=1, sd=0.2)*dnorm(x=1.1, mean=1, sd=0.2)
pnorm(0.8, .6 , 0.2)*pnorm(1.2, .6, 0.2)*pnorm(1.1, .6, 0.2)
pnorm(0.8, 1, 0.2)*pnorm(1.2, 1, 0.2)*pnorm(1.1, 1, 0.2)

#Notes:
#Should I use ML or REML?? See Zuur et al. 2009


############### Testing some Bayesian!
library("blmeco")
library("arm")
germsno<-germs
germs<-subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR")
mod<-lmer((log(daysfromstart))~as.factor(origin)*as.factor(temp)*strat +(1|sp/location/uniqind), data=subset(germs, germinated==1 & sp!="PLAMED" & sp!="PLACOR"))
mod

##Resid plots for lmer {lme4}:
par(mfrow=c(2,2), mar=c(4,4,2,1), mgp=c(2.2,0.8,0))

scatter.smooth(fitted(mod), resid(mod)); abline(h=0, lty=2)
mtext("Tukey-Anscombe Plot", 3, line=0.8, cex=0.8)  # residuals vs. fitted
#A positive correlation between the residuals and the fitted values (not present in the example data) would not bother us statistically, but it indicates strong shrinkage and may have biological meaning.
  
qqnorm(resid(mod), main="normal qq-plot, residuals", cex.main=0.8) # qq of residuals
qqline(resid(mod))
       
scatter.smooth(fitted(mod), sqrt(abs(resid(mod)))) # res. var vs. fitted
       
qqnorm(ranef(mod)$sp[,1], main="normal qq-plot, random effects", cex.main=0.8)
qqline(ranef(mod)$sp[,1]) # qq of random effects. These should be normally distributed 
#comparing the qq norm plot to random samples:
compareqqnorm(mod) #ours doesn't look too normal... 

nsim <- 2000 
bsim <- sim(mod, n.sim=nsim)
str(bsim)
round(apply(bsim@fixef, 2,quantile, prob=c(0.025,0.5,0.975)),3)

#------------------------------------------------Plotting the fitted values-------------------
newdat<-expand.grid(origin = factor(c('Europe','USA'),levels=levels(as.factor(germs$origin))),
                    strat    = (c('30','60')),
                    temp    =factor(c('11.3','16', '20.7', '25.3'), levels=levels(as.factor(germs$temp)))) # create a new data frame
Xmat <- model.matrix(~ origin*temp*strat, data=newdat)  # use exactly the same formula as for the fixed-effect part in the model specification
fitmat <- matrix(ncol=nsim, nrow=nrow(newdat))
for(i in 1:nsim) fitmat[,i] <- Xmat %*% bsim@fixef[i,]    # fitted values
newdat$lower <- apply(fitmat, 1, quantile, prob=0.025)
newdat$upper <- apply(fitmat, 1, quantile, prob=0.975)
newdat$fit <- Xmat %*% fixef(mod)


# Draw a graph with 95% CrI for each group at each day
jpeg(filename = "../figures/Figure7-4_cort_fitted.jpg", 
     width = 6000, height = 6000, units = "px", res=1200)

par(mfrow=c(1,1), mar=c(2,2,1,1), omi= c(0.5,0.5,0,0))     # "mar" sets the margins (lower, left, upper, right) around the plot

indexP <- newdat$origin=='USA'
indexC <- newdat$origin=='Europe'
a <- 1
#germs$stratNum <- ifelse(germs$strat=='before', 0, as.character(dat$days)) # nummeric day variable
#dat$daysNum <- as.numeric(dat$daysNum) # nummeric day variable

plot(seq(-2,77,1), seq(-0.1,4.8,length=80), yaxt="n", xaxt="n", type="n", xlab="", ylab="", las=2, cex.lab=a,
    main="", cex.main=a, font.main=1)
points(jitter(germs$strat[germs$origin=='USA']-0.4)-5, log(germs$daysfromstart[germs$origin=='USA']), lwd=2 ,pch=16, col="blue",   cex=0.5)          # Placebo raw data
points(jitter(germs$strat[germs$origin=='Europe']+0.2)+5, log(germs$daysfromstart[germs$origin=='Europe']), lwd=2 ,pch=16, col="orange",   cex=0.5, lty=2) #cort raw data 
plot(p, col="blue")
points(q, col="orange")
p<-data.frame(jitter(germs$strat[germs$origin=='USA']-.4)-5, log(germs$daysfromstart[germs$origin=='USA']))
q<-data.frame(jitter(germs$strat[germs$origin=='Europe']+0.2)+5, log(germs$daysfromstart[germs$origin=='Europe']))

ggplot(p, aes(x=p[,1], y=p[,2]))+geom_point(color="blue")+
p[,3]<-jitter(germs$strat[germs$origin=='Europe']+0.2)
p[,4]<-log(germs$daysfromstart[germs$origin=='Europe'])
ggplot(q, aes(x=q[,1], y=q[,2]))+geom_point(color="orange")
geom_point(aes(x=q[,1], y=q[,2]), color="orange")


segments(c(30,60)-c(0.4,0.4), newdat$lower[indexP], c(30, 60)-c(0.4,0.4), newdat$upper[indexP], lty=1, lwd=2, col="black" ) # CrI 
points(c(0,2,20)-c(0.4,0.4,0.4), newdat$fit[indexP], lwd=2 ,pch=16, col="black", cex=1.2)     # Placebo 

segments(c(30, 60)+c(0.2,0.2), newdat$upper[indexC], c(30, 60)+c(0.2,0.2), newdat$lower[indexC],lty=1, lwd=2, col="black" )  # CrI 
points(c(0,2,20)+c(0.2,0.2,0.2), newdat$fit[indexC], lwd=1 ,lty=1, pch=21, bg="white", col="black", cex=1.2) # CORT 

axis(side=2, labels=F,at=seq(0,4.8,0.4),line=0,tcl=-0.3,las=1)
axis(side=2, labels=seq(0,4.8,0.4),at=seq(0,4.8,0.4),line=0,tcl=-0.3,las=1, mgp=c(3,0.5,0),cex.axis=a)    
axis(side=1, labels=c(NA, 2,20),at=c(0, 2,20),line=0,tcl=-0.3,las=1)
axis(side=1, labels=c("before"),at=c(-0.5),line=0,tcl=0,las=1)

mtext(side=2,line=3,adj=0.5,cex=a,font=2,"Total corticosterone [log, ng/ml]",las=0,outer=FALSE)
mtext(side=1,line=3,adj=0.5,cex=a,font=2,"Days after implantation",outer=FALSE)

points(c(10,10),c(4.5,4.8),pch=21,bg=c("black","white"))
text(c(11,11),c(4.5,4.8),c("placebo", "corticosterone"),adj=c(0,0.5))
dev.off()

#---------trying GLMER-------------------

modrater<-glmer(germinated~origin*strat+(1|sp), 
                family=binomial(link="logit"), 
                data=subset(germs, sp!="PLAMED" & sp!="PLACOR")) 
mod<-modrater
#plots
par(mfrow=c(2,2))    # check residuals
qqnorm(resid(mod), main="qq-plot residuals")
qqline(resid(mod))

qqnorm(ranef(mod)$sp[,1], main="qq-plot, farm")
qqline(ranef(mod)$sp[,1])

plot(fitted(mod), resid(mod)) # residuals vs fitted values
abline(h=0)

dat$fitted <- fitted(mod)
plot(dat$fitted,dat$fledge/dat$clutch)    # plot data vs. predicted values
abline(0,1)

## Checking to make sure the random effects have a mean close to 0
mean(ranef(mod)$sp[,1])
sp.should <- plogis(fixef(mod)["(Intercept)"])                # expected value at the intercept
sp.is     <- plogis(fixef(mod)["(Intercept)"]-0.01570766)    # slightly reduced value because the mean farm random effect is not precisely 0
# => the error at the Intercept is:
(sp.should-sp.is)/sp.should   # only a difference of  0.2% at the intercept than ideal -- this seems negligible. 
#
dispersion_glmer(mod)
#
germs$resid <- resid(mod)
par(mfrow=c(1,3))
plot(germs$colsize, germs$resid, col="orange", lwd=2)
plot(jitter(germs$strat), germs$resid, col="orange", lwd=2)
plot(germs$origin, dat$resid, col="orange", lwd=2)

germs$strat.j <- jitter(germs$strat)
#germs$surv <- dat$fledge/dat$clutch

pairs(dat[,c("germinated","colsize","strat.j","origin")],)


##testing stan
library(rstan)
#-------------------------------------------------------------------------------
# Simulate fake data 
#-------------------------------------------------------------------------------
n <- 50                                      # sample size
sigma <- 5                                   # standard deviation of the residuals
b0 <- 2                                      # intercept
b1 <- 0.7                                    # slope

x <- runif(n, 10, 30)                        # random numbers of the covariate
simresid <- rnorm(n, 0, sd=sigma)            # residuals

y <- b0 + b1*x + simresid                    # calculate y, i.e. the data

#-------------------------------------------------------------------------------
# Bundle data into a list 
#-------------------------------------------------------------------------------
datax <- list(n=length(y), y=y, x=x)
fit <- stan(file = "~/linreg.stan", data=datax, chains=10, iter=1000)

print(fit, c("beta", "sigma"))

traceplot(fit, "beta")
traceplot(fit, "sigma")
modsims <- extract(fit)
str(modsims)



# Make predictions for covariate values between 10 and 30
newdat <- data.frame(x=seq(10, 30, length=100)) #x ranges from 10 to 30 
Xmat <- model.matrix(~x, data=newdat)
b <- apply(modsims$beta, 2, mean)
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

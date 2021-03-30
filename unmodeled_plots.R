# Code for plotting the figurs in the SI 

library(plyr)
library(ggplot2)
library(here)


load(here('clean_dat/germs.rdata'))
load(here('clean_dat/hlms.rdat'))
load(here('clean_dat/gh.rdata'))

pd<-position_dodge(.4)
templab <- c('18/8','22.7/12.7','27.3/17.3','32/22')
xlabel <- "Temperature range (high/low°C)"
## Raw plot of rate: 
germinatedsotsummarys<-
  ddply(subset(germs, sp!="PLACOR" & sp!="PLAMED"), c( "sp", "origin", "strat", "temp"), summarise,
        mean=mean(germinated), sd=sd(germinated),
        sem=sd(germinated)/sqrt(length(germinated)))
plot3b<-ggplot(germinatedsotsummarys, aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat, scale="free_y")+theme_bw()+
  ylab("Percent germination")+
  xlab(xlabel)+
  ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))+
  geom_text(aes(x, y, label=labs, group=NULL, color=NULL),data=lab1) + 
  scale_x_discrete(labels= templab)

pdf(here("manuscript/figure5.pdf"), width=8, height=11)
print(plot3b)
dev.off()

## Raw plot of dates: 
germsn<-subset(germs, germinated==1) 
germinatedsummarydatests<-
  ddply(germsn, c( "sp", "origin", "temp" , "strat"), summarise,
        mean=mean(daysfromstart), sd=sd(daysfromstart),
        sem=sd(daysfromstart)/sqrt(length(daysfromstart)))
lab2<-lab1
lab2$y<-20
lab2[13:14, 2] <- 11
lab2[3:4, 2] <- 40
lab2[5:6, 2]<-11
lab2[11:12, 2]<-28
lab2[1:2, 2]<-25

plot3a<-ggplot(subset(germinatedsummarydatests, sp!="PLAMED" & sp!="PLACOR"), aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat, scales="free_y")+theme_bw()+
  ylab("Days to germination")+
  xlab(xlabel)+
  ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))+
  geom_text(aes(x, y, label=labs, group=NULL, color=NULL),data=lab2) + 
    scale_x_discrete(labels= templab)
#ggtitle("Germ date vs. temp by continent and strat for each sp")
pdf(here("manuscript/figure7.pdf"), width=8, height=11)
print(plot3a)
dev.off()

# raw plot of growth rate 

germinatedsummarygrts<-
  ddply(hlms, c( "origin", "sp", "temp", "strat"), summarise,
        mean=mean(gr), sd=sd(gr),
        sem=sd(gr)/sqrt(length(gr)))
lab3<-lab2
lab3$y<-c(0.08)
lab3[5:6, 2] <- .4
#set up multipanel plot to look at growth rate for each species  
plot3c<-ggplot(germinatedsummarygrts, aes(x=(as.factor(temp)),y=mean, color=origin, group=origin))+  
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2, position=pd, size=.8) + 
  geom_line(size=.6) +geom_point(position=pd, size=1.6)+ 
  facet_grid(sp~strat, scale="free_y")+theme_bw()+
  ylab("Growth rate (cm/day)")+
  xlab(xlabel)+
  ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))+
  geom_text(aes(x, y, label=labs, group=NULL, color=NULL),data=lab3) + 
  scale_x_discrete(labels= templab)
pdf(here("manuscript/figure9.pdf"), width=8, height=11)
print(plot3c)
dev.off()

## LM models of gr for each seed: 
templab2 <- templab
templab2 <- paste0(templab2, '°C')
names(templab2) <- c("11.3","16","20.7","25.3")
species<-sort(unique(gh$sp))
pdf(file=here("manuscript/supplement.pdf"), height=8, width=7)
for(i in 1:length(species)){
  a<-ggplot(subset(gh, sp==species[i]), aes(x=(mdaysfromgerm),y=plantheight, color=origin, group=trayloc))+  
    geom_line(size=.6, alpha=.2) +geom_point(position=pd, size=1.6, alpha=.2)+ 
    geom_smooth(aes(group = origin), size = 1, method = "lm", se = FALSE, alpha=1)+
    facet_grid(temp~strat, labeller = labeller(temp = templab2))+theme_bw()+
    ylab("Height (cm)")+
    xlab("Days since germination")+
    ggtitle("Stratification length (days)") +theme(plot.title=element_text(size=10))
  plot(a)
}
dev.off()

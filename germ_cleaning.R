### Started 21 December 2015 ###
### By Lizzie ###
### Modified by Harold Eyster ###
## Re-written to just include data cleaning on 2021-03-29

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## load libraries
library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
#library(lme4)
#library(nlme)
#library(MASS)
#library(car)
library(here)
#library(lmerTest)
## point to files on computer

## Get files
height<- read.csv(here("input/Height_Data_2-13-16.csv"), header=TRUE) #Read the height file
germ <- read.csv("input/Germ_Data_2-9-16.csv", header=TRUE) #read the germination file
id<- read.csv("input/id_Data-2-4-16.csv", header=TRUE) #read the id file
lab1<-read.csv("input/label.csv", header=TRUE) #read in the graph labeling file


## basic data cleaning
#rename the columns
names(height)<- c("trayloc", "substrate", "measuredate", "plantheight" ) 
names(germ) <- c("trayloc", "substrate", "germdate")
names(id) <- c("trayloc", "sp", "strat", "temp", "individual", "location", "substrate")

#reformat the dates 
germ$germdate <- as.Date(germ$germdate, format="%m/%d/%Y")
germ$doy <- as.numeric(format(germ$germdate, "%j"))
height$measuredate <- as.Date(height$measuredate, format="%m/%d/%Y") #this line doesn't work -- won't save in correct format
#height$heightdate <-format(height$measuredate, "%b/%d/%Y")
germ$daysfromstart <- ifelse(germ$doy<100, (germ$doy)+38, (germ$doy)-327) # use start date of 23 Nov 2015, and account for 2016 vs. 2015 date
#create unique identifiers for each seed
germ$uniqueid<- NA
germ$uniqueid<-do.call(paste, c(germ[c("trayloc", "substrate")], sep=""))
id$uniqueid<- NA
id$uniqueid<-do.call(paste, c(germ[c("trayloc", "substrate")], sep=""))
height$uniqueid<- NA
height$uniqueid<-do.call(paste, c(height[c("trayloc", "substrate")], sep="")) 
height$plantheight<-as.numeric(height$plantheight) # make the plant height measurements numerical 

#merge the germ and id data frames
germid <- merge(germ,id,by=("uniqueid"))
colnames(germid)[2]<-"trayloc" 
colnames(germid)[3]<-"substrate"
germid$substrate.y<-NULL #remove duplicate columns 
germid$trayloc.y<-NULL #remove duplicate columns 

#create column for continental origin 
germid$origin[which(grepl("EU", germid$individual)==TRUE)] <- "Europe"
germid$origin[which(grepl("US", germid$individual)==TRUE)] <- "USA"
germid$location<-do.call(paste, c(germid[c("origin", "location")], sep="--"))

## Assign the treatments an average temp
# 18/8, 22.67/12.67C, 27.33/17.33, and 32/22
germid$temp[germid$temp=="LL"] <- round((16*8+18*8)/24, digits=1)
germid$temp[germid$temp=="LM"] <- round((16*12.67+22.67*8)/24, digits=1)
germid$temp[germid$temp=="HM"] <- round((16*17.33+27.33*8)/24, digits=1)
germid$temp[germid$temp=="HH"] <- round((16*22+32*8)/24, digits=1)

germid$temp <- as.numeric(germid$temp)


#add column to for germ rate, setting germinated seeds to 1, ungerminated to 0
germid$germinated <- germid$doy
germid$germinated[is.na(germid$germinated)==TRUE] <- 0
germid$germinated[germid$germinated>0] <- 1

#subset the data by substrate 
germs<-subset(germid, substrate=="Soil", select=uniqueid:germinated) #subset with data from seeds planted in soil
#germf<-subset(germid, substrate=="Filter", select=uniqueid:germinated) #subset with data from seeds planted on filter paper

germs$uniqind<- NA
germs$uniqind<- do.call(paste, c(germs[c("sp", "individual")], sep="_")) #combine sp and individual to obtain a column for unique individual

## HEIGHT 


germsheightna <- merge(germs,height,by=("trayloc")) #merging the germination and height data sets. Including non-gemrinating seeds
germsheight<-germsheightna[which(germsheightna$germinated==1),] #removing the seeds that didn't germinate 

#data cleaning 
germsheight$substrate.y=NULL 
germsheight$uniqueid.y=NULL
names(germsheight)[names(germsheight)=="uniqueid.x"] <- "uniqueid"
names(germsheight)[names(germsheight)=="substrate.x"] <- "substrate"
#Relating measure date and start date 
germsheight$dom <- as.numeric(format(germsheight$measuredate, "%j"))
germsheight$mdaysfromstart <- ifelse(germsheight$dom<100, (germsheight$dom)+38, (germsheight$dom)-327) # use start date of 23 Nov 2015, and account for 2016 vs. 2015 date
germsheight$mdaysfromgerm <- germsheight$mdaysfromstart-germsheight$daysfromstart #this creates a column for how old the seedling was when measured. Seeds that haven't germinated yet will yeild negative values 
ghc<-subset(germsheight, mdaysfromgerm>=0) #subset data to remove false height measurements for seeds that hadn't yet germinated
gh<-subset(ghc, sp!="PLAMED" & sp!="PLACOR" & plantheight!="NA" ) #removing the european congenerics 


n<-unique(gh$trayloc)
hlm=data.frame(n)

#run a for loop to calculate lm coef for height vs. time 
for(i in 1:length(n)){
  print(n[i])
  hlm$m[i]<-lm(plantheight~mdaysfromgerm, data=subset(gh, trayloc==n[i]))
}
hlm$b <- lapply(strsplit(as.character(hlm$m), ","), "[", 2)  #split the intercept and slope 
hlm$b<-gsub("\\)","",as.character(hlm$b)) #remove the paranthesis
hlm$b <- gsub ("mdaysfromgerm = ", "", as.character(hlm$b)) # removing the text, to isolate just the slope coef
hlm$gr<-as.numeric(hlm$b) #this columm now has the slope coef in numeric form
hlmn<-subset(hlm, gr!="NA") #removes NA values (i.e. where there is only one height measurment, hence no growth rate could be calulated)
hlms<-merge(hlmn, germs, by.x=("n"), by.y=("trayloc"), all.y=FALSE)

# examining differences in germination 
germsub <- germs[germs$sp !='PLACOR',]
germsub <- germsub[germsub$sp !='PLAMED',]
us <- 10*8*7 + 3*5*8
eu <- 10*8*7 + 10*5*8
length(germsub$sp[germsub$origin == 'USA'])
length(germsub$sp[germsub$origin == 'Europe'])

## Saving cleaned data

save('germs', file = here("clean_dat/germs.rdata")) # for modeling and plotting 
save('hlms', file = here("clean_dat/hlms.rdata")) # for modeling and plotting 
save('gh', file = here("clean_dat/gh.rdata")) # for plotting the gr of each seed for the supplement; not for Stan input. 

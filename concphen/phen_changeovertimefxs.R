### Started 18 July 2012 ####
### By Lizzie ###

## f(x) to split data, by year, and calculate change in days ##

changeoversplit <- function(data, yearsplit){
    early <- subset(data, year<=yearsplit)
    recent <- subset(data, year>yearsplit)
    earlymean <- aggregate(early["doy"], early[c("latbi", "genus", "species")],
        FUN=mean)
    recentmean <- aggregate(recent["doy"], recent[c("latbi", "genus", "species")],
        FUN=mean)
    altogethernow <- merge(earlymean, recentmean, by="latbi",
        suffixes=c("early", "recent"))
    altogethernow$doychange <- altogethernow$doyrecent-altogethernow$doyearly
    altogethernow.out <- subset(altogethernow, select=c("latbi", "doychange"))
    return(altogethernow.out)
  }

changeovertime <- function(data){
    dataframer <- as.data.frame(unique(data[["latbi"]]))
    names(dataframer) <- "latbi"
    dataframer$changeovertime <- NA    
    for (i in 1:length(unique(data[["latbi"]]))){    
        sppdat <- subset(data, latbi==unique(data[["latbi"]])[i])
        if (nrow(sppdat) < 5) dataframer$changeovertime[i] <- 1001
        else
        {
        modelhere <- lm(doy~year, data=sppdat)
        dataframer$changeovertime[i] <- summary(modelhere)$coef[2]
        }
      }
    dataframer$changeovertime[dataframer$changeovertime==1001] <- NA
    return(dataframer)
  }

## Started 13 March 2020 ##
## Quick code by Lizzie ##

## Start of taken from phenNatExo_2012msAnalysisGraphs.R ##

options(stringsAsFactors=FALSE)

library(car)
library(ggplot2)

setwd("~/Documents/git/projects/others/undergrads/harold/analyses/eysterthesis/concphen")
# source("/Users/Lizzie/Documents/R/fromNicolas/pglm.R")
source("conctraits.R")
source("phenNatExo_helperfxs.R")


conc <- conctraits("input/concord_ALLSEAS.", "concord", conc.natexo, noxconc)
concdoy <- conctraits.doy(conc.natexo, noxconc, concchange)

conc.dat <- subset(as.data.frame(conc$mat[-341,]),is.na(native.exotic)==FALSE)
conc.dat <- conc.dat[-14,]

conc.doy <- subset(concdoy, is.na(native.exotic)==FALSE)
conc.doy <-  conc.doy[-13,]

## End of taken from phenNatExo_2012msAnalysisGraphs.R #

eysterspp <- c("Alliaria_petiolata", "Capsella_bursa-pastoris", "Chelidonium_majus", "Dactylis_glomerata", "Plantago_lanceolata", "Plantago_major", "Rumex_crispus", "Taraxacum_officinale")
eysterspplow <- tolower(eysterspp)


unique(conc.doy$latbi)[grep("petiolata",  unique(conc.doy$latbi))]
unique(conc.doy$latbi)[grep("dact",  unique(conc.doy$latbi))]
unique(conc.doy$latbi)[grep("tarax",  unique(conc.doy$latbi))]

conc.doy[which(conc.doy$latbi %in% eysterspplow),]
conc.dat[which(conc.dat$latbi %in% eysterspplow),]

ggplot(conc.doy, aes(x=doychange), xlab="flowering time shift") + 
    geom_histogram(data=subset(conc.doy, native.exotic=="exotic"), 
        fill="red", alpha = 0.8) + 
    geom_histogram(data=subset(conc.doy, native.exotic=="native"), 
        fill="blue", alpha = 0.5)

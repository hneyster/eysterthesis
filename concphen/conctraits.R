
source("phen_changeovertimefxs.R")

conctraits.doy <- function(traitfile, noxlist, changefile){
    ## native/exotic/invasive codings
    sitetra <- traitfile
    sitetra$inv <- "noninv"
    sitetra$inv[which(sitetra$latbi %in% noxlist)] <- "inv"
    sitetra$full.code <- paste(sitetra$native.exotic, sitetra$inv, sep=".")
    sitetra$full.code[sitetra$full.code=="NA.noninv"] <- NA
    sitetra$full.code[sitetra$full.code=="NA.inv"] <- NA
    sitetra$nat.exoinv <- sitetra$full.code
    sitetra$nat.exoinv[sitetra$native.exotic=="native"] <- "native"
    sitetra$native.exotic12 <- sitetra$native.exotic
    sitetra$native.exotic12[sitetra$native.exotic12=="native"] <- 1
    sitetra$native.exotic12[sitetra$native.exotic12=="exotic"] <- 2
    sitetra$native.exotic12 <- as.numeric(sitetra$native.exotic12)
    sitetra$inv.non12 <- sitetra$inv
    sitetra$inv.non12[sitetra$inv.non12=="inv"] <- 1
    sitetra$inv.non12[sitetra$inv.non12=="noninv"] <- 2
    sitetra$inv.non12 <- as.numeric(sitetra$inv.non12)
    sitetra$nat.exo.inv123 <- sitetra$full.code
    sitetra$nat.exo.inv123[sitetra$nat.exo.inv123=="native.noninv"] <- 1
    sitetra$nat.exo.inv123[sitetra$nat.exo.inv123=="exotic.noninv"] <- 2
    sitetra$nat.exo.inv123[sitetra$nat.exo.inv123=="exotic.inv"] <- 3
    sitetra$nat.exo.inv123 <- as.numeric(sitetra$nat.exo.inv123)
    sitetra <- merge(sitetra, changefile, by="latbi")
    return(sitetra)
  }

conctraits <- function(path, sitename, traitfile, noxlist){
    outputter <- list()
    sitepath <- path
    site.mo3 <- read.csv(paste(sitepath, "3month.temp.T-ONLY.csv", sep=""))
    site.mat <- read.csv(paste(sitepath,  "annual.temp.T-ONLY.csv", sep="")) 
    site.gdd <- read.csv(paste(sitepath, "gdd.mod.T-ONLY.csv", sep=""))
    # site.mo3 <- scrubtotrees(site.mo3, "Genus")
    # site.mat <- scrubtotrees(site.mat, "Genus")
    # site.gdd <- scrubtotrees(site.gdd, "Genus")
    site.mo3$latbi <- tolower(paste(site.mo3$Genus, site.mo3$Species, sep="_"))
    site.mat$latbi <- tolower(paste(site.mat$Genus, site.mat$Species, sep="_"))
    site.gdd$latbi <- tolower(paste(site.gdd$Genus, site.gdd$Species, sep="_"))
    ## native/exotic/invasive codings
    sitetra <- traitfile
    sitetra$inv <- "noninv"
    sitetra$inv[which(sitetra$latbi %in% noxlist)] <- "inv"
    sitetra$full.code <- paste(sitetra$native.exotic, sitetra$inv, sep=".")
    sitetra$full.code[sitetra$full.code=="NA.noninv"] <- NA
    sitetra$full.code[sitetra$full.code=="NA.inv"] <- NA
    sitetra$nat.exoinv <- sitetra$full.code
    sitetra$nat.exoinv[sitetra$native.exotic=="native"] <- "native"
    sitetra$native.exotic12 <- sitetra$native.exotic
    sitetra$native.exotic12[sitetra$native.exotic12=="native"] <- 1
    sitetra$native.exotic12[sitetra$native.exotic12=="exotic"] <- 2
    sitetra$native.exotic12 <- as.numeric(sitetra$native.exotic12)
    sitetra$inv.non12 <- sitetra$inv
    sitetra$inv.non12[sitetra$inv.non12=="inv"] <- 1
    sitetra$inv.non12[sitetra$inv.non12=="noninv"] <- 2
    sitetra$inv.non12 <- as.numeric(sitetra$inv.non12)
    sitetra$nat.exo.inv123 <- sitetra$full.code
    sitetra$nat.exo.inv123[sitetra$nat.exo.inv123=="native.noninv"] <- 1
    sitetra$nat.exo.inv123[sitetra$nat.exo.inv123=="exotic.noninv"] <- 2
    sitetra$nat.exo.inv123[sitetra$nat.exo.inv123=="exotic.inv"] <- 3
    sitetra$nat.exo.inv123 <- as.numeric(sitetra$nat.exo.inv123)
    outputter$mo3 <- merge(sitetra, site.mo3, by="latbi", all.y=TRUE)
    outputter$mat <- merge(sitetra, site.mat, by="latbi", all.y=TRUE)
    outputter$gdd <- merge(sitetra, site.gdd, by="latbi", all.y=TRUE)
    return(outputter)
  }


##
## get the nativity and noxious weeds files
usda.washed.out <- read.csv("input/traits/usda/usdanatexo.csv", header=TRUE)
usda.washed.out$latbi <- tolower(paste(usda.washed.out$genus,
    usda.washed.out$species, sep="_"))

noxstate <- read.csv("input/traits/usda/noxious5sites.csv", header=TRUE)
noxstate$latbi <- tolower(paste(noxstate$genus, noxstate$species, sep="_"))

gentraitstra <- read.csv("input/traits/PhenTraits2012.csv", header=TRUE)
gentraitstra$latbi <- tolower(paste(gentraitstra$genus, gentraitstra$species, sep="_"))

spp <- read.csv("input/specieslists/other4_allspp_5yrssplist.csv", header=TRUE)
spp$latbi <- tolower(paste(spp$genus, spp$species, sep="_"))
sppconc <- subset(spp, site=="concord")


## Make up the exotic list for Concord 
noxconc1 <- merge(sppconc, subset(noxstate, site=="concord"), by="latbi", all.x=FALSE)
noxconc2 <- merge(sppconc, subset(noxstate, site=="concord" & (species=="L." |
    species=="Lour." |species=="Thunb.")), by="genus", all.x=FALSE,
    suffixes=c("","s"))
noxconc <- c(noxconc1$latbi, noxconc2$latbi)

## add in abundance data for Concord
concAbun <- read.delim("input/traits/data_6cat.txt", header=TRUE)
concAbun$name <- tolower(concAbun$name)
names(concAbun)[names(concAbun)=="name"] <- "latbi"
concAbunsm <- subset(concAbun, select=c("latbi", "DeltaAbun_cont"))
# note to self: DeltaAbun_cont is Primack abundance MINUS Hosmer abundance
# so declines in abundance are negative #s and increases in abundance are positive #s

## Get the list of add-ons for native.exotic status for Concord ##
## Most of these showed up as 
concadd <- read.csv("input/traits/natexo_addconc.csv", header=TRUE, na.strings="NA")
concadd$latbi <- tolower(paste(concadd$genus, concadd$species, sep="_"))

conc.natexo <- rbind(usda.washed.out, concadd)

## get the change over time or split info ##
if(FALSE){
rawdat <- read.csv("~/Documents/Subversion/phenology/Data/pheno_raw.csv", header=TRUE)
concraw <- subset(rawdat, site=="concord")
write.csv(concraw, "input/concraw.csv", row.names=FALSE)
}

ffdfld <- read.csv("input/concraw.csv", header=TRUE)
ffd <- subset(ffdfld, event=="ffd")
ffd$date <- as.Date(ffd$date)
ffd$doy <- as.numeric(ffd$doy)
ffd$latbi <- tolower(paste(ffd$genus, ffd$species, sep="_"))

concchange <- changeoversplit(subset(ffd, site=="concord"), 1904)


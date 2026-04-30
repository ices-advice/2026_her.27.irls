## Preprocess data, write TAF data tables

## Before: canum.txt, caton.txt, fleet.txt, fprop.txt, index.txt, matprop.txt,
##         mprop.txt, natmor.txt, weca.txt, west.txt (boot/data)
## After:  catage.csv, catch.csv, maturity.csv, natmort.csv, stock.Rdata,
##         stock.tun.Rdata, survey.csv, wcatch.csv, wstock.csv (data)

library(icesTAF)
library(FLCore)

mkdir("data")

##############################################################################################
## Create the FLStock object from the Lowestoft Input Files
#############################################################################################


# read in the lowestoft input files
stock<-readFLStock("boot/data/index.txt",quiet=FALSE)

#Set no discards
stock@catch.n                <- stock@landings.n
stock@catch                  <- stock@landings
stock@catch.wt               <- stock@landings.wt


#Set fbar
range(stock)[c("minfbar","maxfbar")] <- c(2,5)

#Set plus group
stock<- setPlusGroup(stock,stock@range["max"])

stock.tun <- readFLIndices("boot/data/fleet.txt")


#Set names, and parameters etc
names(stock.tun) <-  gsub(":.*$","",names(stock.tun))
stock.tun   <- lapply(stock.tun,function(idx) {
  idx@type 	     <- 	"number"
  idx@index.var[]  <-	1
  idx@range["plusgroup"] <- NA
  return(idx)})


names(stock.tun)[1] <- c("CS Herring Acoustic")

## Write out the csv files
write.taf(flr2taf(catch.n(stock)), "data/catage.csv")
write.taf(flr2taf(catch(stock),"Catch"), "data/catch.csv")
write.taf(flr2taf(catch.wt(stock)), "data/wcatch.csv")
write.taf(flr2taf(stock.wt(stock)), "data/wstock.csv")
write.taf(flr2taf(mat(stock)), "data/maturity.csv")
write.taf(flr2taf(m(stock)), "data/natmort.csv")
survey <- flr2taf(catch.n(stock.tun[[1]]))
survey[survey == -1] <- NA
write.taf(survey, dir="data")

##save the stock object
save(stock,file='data/stock.Rdata')
save(stock.tun,file='data/stock.tun.Rdata')


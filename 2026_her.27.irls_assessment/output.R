## Extract results of interest, write TAF output tables

## Before: asap3.rdat, stock.mult.opts.Rdata, stock.options.Rdata,
##         stock.proj.Rdata, yrs.csv (model)
## After:  fatage.csv, multi_options_summary.csv, natage.csv,
##         options_details.csv, options_input.csv, options_summary.csv,
##         sag.xml, summary.csv (output)

library(icesTAF)
library(FLCore)
library(icesSAG)


mkdir("output")

##############################################################################################
### Assessment Output
#############################################################################################

## Read in all results
asap <- dget("model/asap3.rdat")

## Numbers at age
natage <- xtab2taf(asap$N.age)
names(natage)[-1] <- 1:9
write.taf(natage, dir="output")

## F at age
fatage <- xtab2taf(asap$F.age)
names(fatage)[-1] <- 1:9
write.taf(fatage, dir="output")

## Number of years and ages for the summary table
nyears<-asap$parms$nyears
nages<-asap$parms$nages

## Summary table
year <- natage$Year
catch<-asap$catch.obs[1:nyears]
rec <- natage$"1"
ssb <- asap$SSB
tsb<-asap$tot.jan1.B 
fbar <- rowMeans(fatage[as.character(2:5)])
summary <- data.frame(Year=year,Catch=catch, Rec=rec, SSB=ssb, Fbar=fbar)
write.taf(summary, dir="output")



##############################################################################################
### Output from the short term forecast
#############################################################################################
load(file='model/stock.proj.Rdata')
load(file='model/stock.options.Rdata')
load(file='model/stock.mult.opts.Rdata')

yrs <- read.taf("model/yrs.csv")
ImY <- yrs$ImY
AdY <- yrs$AdY
CtY <- yrs$CtY
ExY<-yrs$ExY
tbl.yrs <- as.character(yrs)

input.tbl.file <-file.path(dir="output","options_input.csv")

write.table(NULL,file=input.tbl.file,col.names=FALSE,row.names=FALSE)
input.tbl.list <- list(N="stock.n",M="m",Mat="mat",PF="harvest.spwn",
                       PM="m.spwn",SWt="stock.wt",Sel="harvest",CWt="catch.wt")
for(yr in c(ImY,AdY,CtY,ExY)){
  col.dat <- sapply(input.tbl.list,function(slt) slot(stock.proj,slt)[,as.character(yr),drop=TRUE])
  write.table(yr,file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(t(c("Age",colnames(col.dat))),file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
  write.table(col.dat,file=input.tbl.file,col.names=FALSE,row.names=TRUE,append=TRUE,sep=",",na="-")
  write.table("",file=input.tbl.file,col.names=FALSE,row.names=FALSE,append=TRUE,sep=",")
}


#Detailed options table
options.file <-file.path(dir="output","options_details.csv")
write.table(NULL,file=options.file,col.names=FALSE,row.names=FALSE)
for(i in 1:length(stock.options)) {
  opt <- names(stock.options)[i]
  stk <- stock.options[[opt]]
  #Now the F and N by age
  nums.by.age <- stk@stock.n[,tbl.yrs,drop=TRUE]
  colnames(nums.by.age) <- sprintf("N(%s)",tbl.yrs)
  f.by.age    <- stk@harvest[,tbl.yrs,drop=TRUE]
  colnames(f.by.age) <- sprintf("F(%s)",tbl.yrs)
  age.tbl     <- cbind(Age=rownames(f.by.age),N=nums.by.age,F=f.by.age)
  #And now the summary tbl
  sum.tbl     <- cbind(Year=tbl.yrs,SSB=ssb(stk)[,tbl.yrs],
                       F.bar=fbar(stk)[,tbl.yrs],Yield=computeCatch(stk)[,tbl.yrs])
  #Now, bind it all together
  sum.tbl.padding <- matrix("",nrow=nrow(age.tbl)-nrow(sum.tbl),ncol=ncol(sum.tbl))
  comb.tbl    <- cbind(age.tbl," ",rbind(sum.tbl,sum.tbl.padding))
  #And write it - hdr first, then the rest
  write.table(sprintf("%s). %s",letters[i],opt),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(t(colnames(comb.tbl)),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(comb.tbl,options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
  write.table(c(""),options.file,append=TRUE,col.names=FALSE,row.names=FALSE,sep=",")
}

#Options summary table
opt.sum.tbl <- function(stcks,fname) {
  options.sum.tbl <- sapply(as.list(1:length(stcks)),function(i) {
    opt <- names(stcks)[i]
    stk <- stcks[[opt]]
    #Build up the summary
    sum.tbl     <- data.frame(Rationale=opt,
                              F.ImY=fbar(stk)[,as.character(ImY),drop=TRUE],
                              Catch.ImY=computeCatch(stk)[,as.character(ImY),drop=TRUE],
                              SSB.ImY=ssb(stk)[,as.character(ImY),drop=TRUE],
                              F.AdY=fbar(stk)[,as.character(AdY),drop=TRUE],
                              Catch.AdY=computeCatch(stk)[,as.character(AdY),drop=TRUE],
                              SSB.AdY=ssb(stk)[,as.character(AdY),drop=TRUE],
                              F.CtY=fbar(stk)[,as.character(CtY),drop=TRUE],
                              Catch.CtY=computeCatch(stk)[,as.character(CtY),drop=TRUE],
                              SSB.CtY=ssb(stk)[,as.character(CtY),drop=TRUE],
                              SSB.ExY=ssb(stk)[,as.character(ExY),drop=TRUE])
  })
  options.sum.tbl <- t(options.sum.tbl)
  colnames(options.sum.tbl) <- c("Rationale",
                                 sprintf("Fbar (%i)",ImY),sprintf("Catch (%i)",ImY),sprintf("SSB (%i)",ImY),
                                 sprintf("Fbar (%i)",AdY),sprintf("Catch (%i)",AdY),sprintf("SSB (%i)",AdY),
                                 sprintf("Fbar (%i)",CtY),sprintf("Catch (%i)",CtY),sprintf("SSB (%i)",CtY),
                                 sprintf("SSB (%i)",ExY))
  write.csv(options.sum.tbl,file=fname,row.names=FALSE)
}
opt.sum.tbl(stcks=stock.options,fname=file.path(dir="output","options_summary.csv"))
opt.sum.tbl(stcks=stock.mult.opts,fname=file.path(dir="output","multi_options_summary.csv"))





##############################################################################################
### create SAG xml file
#############################################################################################

stockinfo <- stockInfo("her.27.irls", 2026, "afra.egan@marine.ie",StockCategory = 1,ModelType='A',
                       ModelName = 'ASAP')


stockinfo$StockCategory             <- "1" 
stockinfo$MSYBtrigger               <- 54000
stockinfo$Blim                      <- 34000
stockinfo$Bpa                       <- 54000
stockinfo$Flim                      <- 0.45
stockinfo$Fpa                       <- 0.26
stockinfo$FMSY                      <- 0.26
stockinfo$Fage                      <- "2-5" 
stockinfo$RecruitmentAge            <- 1
stockinfo$CatchesCatchesUnits       <- "tonnes" 
stockinfo$RecruitmentDescription    <- "WR" 
stockinfo$FishingPressureDescription<- "F" 
stockinfo$StockSizeDescription      <- "SSB" 
stockinfo$StockSizeUnits            <- "tonnes" 
stockinfo$Purpose                   <- "Advice" 

stockData <- stockFishdata(Year = year)
stockData$Catches <- catch
stockData$Landings <- catch
stockData$Recruitment <- rec
stockData$TBiomass<-tsb
stockData$StockSize<-ssb
stockData$FishingPressure<-fbar



xmlfile <- createSAGxml(stockinfo, stockData)
cat(xmlfile, file = "output/sag.xml")



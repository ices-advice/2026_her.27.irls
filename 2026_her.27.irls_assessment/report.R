## Prepare plots and tables for report

## Before: fatage.csv, natage.csv, summary.csv (output.R), assessment inputs csv files, asap.r output file
## After:  Data Plots: catches.png,Catch weights.png, stock weights.png
##        Assessment Output Plots: Stock Summary.png, Obs and pred catch.png, catch proportions at age.png, 
##        Fishery selectivity.png, Index selectivity.png, Index Fit.png, Catch Residuals.png, Survey Residuals.png, Survey Internal Consistency.png
##        Tables for the Report: N at age, F at age, Stock Summary, assessment Input Files. 

library(icesTAF)
library(FLCore)
library(ggplot2)
library(gridExtra)
#library(icesTAFextra) 

mkdir("report")

##############################################################################################################
## Input Data Plots
## 1 Catch time series
## 2 Mean Weights in the Catch
## 3 Mean Weights in the Stock
##############################################################################################################
## Read in all results
asap <- dget("model/asap3.rdat")
load(file='data/stock.Rdata')
load(file='data/stock.tun.Rdata')

years <- asap$parms$styr:asap$parms$endyr
ages <- 1:asap$parms$nages
ind_years<-asap$index.year
nindices<-asap$parms$nindices
nyears<-asap$parms$nyears
nages<-asap$parms$nages

taf.png("Catches")
Catches<-as.data.frame(stock@catch)
print(ggplot(data=Catches, aes(year, data)) + 
  geom_bar(stat="identity")+
  ggtitle("Celtic Sea Herring Catches")+
  xlab("year")+
  scale_x_continuous(breaks=c(1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020,2025))+
  ylab("Catch(t)"))
dev.off()



taf.png("Catch weights")
catch_wt<-as.data.frame(stock@catch.wt)
print(ggplot(data=catch_wt, aes(year, data)) + geom_line(aes(group=age, colour=factor(age)),size=1)+
  ggtitle("CSH Mean Weights in the Catch")+
  ylab("weight(kg)")+ 
  facet_wrap(~age, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none"))
dev.off()

taf.png("Stock weights")
stock_wt<-as.data.frame(stock@stock.wt)
print(ggplot(data=stock_wt, aes(year, data)) + geom_line(aes(group=age, colour=factor(age)),size=1)+
  ggtitle("CSH Mean Weights in the stock")+
  ylab("weight(kg)")+
  facet_wrap(~age, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "none"))
dev.off()

###########################################################################################################
## Assessment plots
###########################################################################################################

## 1 Stock Summary Plots

summary <- read.taf("output/summary.csv")

taf.png("summary plots")
legend_title="Catches"
p1<-ggplot(summary,aes(x=Year,y=Catch)) + 
  geom_bar(stat="identity",colour="black")+ 
  ggtitle("Catches")+
  theme(plot.title=element_text(size=20))+
  ylab("catches (t)")+
  scale_colour_manual(legend_title,values=c("black"))   


legend_title="Rec"
p2<-ggplot(summary, aes(x=Year, y=Rec)) + 
  geom_bar(stat="identity",colour="black")+ 
  ggtitle("Recruitment 1 wr")+
  theme(plot.title=element_text(size=20))+
  ylab("rec")+
  scale_colour_manual(legend_title,values=c("black"))

legend_title="SSB"
p3<-ggplot() + 
  geom_line(data=summary, aes(x=Year, y=SSB),size=1)+ 
  ggtitle("SSB")+
  theme(plot.title=element_text(size=20))+
  ylab("SSB")+
  scale_colour_manual(legend_title,values=c("black"))


legend_title="F"
p4<-ggplot() + 
  geom_line(data=summary, aes(x=Year, y=Fbar),size=1)+ 
  ggtitle("Fishing Mortality 2-5")+
  theme(plot.title=element_text(size=20))+
  ylab("Mean F 2-5")+
  scale_colour_manual(legend_title,values=c("black"))


grid.arrange(
  p1,p2,
  p3,p4,
  nrow = 2)

dev.off()


## 2 Observed and predicted catch

taf.png("obs and pred catch")
catch <- data.frame(years,observed=c(asap$catch.obs),predicted=c(asap$catch.pred))
legend_title="Catch"
print(ggplot() + 
  geom_line(data=catch, aes(x=years, y=observed, colour="Observed"),size=1) + 
  geom_line(data=catch, aes(x=years, y=predicted, colour="Predicted"),size=1) +
  scale_colour_manual(legend_title,values=c("red","blue"))+
  ylab("catch")+
  ggtitle("Observed and Predicted Catch"))
dev.off()


## 3 Catch proportions at age observed and predicted
taf.png("Catch proportions at age")
catch_res <- data.frame(year=years,age=rep(ages,each=nyears),obs=c(asap$catch.comp.mats$catch.fleet1.ob),pred=c(asap$catch.comp.mats$catch.fleet1.pr))
catch_res$res <- catch_res$obs-catch_res$pred
legend_title="catch proportions"
print(ggplot() + 
  geom_line(data = catch_res, aes(x = year, y = obs, color = "obs")) +
  geom_line(data = catch_res, aes(x = year, y = pred, color = "pred")) + 
  scale_colour_manual(legend_title,values=c("red","blue"))+  
  facet_wrap(~age,scales="free")+labs(x="Year", y="Proportions") + theme(legend.position = "bottom")+
  ggtitle("Observed and Predicted Catch proportions at age"))
dev.off()




## 4 Selection in the fishery


taf.png("fishery selectivity")
sel_fishery <- stack(as.data.frame(asap$fleet.sel.mats$sel.m.fleet1))
names(sel_fishery) <- c('s','age')
sel_fishery$year <- years
sel_fishery$age <- as.numeric(as.character(sel_fishery$age))
print(ggplot() + 
  geom_line(data=sel_fishery, aes(x=age, y=s), colour="black",size=1.25) + 
  ggtitle("Selectivity at age Celtic Sea Herring Fishery")+
  ylab("selection")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)))
dev.off()



## 5 Selection in the survey
taf.png("index selectivity")
index_sel <- data.frame(name=paste('index',1:nindices),age=rep(ages,each=nindices),sel=c(asap$index.sel))
index_sel$sel <- ifelse(index_sel$sel<0,NA,index_sel$sel)
index_sel<-subset(index_sel, age<=7)
print(ggplot() + 
  geom_line(data=index_sel, aes(x=age, y=sel), colour="black",size=1.25) + 
  ggtitle("Selectivity at age Celtic Sea Herring Acoustic Survey")+
  ylab("selection")+
  scale_x_continuous(breaks=c(2,3,4,5,6,7)))
dev.off()


## 6 Index Fit
taf.png("index fit")
index_1<- data.frame(years=asap$index.year,observed=c(asap$index.obs),predicted=c(asap$index.pred))
names(index_1)<-c("year","Observed","Predicted")
legend_title="Index"
print(ggplot() + 
  geom_line(data=index_1, aes(x=year, y=Observed, colour="Observed"),size=1) + 
  geom_line(data=index_1, aes(x=year, y=Predicted, colour="Predicted"),size=1) +
  scale_colour_manual(legend_title,values=c("red","blue"))+
  ylab("Index")+
  ggtitle("Index Fit"))
dev.off()



##  Bubble Plots
## Bubble plot function 
bubbles <- function(x,z,cex, key.space = 'right',...){
  maxz <- max(abs(z),na.rm=T)
  panel.fun <- function(x,z,subscripts,cex,...){
    pt.cex <- sqrt(abs(z)/maxz)*cex
    pt.bg <- ifelse(z<0, '#FF000050','#00000050')
    lpoints(x,cex=pt.cex[subscripts],pch=21,fill=pt.bg[subscripts],col=1,...)
  }
  text <- as.character(round(seq(maxz,-maxz,length=6),2))
  key = list(space = key.space, text = list(text),
             points = list(pch = c(21), cex=sqrt(abs(seq(cex,-cex,length=6)))^2,
                           fill = rep(c('#00000050','#FF000050'),each=3)),
             rep = FALSE)
  xyplot(x,z=z,cex=cex,panel=panel.fun,key=key,...)
}

## 7 Catch residuals
taf.png("catch residuals")
print(bubbles(age~year,data=catch_res,z=catch_res$res,cex=5,xlab='Year',ylab='Age',main="Catch Proportions at age Residuals"))
dev.off()
## Survey Residuals

## 8 Survey Residuals
taf.png("survey residuals")
index_res <- data.frame(year=years,age=rep(ages,each=nyears),obs=c(asap$index.comp.mats$ind01.ob),pred=c(asap$index.comp.mats$ind01.pr))
index_res$res <- index_res$obs-index_res$pred
## Select the years where we have a survey index
index_res<-subset(index_res,year %in% seq(2002,2025))
print(bubbles(age~year,data=index_res,z=index_res$res,cex=5,xlab='Year',ylab='Age', main="Index proportions at age residuals"))
dev.off()


## 9 Internal consistency of the acoustic survey

taf.png("Survey Internal Consistency")
print(plot(stock.tun[[1]],type="internal", main="Celtic Sea Herring Acoustic Survey"))
dev.off()

###########################################################################################################
## Tables for the report
###########################################################################################################


natage <- read.taf("output/natage.csv")
natage <- round(natage)
write.taf(natage, dir="report")

fatage <- read.taf("output/fatage.csv")
fatage <- round(fatage, 3)
write.taf(fatage, dir="report")

summary <- read.taf("output/summary.csv")
summary$SSB <- round(summary$SSB)
summary$Fbar <- round(summary$Fbar, 3)
write.taf(summary, dir="report")

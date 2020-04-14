# Function-based Plotting growth of Covid cases/deaths by unit (country, state, province)
# (c) Brian Rubineau 
# start: 2020-04-12

rm(list=ls())
gc()

# Create Functions for Plotting Cumulative Events (Confirmed ases or Deaths) growth and New Cases 

# Cumulative Cases or Deaths
plotCum <- function(infile,startCount,eventType="cases",unitType="country"){
  # infile a data.frame with:
  #   counts of events by day, in order from earliest to most recent, with day name variables starting with "X"
  #   a "name" variable giving the name of the unit (state, province, country)
  # startCount is the threshold when starting to count (e.g. 10 deaths, 10 cases, 100 cases)
  # eventType is "cases" or "deaths", "cases" by default
  # unitType is "country", "state", or "province"; "country" by default
  
  dayData <- which(substr(names(infile),1,1)=="X")
  days <- length(dayData)
  infile$start <- as.numeric(unlist(apply(infile[,dayData],
                                          MAR=1,function(r){
                                            return(min(which(r>=startCount)))
                                          })))
  maxCount <- max(as.vector(infile[,dayData]),na.rm=T)
  ordMagCount <- ceiling(log(maxCount,base=10))
  gtStart <- which(infile$start != Inf)
  xax <- max(apply(infile[which(infile$name != "China"),dayData],MAR=1,function(r){ # remove China here to keep x-axis useful
    sum(as.numeric(r>=startCount))
  }))
  outPlot <- {
    plot(0:xax,rep(NA,xax+1), 
         xlab=paste("Days since",startCount,eventType,"in",unitType,sep=" "), 
         ylab=paste0("Cumulative ",eventType," (logged)"),
         main=paste0("Growth of Covid ",eventType," by ",unitType),
#         xlim=c(0,days-30), # China numbers started >1month before others, and extends the graph, compressing the left side
         xlim=c(0,xax), 
         ylim=c(log(startCount+1),
                log(10^ordMagCount+1)
         ), axes=F )
    axis(1)
    axis(2, at=log(10^(1:ordMagCount)+1),labels=10^(1:ordMagCount)) # left
    axis(2, at=log(1+unlist(sapply(1:ordMagCount,function(t){sapply(1:9,function(m){m*(10^t)})}))),labels=NA)
    abline(h=log(1+unlist(sapply(1:ordMagCount,function(t){sapply(1:9,function(m){m*(10^t)})}))),lwd=0.25,col="gray")
    for(c in gtStart){
      lines(x=0:(days-infile$start[c]),
            y=log(infile[c,(infile$start[c]+1):(days+1)]+1),
            col=rainbow(length(gtStart))[which(gtStart==c)],lwd=2)
      text(x=days-infile$start[c],
           y=log(infile[c,(days+1)]+1),
           labels=infile$name[c],col=rainbow(length(gtStart))[which(gtStart==c)])
    }
    text(x=xax/10,y=log(0.9*(10^ordMagCount)+1),
         labels=paste0("Data through: ",
                       as.Date(substr(names(infile)[max(dayData)],2,nchar(names(infile)[max(dayData)])),format="%m.%d.%y")))
  }
  
  return(outPlot)
}

# New Cases or Deaths
plotNew <- function(infile,eventType="cases",unitType="country"){
  # infile a data.frame with:
  #   counts of events by day, in order from earliest to most recent, with day name variables starting with "X"
  #   a "name" variable giving the name of the unit (state, province, country)
  # eventType is "cases" or "deaths", "cases" by default
  # unitType is "country", "state", or "province"; "country" by default
  
  dayData <- which(substr(names(infile),1,1)=="X")
  days <- length(which(substr(names(infile),1,1)=="X"))
  ordMagCount <- ceiling(log(max(infile[,dayData]),base=10))
  
  outPlot <- plot(0:ceiling(log(1+max(infile[,dayData]))),
                  rep(NA,ceiling(log(1+max(infile[,dayData])))+1), 
                  xlab=paste("Cumulative",eventType,"in",unitType,"(logged)",sep=" "), 
                  ylab=paste0("New ",eventType," in prior 5 days (logged)"),
                  main=paste0("Growth of Covid ",eventType," by ",unitType),
                  ylim=c(log(0+1),
                         log(10^ordMagCount+1)), 
                  axes=F)
  axis(1, at=log(10^(0:ordMagCount)+1),labels=10^(0:ordMagCount))
  axis(1, at=log(1+unlist(sapply(0:ordMagCount,
                                 function(t){sapply(1:9,function(m){m*(10^t)})}))),labels=NA)
  
  axis(2, at=log(10^(0:(ordMagCount))+1),labels=10^(0:ordMagCount)) # left
  axis(2, at=log(1+unlist(sapply(0:ordMagCount,
                                 function(t){sapply(1:9,function(m){m*(10^t)})}))),labels=NA)
  abline(h=log(1+unlist(sapply(0:ordMagCount,
                               function(t){sapply(1:9,function(m){m*(10^t)})}))),lwd=0.25,col="gray")
  
  for(c in 1:(dim(infile)[1])){
    lines(x=log(1+infile[c,dayData[5:length(dayData)]]),
          y=log(1+(infile[c,dayData[5:length(dayData)]]-
                     infile[c,dayData[1:(length(dayData)-4)]])),
          col=rainbow(dim(infile)[1])[c],lwd=2)
    text(x=log(1+infile[c,dayData[length(dayData)]]),
         y=log(1+(infile[c,dayData[length(dayData)]]-infile[c,dayData[length(dayData)-4]])),
         labels=as.character(infile$name)[c],col=rainbow(dim(infile)[1])[c])
  }
  return(outPlot)
}

# Read in Data files from JHU
# Prepare them for sending to plotting functions

# Global Death
deathfile<-'https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
d.df<-read.csv(deathfile)
d.country <- aggregate(d.df[,which(substr(names(d.df),1,1)=="X")],
                       by=list(d.df$Country.Region),FUN=sum,na.rm=T)
d.country$name <- as.character(d.country$Group.1)

# Global Cases
casesfile<-'https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
c.df<-read.csv(casesfile)
c.country <- aggregate(c.df[,which(substr(names(c.df),1,1)=="X")],
                       by=list(c.df$Country.Region),FUN=sum,na.rm=T)
c.country$name <- as.character(c.country$Group.1)

# US cases
uscasefile<-'https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
c.us.df <-read.csv(uscasefile)
c.us <- aggregate(c.us.df[,which(substr(names(c.us.df),1,1)=="X")],
                  by=list(c.us.df$Province_State),FUN=sum,na.rm=T)
c.us$name <- as.character(c.us$Group.1)

# US Deaths
usdeathfile <- 'https://raw.github.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv'
d.us.df <-read.csv(usdeathfile)
d.us <- aggregate(d.us.df[,which(substr(names(d.us.df),1,1)=="X")],
                  by=list(d.us.df$Province_State),FUN=sum,na.rm=T)
d.us$name <- as.character(c.us$Group.1)

# Canadian Cases
c.df$Cprov <- NA
c.df$Cprov[which(c.df$Country.Region=="Canada")] <- sapply(as.character(c.df$Province.State)[which(c.df$Country.Region=="Canada")],
                                                           function(s){
                                                             t <- unlist(strsplit(s,split=","))
                                                             return(trimws(t[length(t)]))
                                                           })
c.prov <- aggregate(c.df[which(c.df$Country.Region=="Canada"),
                         which(substr(names(c.df),1,1)=="X")],
                    by=list(c.df$Cprov[which(c.df$Country.Region=="Canada")]),FUN=sum,na.rm=T)
c.prov$name <- as.character(c.prov$Group.1)

# Canadian Deaths
d.df$Cprov <- NA
d.df$Cprov[which(d.df$Country.Region=="Canada")] <- sapply(as.character(d.df$Province.State)[which(d.df$Country.Region=="Canada")],
                                                           function(s){
                                                             t <- unlist(strsplit(s,split=","))
                                                             return(trimws(t[length(t)]))
                                                           })
d.prov <- aggregate(d.df[which(d.df$Country.Region=="Canada"),
                         which(substr(names(d.df),1,1)=="X")],
                    by=list(d.df$Cprov[which(d.df$Country.Region=="Canada")]),FUN=sum,na.rm=T)
d.prov$name <- as.character(d.prov$Group.1)

# Georgia & West Virginia Counties
# GA & WV Cases
c.ga <- aggregate(c.us.df[which(c.us.df$Province_State=="Georgia"),
                            which(substr(names(c.us.df),1,1)=="X")],
                  by=list(c.us.df$Admin2[which(c.us.df$Province_State=="Georgia")]),FUN=sum,na.rm=T)
c.ga$name <- as.character(c.ga$Group.1)

c.wv <- aggregate(c.us.df[which(c.us.df$Province_State=="West Virginia"),
                            which(substr(names(c.us.df),1,1)=="X")],
                    by=list(c.us.df$Admin2[which(c.us.df$Province_State=="West Virginia")]),FUN=sum,na.rm=T)
c.wv$name <- as.character(c.wv$Group.1)

# GA & WV Deaths
d.ga <- aggregate(d.us.df[which(d.us.df$Province_State=="Georgia"),
                          which(substr(names(d.us.df),1,1)=="X")],
                  by=list(d.us.df$Admin2[which(d.us.df$Province_State=="Georgia")]),FUN=sum,na.rm=T)
d.ga$name <- as.character(d.ga$Group.1)

d.wv <- aggregate(d.us.df[which(d.us.df$Province_State=="West Virginia"),
                          which(substr(names(d.us.df),1,1)=="X")],
                  by=list(d.us.df$Admin2[which(d.us.df$Province_State=="West Virginia")]),FUN=sum,na.rm=T)
d.wv$name <- as.character(d.wv$Group.1)

# Plot
# 2 x 3 (Cumulative vs. New x Country, States Provinces)
par(mfrow=c(2,3))
# 2 x 3 cases plot
plotCum(c.country,100,"cases","country")
plotCum(c.us,100,"cases","state")
plotCum(c.prov,100,"cases","province")

plotNew(c.country,"cases","country")
plotNew(c.us,"cases","state")
plotNew(c.prov,"cases","province")

# 2 x 3 deaths plot
plotCum(d.country,10,"deaths","country")
plotCum(d.us,10,"deaths","state")
plotCum(d.prov,10,"deaths","province")

plotNew(d.country,"deaths","country")
plotNew(d.us,"deaths","state")
plotNew(d.prov,"deaths","province")

# GA & WV
par(mfrow=c(2,4))
plotCum(c.ga,10,"cases","GA county")
plotCum(d.ga,1,"deaths","GA county")

plotCum(c.wv,10,"cases","WV county")
plotCum(d.wv,1,"deaths","WV county")

plotNew(c.ga,"cases","GA county")
plotNew(d.ga,"deaths","GA county")

plotNew(c.wv,"cases","WV county")
plotNew(d.wv,"deaths","WV county")


par(mfrow=c(1,1))

# next steps:
# bring in population data
# plot cases normalized by population

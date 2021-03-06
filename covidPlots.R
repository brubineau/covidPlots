# Function-based Plotting growth of Covid cases/deaths by unit (country, state, province)
# (c) Brian Rubineau 
# start: 2020-04-12

rm(list=ls())
gc()

# Create Functions for Plotting Cumulative Events (Confirmed ases or Deaths) growth and New Cases 

getSubset <- function(infile,unitType,pastWindow=90){
  if.stack.days <- as.Date(substr(names(infile)[which(substr(names(infile),1,1)=="X")],
                                  2,
                                  stop=nchar(names(infile)[which(substr(names(infile),1,1)=="X")])),
                           format="%m.%d.%y")
  startDay <- max(if.stack.days)-pastWindow
  endDay <- max(if.stack.days)
  dayRange <- as.numeric(endDay - startDay)
  top5recent <- infile$name[(order(infile[,date2column(endDay,infile)]-infile[,date2column(endDay,infile)-13],decreasing = T)[1:5])] # use last 2 weeks 
  top5total <- infile$name[(order(infile[,date2column(endDay,infile)],decreasing = T)[1:5])]
  plus <- NA
  restText <- NA
  if (unitType=="state") {
    plus <- c("Georgia","West Virginia")
    restText <- "rest.of.US"
  }
  else if (unitType=="country") {
    plus <- c("US","Israel","Canada")
    restText <- "rest.of.World"
  }
  subsetNames <- sort(unique(c(top5recent,top5total,plus)))
  df.stack <- infile
  df.stack$Group.1 <- as.character(df.stack$Group.1)  
  df.stack$name <- as.character(df.stack$name)
  df.stack$Group.1[which(!(infile$Group.1 %in% subsetNames))] <- restText
  df.stack$name[which(!(infile$Group.1 %in% subsetNames))] <- restText
  df.stack <- aggregate(df.stack[,which(substr(names(df.stack),1,1)=="X")],by=list(df.stack$name),FUN=sum,na.rm=T)
  df.stack$name <- as.character(df.stack$Group.1)
  return(df.stack)
}

# Stacked graph of selected US states

date2column <- function(d,df){
  which(names(df)== paste0("X",
                           paste(as.numeric(format(d,"%m")),
                                 as.numeric(format(d,"%d")),
                                 as.numeric(format(d,"%y")),collapse=".",sep=".")))}




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
    text(x=1,y=log(0.9*(10^ordMagCount)+1),pos=4,
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
  days <- length(dayData)
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
    ys <- log(1+(sapply(1:(length(dayData)-4),function(d){
      mean(diff(as.numeric(infile[c,dayData]))[d:(d+4)],na.rm=T)})))
    lines(x=log(1+infile[c,dayData[5:length(dayData)]]),
          # y=log(1+(infile[c,dayData[5:length(dayData)]]-
          #            infile[c,dayData[1:(length(dayData)-4)]])),
          y=ys,
          col=rainbow(dim(infile)[1])[c],lwd=2)
    text(x=log(1+infile[c,dayData[length(dayData)]]),
         y=ys[length(ys)],
         labels=as.character(infile$name)[c],col=rainbow(dim(infile)[1])[c])
  }
  text(x=1,
       y=log(0.9*(10^ordMagCount)+1),
       pos=4,
       labels=paste0("Data through: ",
                     as.Date(substr(names(infile)[max(dayData)],
                                    2,
                                    nchar(names(infile)[max(dayData)])),format="%m.%d.%y")))
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

# Plot
# 2 x 3 (Cumulative vs. New x Country, States Provinces)
par(mfrow=c(2,3))
# 2 x 3 cases plot
plotCum(getSubset(c.country,"country"),100,"cases","country")
plotCum(getSubset(c.us,"state"),100,"cases","state")
plotCum(c.prov,100,"cases","province")

plotNew(getSubset(c.country,"country"),"cases","country")
plotNew(getSubset(c.us,"state"),"cases","state")
plotNew(c.prov,"cases","province")

# 2 x 3 deaths plot
plotCum(getSubset(d.country,"country"),10,"deaths","country")
plotCum(getSubset(d.us,"state"),10,"deaths","state")
plotCum(d.prov,10,"deaths","province")

plotNew(getSubset(d.country,"country"),"deaths","country")
plotNew(getSubset(d.us,"state"),"deaths","state")
plotNew(d.prov,"deaths","province")

par(mfrow=c(1,1))

# Plotting County Info for specific US States
plotState <- function(stateName){
  c.st <- aggregate(c.us.df[which(c.us.df$Province_State==stateName),
                            which(substr(names(c.us.df),1,1)=="X")],
                    by=list(c.us.df$Admin2[which(c.us.df$Province_State==stateName)]),FUN=sum,na.rm=T)
  c.st$name <- as.character(c.st$Group.1)
  d.st <- aggregate(d.us.df[which(d.us.df$Province_State==stateName),
                            which(substr(names(d.us.df),1,1)=="X")],
                    by=list(d.us.df$Admin2[which(d.us.df$Province_State==stateName)]),FUN=sum,na.rm=T)
  d.st$name <- as.character(d.st$Group.1)
  par(mfrow=c(2,2))
  plotCum(c.st,10,"cases",paste0(state.abb[which(state.name==stateName)]," county"))
  plotCum(d.st,1,"deaths",paste0(state.abb[which(state.name==stateName)]," county"))
  plotNew(c.st,"cases",paste0(state.abb[which(state.name==stateName)]," county"))
  plotNew(d.st,"deaths",paste0(state.abb[which(state.name==stateName)]," county"))
  par(mfrow=c(1,1))
}
# plotState("Georgia")
# plotState("West Virginia")


# Plotting Country-specific Info with comparisons
plotCountry <- function(countryName){
  c.restOfWorld <- rep("Rest of World",dim(c.country)[1])
  c.restOfWorld[which(c.country$Group.1=="US")] <- "US"
  c.restOfWorld[which(c.country$Group.1=="Canada")] <- "Canada"
  c.restOfWorld[which(c.country$Group.1==countryName)] <- countryName
  c.ct <- rbind(aggregate(c.country[,which(substr(names(c.country),1,1)=="X")],
                    by=list(c.restOfWorld),FUN=sum,na.rm=T),
                aggregate(c.country[,which(substr(names(c.country),1,1)=="X")],
                          by=list(rep("Whole World",dim(c.country)[1])),FUN=sum,na.rm=T))
  c.ct$name <- as.character(c.ct$Group.1)
  
  d.restOfWorld <- rep("Rest of World",dim(d.country)[1])
  d.restOfWorld[which(d.country$Group.1=="US")] <- "US"
  d.restOfWorld[which(d.country$Group.1=="Canada")] <- "Canada"
  d.restOfWorld[which(d.country$Group.1==countryName)] <- countryName
  d.ct <- rbind(aggregate(d.country[,which(substr(names(d.country),1,1)=="X")],
                          by=list(d.restOfWorld),FUN=sum,na.rm=T),
                aggregate(d.country[,which(substr(names(d.country),1,1)=="X")],
                          by=list(rep("Whole World",dim(d.country)[1])),FUN=sum,na.rm=T))
  d.ct$name <- as.character(d.ct$Group.1)
  par(mfrow=c(2,2))
  plotCum(c.ct,10,"cases"," country")
  plotCum(d.ct,1,"deaths"," country")
  plotNew(c.ct,"cases"," country")
  plotNew(d.ct,"deaths"," country")
  par(mfrow=c(1,1))
}
# plotCountry("Sweden")
# plotCountry("Israel")

countryCompare <- function(countryNameVector){
  par(mfrow=c(2,2))
  plotCum(c.country[which(c.country$Group.1 %in% countryNameVector),],10,"cases","country")
  plotCum(d.country[which(d.country$Group.1 %in% countryNameVector),],1,"deaths","country")
  plotNew(c.country[which(c.country$Group.1 %in% countryNameVector),],"cases","country")
  plotNew(d.country[which(d.country$Group.1 %in% countryNameVector),],"deaths","country")
  par(mfrow=c(1,1))
  
}

# countries <- c("US","Canada","Israel","Korea, South","Singapore","Sweden","Switzerland","Italy","Spain","France","United Kingdom","Denmark","Norway","Finland","Lituania")
# countryCompare(countries)

# Plot Coarse Fatality Rate
plotFatality <- function(caseFile,deathFile,startCount=10,unitType){
  commonDays <- intersect(names(caseFile)[which(substr(names(caseFile),1,1)=="X")],
                          names(deathFile)[which(substr(names(deathFile),1,1)=="X")])
  c.Days <- which(names(caseFile) %in% commonDays)
  d.Days <- which(names(deathFile) %in% commonDays)
  commonNames <- sort(intersect(caseFile$name,deathFile$name))
  fr.df <- Reduce(rbind,
                  lapply(as.list(commonNames),
                  function(n){
                    deathFile[which(deathFile$name==n),d.Days]/
                      pmax(caseFile[which(caseFile$name==n),c.Days],1)
  }))
  names(fr.df) <- commonDays
  fr.df$name <- commonNames
  fr.df$start <- as.numeric(unlist(apply(deathFile[,commonDays],
                                          MAR=1,function(r){
                                            return(min(which(r>=startCount)))
                                          })))
  gtStart <- which(fr.df$start != Inf)
  xax <- length(commonDays) - min(fr.df$start[intersect(gtStart,which(fr.df$name != "China"))])
  plot(0:xax,rep(NA,xax+1), 
         xlab=paste("Days since",startCount," deaths in",unitType,sep=" "), 
         ylab="fatality rate (deaths/cases)",
         main=paste0("Changes in Covid Fatality Rate by ",unitType),
         xlim=c(0,xax), 
         ylim=c(0,0.15))
  for(c in gtStart){
    lines(x=0:(length(commonDays)-fr.df$start[c]),
          y=(fr.df[c,commonDays][fr.df$start[c]:length(commonDays)]),
          col=rainbow(length(gtStart))[which(gtStart==c)],lwd=2)
    text(x=length(commonDays)-fr.df$start[c],
         y=fr.df[c,commonDays][length(commonDays)],
         labels=fr.df$name[c],
         col=rainbow(length(gtStart))[which(gtStart==c)])
  }
}
# par(mfrow=c(2,1))
# plotFatality(c.country,d.country,10,"country")
# plotFatality(c.us,d.us,10,"state")
# par(mfrow=c(1,1))

# next steps:
# bring in population data
# plot cases normalized by population

sepStates <- c("California","New Jersey","New York","Washington","West Virginia","Georgia")

# par(mfrow=c(2,2))
# # 2 x 3 cases plot
# plotCum(c.us[which(!(c.us$Group.1 %in% sepStates)),],100,"cases","state")
# plotCum(d.us[which(!(c.us$Group.1 %in% sepStates)),],10,"deaths","state")
# plotNew(c.us[which(!(c.us$Group.1 %in% sepStates)),],"cases","state")
# plotNew(d.us[which(!(c.us$Group.1 %in% sepStates)),],"deaths","state")
# par(mfrow=c(1,1))

# c.us.ny <- c.us
# c.us.ny$Group.1 <- as.character(c.us.ny$Group.1)  
# c.us.ny$name <- as.character(c.us.ny$name)
# c.us.ny$Group.1[which(!(c.us$Group.1 %in% sepStates))] <- "Rest.of.US"
# c.us.ny$name[which(!(c.us$Group.1 %in% sepStates))] <- "Rest.of.US"
# c.us.ny <- aggregate(c.us.ny[,which(substr(names(c.us.ny),1,1)=="X")],by=list(c.us.ny$name),FUN=sum,na.rm=T)
# c.us.ny$name <- as.character(c.us.ny$Group.1)
# 
# d.us.ny <- d.us
# d.us.ny$Group.1 <- as.character(d.us.ny$Group.1)  
# d.us.ny$name <- as.character(d.us.ny$name)
# d.us.ny$Group.1[which(!(c.us$Group.1 %in% sepStates))] <- "Rest.of.US"
# d.us.ny$name[which(!(c.us$Group.1 %in% sepStates))] <- "Rest.of.US"
# d.us.ny <- aggregate(d.us.ny[,which(substr(names(d.us.ny),1,1)=="X")],by=list(d.us.ny$name),FUN=sum,na.rm=T)
# d.us.ny$name <- as.character(d.us.ny$Group.1)

# par(mfrow=c(2,2))
# # 2 x 3 cases plot
# plotCum(getSubset(c.us,"state"),100,"cases","state")
# plotCum(getSubset(d.us,"state"),10,"deaths","state")
# plotNew(getSubset(c.us,"state"),"cases","state")
# plotNew(getSubset(d.us,"state"),"deaths","state")
par(mfrow=c(1,1))



plotStacked <- function(infile,eventType,unitType,pastWindow=90){
  if.stack.days <- as.Date(substr(names(infile)[which(substr(names(infile),1,1)=="X")],
                                  2,
                                  stop=nchar(names(infile)[which(substr(names(infile),1,1)=="X")])),
                           format="%m.%d.%y")
  startDay <- max(if.stack.days)-pastWindow
  endDay <- max(if.stack.days)
  dayRange <- as.numeric(endDay - startDay)
  # top5recent <- infile$name[(order(infile[,date2column(endDay,infile)]-infile[,date2column(endDay,infile)-13],decreasing = T)[1:5])] # use last 2 weeks 
  # top5total <- infile$name[(order(infile[,date2column(endDay,infile)],decreasing = T)[1:5])]
  # stackStates <- union(c("Georgia",top5recent),c("West Virginia",top5total))
  # df.stack <- infile
  # df.stack$Group.1 <- as.character(df.stack$Group.1)  
  # df.stack$name <- as.character(df.stack$name)
  # df.stack$Group.1[which(!(infile$Group.1 %in% stackStates))] <- "Rest.of.US"
  # df.stack$name[which(!(infile$Group.1 %in% stackStates))] <- "Rest.of.US"
  # df.stack <- aggregate(df.stack[,which(substr(names(df.stack),1,1)=="X")],by=list(df.stack$name),FUN=sum,na.rm=T)
  # df.stack$name <- as.character(df.stack$Group.1)
  df.stack <- getSubset(infile,unitType,pastWindow)
  df.14.ma.stack <- (df.stack[,(date2column(startDay,df.stack)):(date2column(endDay,df.stack))]-
                       df.stack[,(date2column(startDay,df.stack)-13):(date2column(endDay,df.stack)-13)])/14
  names(df.14.ma.stack) <- names(df.stack)[date2column(startDay,df.stack):date2column(endDay,df.stack)]
  df.14.ma.stack$name <- df.stack$name
  df.stack.max <- max(colSums(df.14.ma.stack[,1:(dim(df.14.ma.stack)[2]-1)]))
  plot(1:(dayRange+10),
       rep(NA,dayRange+10),axes=F,
       ylim=c(0,df.stack.max),
       # xlab="days",
       # ylab=eventType,
       main=paste0("New daily ",eventType," (averaging over prior 14 days) in the past 3 months"))
  axis(1,labels=if.stack.days[which(if.stack.days <= endDay & if.stack.days>=startDay)],
       at=1+(0:dayRange))
  axis(2)
  for(c in 1:(dim(df.14.ma.stack)[1])){
    bottom <- NA
    if (c==1) {
      bottom <- rep(0,dayRange+1)
    }
    else {
      bottom <- rev(colSums(df.14.ma.stack[1:(c-1),1:(dim(df.14.ma.stack)[2]-1)]))
    }
    polygon(x=c(1+(0:dayRange),seq(dayRange+1,1,-1)),
            y=c(colSums(df.14.ma.stack[1:c,1:(dim(df.14.ma.stack)[2]-1)]),
                bottom),
            col=rainbow(dim(df.14.ma.stack)[1])[c])
    text(x=dayRange+1,
         y=(sum(df.14.ma.stack[1:c,dim(df.14.ma.stack)[2]-1])+
              bottom[1])/2,
         labels=as.character(df.14.ma.stack$name)[c],
         col=rainbow(dim(df.14.ma.stack)[1])[c],
         adj=0)
  }
}
par(mfrow=c(2,1),mar=c(2,2,4,2))
plotStacked(c.us,"cases","state",pastWindow = 270)
plotStacked(d.us,"deaths","state",pastWindow = 270)
par(mfrow=c(1,1),mar=0.1+c(5,4,4,2)) # return to defaults

par(mfrow=c(2,1),mar=c(2,2,4,2))
plotStacked(c.country,"cases","country",pastWindow = 270)
plotStacked(d.country,"deaths","country",pastWindow = 270)
par(mfrow=c(1,1),mar=0.1+c(5,4,4,2)) # return to defaults

# par(mfrow=c(2,1),mar=c(2,2,4,2))
# plotStacked(c.us[which(c.us$Group.1!="New Jersey" & c.us$Group.1!="New York"),],"cases")
# plotStacked(d.us[which(d.us$Group.1!="New Jersey" & d.us$Group.1!="New York"),],"deaths")
# par(mfrow=c(1,1),mar=0.1+c(5,4,4,2)) # return to defaults


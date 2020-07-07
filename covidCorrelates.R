# combine county-level covid-19 data in US with
# county-level location data from google
# county-level demographic data (age, racial/ethnic) from census / cps

rm(list=ls())
gc()
setwd("C:/Users/bribr/OneDrive - McGill University/Research/covidPlots")
source("./covidPlots.R")

gmr <- read.csv("Global_Mobility_Report.csv")
us.mr <- gmr[which(gmr$country_region=="United States"),]
rm(gmr)
# us.mr$census_fips_code matches c.us.df$FIPS
# get county population by FIPS
co.pop <- read.csv("co-est2019-annres.csv",skip=3,header=T)
co.pop$pop2019 <- as.numeric(gsub(",","",co.pop$X2019,fixed=T))
co.pop$county <- sapply(co.pop$X,function(s){
  cname <- NA
  if (as.numeric(regexpr(", ",s))>0) {
    c1 <- unlist(strsplit(s,split=", "))[1]
    c2 <- nchar(c1)
    if (as.numeric(regexpr("County",c1))>0){
      c2 <- as.numeric(regexpr(" County",c1))
    }
    cname <- substr(c1,2,c2)
  }
  return(trimws(cname))
})
co.pop$state <- sapply(co.pop$X,function(s){
  sname <- NA
  if (as.numeric(regexpr(",",s))>0) {
    sname <- unlist(strsplit(s,split=", "))[2]
  }
  return(trimws(sname))
})
counties <- unique(us.mr[,c("sub_region_1","sub_region_2","census_fips_code")])
counties$county <- gsub(" County","",counties$sub_region_2)
names(counties)[1] <- "state"

co.pop <- merge(co.pop,counties[,c("state","county","census_fips_code")])
co.pop$caseThresh <- ceiling(co.pop$pop2019*0.001)

# once there are at least 50 documented COVID-19 cases in a county,
# AND - at least 100 per 100k population (=0.1%),
# are changes in percentages of google users in residential locations
# associated with changes in covid-19 cases (5-7 days later)?


# First, do for a single county (e.g., Cook, IL, Middlesex, MA)
# Then do for all counties

# try as if ols

# reshape county data
# only observations once threshold of 100/100k and at least 50 cases are reached
dateVars <- names(c.us.df)[which(substr(names(c.us.df),1,1)=="X")]
varDates <- as.Date(sapply(dateVars,function(dv){
  mdy <- unlist(strsplit(substr(dv,2,nchar(dv)),split=".",fixed=T))
  return(as.Date(paste(paste0("20",mdy[3]),mdy[1],mdy[2],sep="-",collapse="-")))
}))

c.us.df2 <- merge(c.us.df,co.pop[,c("census_fips_code","caseThresh")],
                  by.x="FIPS",by.y="census_fips_code")
c.us.df2$threshDate <- as.Date(sapply(1:(dim(c.us.df2)[1]),function(r){
  varDates[min(which(c.us.df2[r,dateVars] >= max(50,c.us.df2$caseThresh[r])))]
}))


ts.c.us.df <- reshape(data=c.us.df2[which(c.us.df2$FIPS %in% co.pop$census_fips_code &
                                            !(is.na(c.us.df2$threshDate))),
                                   c("FIPS","Admin2","Province_State","Combined_Key","threshDate",names(c.us.df2)[which(substr(names(c.us.df2),1,1)=="X")])],
                      varying=names(c.us.df2)[which(substr(names(c.us.df2),1,1)=="X")],
                      times=varDates,
                      direction="long",
                      v.names="cases")


ts.c.us.df$daysSinceThresh <- as.numeric(ts.c.us.df$time - ts.c.us.df$threshDate)
ts.c.us.df <- ts.c.us.df[which(ts.c.us.df$daysSinceThresh >= 0),]
ts.c.us.df2 <- merge(ts.c.us.df,us.mr[,c("census_fips_code","date","residential_percent_change_from_baseline")],
                     by.x=c("FIPS","time"),
                     by.y=c("census_fips_code","date"))

lag <- 5
obs <- dim(ts.c.us.df2)[1]
# taking a looong time.
# maybe add the lagged case counts via changing date then merging
gLag <- ts.c.us.df2[,c("FIPS","time","residential_percent_change_from_baseline")]
gLag$lagDate <- as.Date(gLag$time + lag)
names(gLag)[which(names(gLag)=="residential_percent_change_from_baseline")] <- "lag5rpcfb"

ts.c.us.df3 <- merge(ts.c.us.df2,gLag[,c("FIPS","lagDate","lag5rpcfb")],
                     by.x=c("FIPS","time"),
                     by.y=c("FIPS","lagDate"))

gLag2 <- ts.c.us.df2[,c("FIPS","time","residential_percent_change_from_baseline")]
gLag2$lagDate <- as.Date(gLag2$time + lag + 1)
names(gLag2)[which(names(gLag2)=="residential_percent_change_from_baseline")] <- "lag6rpcfb"

ts.c.us.df3 <- merge(ts.c.us.df3,gLag2[,c("FIPS","lagDate","lag6rpcfb")],
                     by.x=c("FIPS","time"),
                     by.y=c("FIPS","lagDate"))

cLag <- ts.c.us.df2[,c("FIPS","time","cases")]
cLag$lagDate <- as.Date(cLag$time + 1)
names(cLag)[which(names(cLag)=="cases")] <- "lag1cases"

ts.c.us.df3 <- merge(ts.c.us.df3,
                     cLag[,c("FIPS","lagDate","lag1cases")],
                     by.x=c("FIPS","time"),
                     by.y=c("FIPS","lagDate"))

# ts.c.us.df2$lagRpcfb <- sapply(1:(dim(ts.c.us.df2)[1]),function(r){
#   county <- ts.c.us.df2$FIPS[r]
#   date2find <- as.Date(ts.c.us.df2$time - lag)
#   return(us.mr$residential_percent_change_from_baseline[which(
#     us.mr$census_fips_code==county &
#       us.mr$date==date2find)])
# })
# ts.c.us.df2$lag1cases <- sapply(1:(dim(ts.c.us.df2)[1]),function(r){
#   county <- ts.c.us.df2$FIPS[r]
#   date2find <- as.Date(ts.c.us.df2$time - 1)
#   caseCol <- date2column(date2find,c.us.df)
#   return(c.us.df[which(c.us.df$FIPS==county),caseCol])
# })

dist.test <- lm(cases~lag1cases+lag5rpcfb+lag6rpcfb+factor(FIPS),data=ts.c.us.df3)
summary(dist.test)
save(ts.c.us.df3,file="covidPlusGoogleData.RData")
# Goal - not forecast, but effect of people leaving home


# Question - counties and dates of BLM protests register in google data?

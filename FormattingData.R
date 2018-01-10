library(lubridate)
library(plyr)
load('data/chldata.Rda')
#Mean monthly chl for overall lake
monthlysummary <- function(data,lake){
  datasub=subset(data,Lake==lake)
  datasub$Month=months(datasub$Date)
  datasub$Year=year(datasub$Date)
  monthly=ddply(datasub, c("Year", "Month"), summarise,
        Mean=mean(Value),
        Max=max(Value),
        StDev=sd(Value))
  return(monthly)
}

GSL.monthly <- monthlysummary(chldata,"GSL")
Utah.monthly <- monthlysummary(chldata,"Utah")
Farmington.monthly <- monthlysummary(chldata,"Farmington")

#Mean monthly chl for specific locations (single or small group of stations)

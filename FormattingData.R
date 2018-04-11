library(lubridate)
library(plyr)
load('data/rschlstationdata.Rda')
load('data/rschllakedata.Rda')
load('data/climatedata.Rda')
load('data/lakeelevationdata.Rda')
load('data/streamflow.Rda')
load('data/gslwwtpdata.Rda')



#Calculate monthly summary values
monthlysummary <- function(data,subcol,subval,sumfun,paramcol){
  data$param <- data[,paramcol]
  data$Subset <- data[,subcol]
  datasub=subset(data,Subset==subval)
  datasub$Month=months(datasub$Date)
  datasub$Year=year(datasub$Date)
  if(sumfun=="mean"){
    monthly=ddply(datasub, c("Year", "Month"), summarise,
                  Value=mean(param))
  }else if(sumfun=="sum"){
    monthly=ddply(datasub, c("Year", "Month"), summarise,
                  Value=sum(param))
  }
  return(monthly)
}

GSL.monthly <- monthlysummary(chldata,"GSL")
Utah.monthly <- monthlysummary(chldata,"Utah")
Farmington.monthly <- monthlysummary(chldata,"Farmington")

#Utah Lake
utahlakeavgchl <- monthlysummary(lakestats,"Lake","Utah Lake","mean","Chl_mean")
utahlakeelevation <- monthlysummary(lakeelevationdata,"Lake","Utah Lake","mean","Elevation_ft")




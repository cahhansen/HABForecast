### Date 12/25/09 - Update 4/23/17
###  This script can be run in its entirety without interaction 

### This code for handling the GSL level data to be first of month records used in 
### in BioWest study.
##  USGS 10010000 GREAT SALT LAKE AT SALTAIR BOAT HARBOR, UT 
###  is the south arm while
#### USGS 10010100 GREAT SALT LAKE NEAR SALINE, UT
###  is the north arm.
###  The south arm as well as the north arm records started to be daily from 1989-10-01 

# This also outputs data for the full record on the days there were observations

levread.r=function(file="gsl_data_south.txt",skip=31)
{
####reading the data
data<-read.table(file,skip=skip,fill=TRUE)  # 4/23/17 changed skip from 28 to 31 for changed USGS format

#time_in_days gives the original date from the USGS file
#original levels gives the levels from the USGS file
dt<-as.character(data$V3)
level<-as.numeric(as.character(data$V4))  # This required to convert characters, such as Eqp to NA
index=!is.na(level)
list(dt=dt[index],level=level[index])
}

#urlS="http://waterdata.usgs.gov/nwis/dv?cb_72020=on&format=rdb&begin_date=1840-01-01&site_no=10010000&referred_module=sw"
# 4/23/17.  Discovered parameter code had changed
urlS="http://waterdata.usgs.gov/nwis/dv?cb_62614=on&format=rdb&begin_date=1840-01-01&site_no=10010000&referred_module=sw"
# Manually retrieved and 1847 to 1901 merged in
name="GSL_south_arm.txt"
download.file(urlS,name,quiet=TRUE)
LevS_new=levread.r(name)
LevS_old=levread.r("GSL_south_arm_2016-03-01.txt",skip=28)

LevS_new$dt[1:4]
LevS_old$dt[291:294]
LevS_new$level[1:4]
LevS_old$level[291:294]
indtoadd=which(LevS_old$dt<LevS_new$dt[1])
LevS=list(dt=c(LevS_old$dt[indtoadd],LevS_new$dt),level=c(LevS_old$level[indtoadd],LevS_new$level))

today <- Sys.Date()
nameS=paste("GSL_south_arm_",today,".txt",sep="")
file.copy(name,nameS)  #  Keep dated copy

#urlN="http://waterdata.usgs.gov/nwis/dv?cb_72020=on&format=rdb&begin_date=1900-05-20&site_no=10010100&referred_module=sw"
urlN="http://waterdata.usgs.gov/nwis/dv?cb_62614=on&format=rdb&begin_date=1900-05-20&site_no=10010100&referred_module=sw"
name="GSL_north_arm.txt"
download.file(urlN,name,quiet=TRUE)
LevN=levread.r("GSL_north_arm.txt")
nameN=paste("GSL_north_arm_",today,".txt",sep="")
file.copy(name,nameN)  #  Keep dated copy

enddt=as.Date(LevS$dt[length(LevS$dt)])
endy=as.numeric(format(enddt,'%Y'))
endm=as.numeric(format(enddt,'%m'))
if(endm<10)endy=endy-1  # have not reached end of water year yet
# Computing mean levels in S Arm for full record statistics
enddt=as.Date(paste(endy,"-9-30",sep=""))
end_of_wy=seq(as.Date("1847-9-30"),enddt,by="year")
beg_of_wy=seq(as.Date("1847-10-1"),as.Date(paste(endy-1,"-10-1",sep="")),by="year")
lastlevdt=as.Date(paste(endy,"-10-1",sep=""))

Smean=rep(NA,length(end_of_wy)-1)  #  Creating arrays
Dmean=rep(as.Date(LevS$dt[1]),length(end_of_wy)-1)

for(i in 2:length(end_of_wy))
{
indt= (as.Date(LevS$dt)>end_of_wy[i-1] & as.Date(LevS$dt) <=end_of_wy[i])
Smean[i-1]=mean(LevS$level[indt])
Dmean[i-1]=mean(as.Date(LevS$dt[indt]))
}

windows(11,8.5)
mar=par("mar")
mar[4]=4.1
par(mar=mar)
plot(as.Date(LevS$dt),LevS$level,ylab="Level (ft)",xlab="",ylim=c(min(LevS$level,LevN$level),
  max(LevN$level,LevN$level)),type="l")
lines(as.Date(LevN$dt),LevN$level,col=2)
lines(as.Date(Dmean),Smean,lwd=2)
legend("top",col=c(1,2,1),legend=c("South Arm USGS 10010000 Great Salt Lake at Saltair boat harbour, UT",
"North Arm USGS 10010100 Great Salt Lake near Saline, UT",
"South Arm water year mean"),bty="n",lty=1,lwd=c(1,1,2),y.intersp=1.2,cex=0.9)

# Quantiles
lquantiles <- quantile(Smean,  probs=c(0.16,0.5,0.84), type=6)  # type 6 is Weibull
lquantiles
mean(Smean)

# 5/4/14 update
#> lquantiles
#16%      50%      84% 
#4195.731 4200.776 4205.460 
#> mean(Smean)
#[1] 4200.82

#  10/26/12 update
#>lquantiles
#     16%      50%      84% 
#4195.713 4200.876 4205.480 
#> mean(Smean)
#[1] 4200.848


#5/19/2012 Update
#> lquantiles
#     16%      50%      84% 
#4195.695 4200.888 4205.500 
#> mean(Smean)
#[1] 4200.866

# 4/1/2011 Update
# lquantiles
#     16%      50%      84% 
#4195.677 4200.900 4205.520 
#> mean(Smean)
#[1] 4200.896


# 2010 update
#> lquantiles
# $     16%      50%      84% 
#4195.615 4200.508 4205.649 
#> mean(Smean)
#[1] 4200.808
#> 


lines(c(as.Date("1847-9-30"),enddt),rep(lquantiles[1],2),lty=2,col=4)
lines(c(as.Date("1847-9-30"),enddt),rep(lquantiles[2],2),lty=2,lwd=2,col=4)
lines(c(as.Date("1847-9-30"),enddt),rep(lquantiles[3],2),lty=2,col=4)

lquantiles2 <- quantile(Smean[103:length(Smean)],  probs=c(0.16,0.5,0.84), type=6)  # quantiles for post 1949 period
lquantiles2
lines(c(as.Date("1949-9-30"),enddt),rep(lquantiles2[1],2),lty=2,col=3)
lines(c(as.Date("1949-9-30"),enddt),rep(lquantiles2[2],2),lty=2,lwd=2,col=3)
lines(c(as.Date("1949-9-30"),enddt),rep(lquantiles2[3],2),lty=2,col=3)
mean(Smean[103:length(Smean)])

lq=lquantiles[1]
text(x=as.Date("1860-9-30"),y=as.numeric(lq)+.3,labels=paste(names(lq),round(lq,1)),cex=0.9)
lq=lquantiles[2]
text(x=as.Date("1875-9-30"),y=as.numeric(lq)+.3,labels=paste(names(lq),round(lq,1)),cex=0.9)
lq=lquantiles[3]
text(x=as.Date("1900-9-30"),y=as.numeric(lq)+.3,labels=paste(names(lq),round(lq,1)),cex=0.9)


lq=lquantiles2[1]
text(x=as.Date("1983-9-30"),y=as.numeric(lq)+.3,labels=paste(names(lq),round(lq,1)),cex=0.9)
lq=lquantiles2[2]
text(x=as.Date("1962-3-30"),y=as.numeric(lq)+.3,labels=paste(names(lq),round(lq,1)),cex=0.9)
lq=lquantiles2[3]
text(x=as.Date("1962-3-30"),y=as.numeric(lq)+.3,labels=paste(names(lq),round(lq,1)),cex=0.9)

text(x=as.Date("1992-3-30"),y=4211,labels=paste("Max 4211.6 on\n1872-06-27\n1986-06-23"),cex=0.9,adj=c(0,0.5))
text(x=as.Date("1944-1-01"),y=4191.8,labels=paste("Min 4191.4 on\n1963-10-15"),cex=0.9,adj=c(0,0.5))

text(x=as.Date("2000-1-01"),y=4191.8,labels=paste(LevS$dt[length(LevS$dt)],"\nN",LevN$level[length(LevN$dt)],"\nS",LevS$level[length(LevS$dt)]),cex=0.9,adj=c(0,0.5))
title("GSL Level Record",cex.main=2.5,line=0.7)
title(sub=paste("10/1/1949 -",format(enddt,"%m/%d/%Y")," period with complete climate and streamflow data"),line=1.9,adj=1,col.sub="red")
meterlabs=(1276+(0:7)*2)
axis(4,at=meterlabs/.3048,labels=meterlabs)
mtext("Level (m)",4,2)

tab=matrix("",nrow=2,ncol=5)
tab[1,1]=paste("1847-10-1","to",enddt)
tab[2,1]=paste("1949-10-1","to",enddt)
tab[1,2]=round(mean(Smean),1)
tab[2,2]=round(mean(Smean[103:length(Smean)]),1)
tab[1,3:5]=round(lquantiles,1)
tab[2,3:5]=round(lquantiles2,1)
write.csv(tab,"levstats.csv")
# 2012 end
#> lquantiles2
#     16%      50%      84% 
#4194.840 4198.854 4202.316 
#> mean(Smean[103:165])
#[1] 4198.848

# 2011 end
#> lquantiles2
#     16%      50%      84% 
#4194.830 4198.856 4202.450 
#> mean(Smean[103:164])
#[1] 4198.864

# 2010 end
#> lquantiles2
#     16%      50%      84% 
#4194.811 4198.858 4202.540 
# mean(Smean[103:163])
# [1] 4198.911

# 2009 end
#    16%      50%      84% 
# 4195.641 4200.592 4205.650 
# > mean(Smean)
# [1] 4200.847


# Latest
LevN$dt[length(LevN$dt)]
LevN$level[length(LevN$dt)]
LevS$dt[length(LevS$dt)]
LevS$level[length(LevS$dt)]

#[1] "2013-04-15"
#[1] 4196.2
#[1] "2013-04-15"
#[1] 4196.9


#[1] "2012-10-25"
#[1] 4195.4
#[1] "2012-10-25"
#[1] 4196.1

#[1] "2012-05-18"
#[1] 4197.7
#[1] "2012-05-18"
#[1] 4198.7

# [1] "2011-08-24"
# [1] 4197
# [1] "2011-08-24"
# [1] 4197.9


windows(8,5)
hist(Smean,nclass=20,main="South Arm Level distribution",xlab="Level (ft)")
box()
dd=density(Smean)
lines(dd$x,dd$y*length(Smean),col=2)
legend("topright","Density - R default",bty="n",lty=1,col=2)

# Output the year average data
SWyrLevels=data.frame(beg_of_wy, end_of_wy[2:length(end_of_wy)],as.Date(Dmean),Smean)
colnames(SWyrLevels)<-c("Start Date","End Date","Average Date", "Average Level (ft)")
write.table(SWyrLevels,file="SWyrLevels.csv",row.names=FALSE,sep=",")


##  Now output 1 value per month on the first day of the month
monthlevwrite.r=function(LevRaw,start="1915-1-1",end="2012-10-1",file="Levels.csv")
{
months=seq(as.Date(start),as.Date(end),by="month")

a<-unclass(as.Date(LevRaw$dt))
b<-unclass(months)
jj<-match(b,a)
index<-(1:length(jj))[is.na(jj)==FALSE]	
record<-rep(0,length(months))

for(j in 1:length(months))
		{
		record[j]<-LevRaw$level[jj[j]]
		}
tt<-data.frame(as.Date(months),record,record*0.3048)
colnames(tt)<-c("Date","Level_ft","Level_m")
write.table(tt,file=file,row.names=FALSE,sep=",")
}

daylevwrite.r=function(LevRaw,file="DayLevels.csv")
{
tt<-data.frame(Date=as.Date(LevRaw$dt),Level_ft=LevRaw$level)
write.table(tt,file=file,row.names=FALSE,sep=",")
}


monthlevwrite.r(LevS,file="S_Levels.csv",end=lastlevdt)
monthlevwrite.r(LevN,file="N_Levels.csv",end=lastlevdt)

# Daily levels are required for salinity interpolation
daylevwrite.r(LevN,"NorthArmLevelDay.csv")
daylevwrite.r(LevS,"SouthArmLevelDay.csv")

#  Additional quantiles
Ssub=Smean[103:length(Smean)]
lquantiles3 <- quantile(Ssub, probs=c(0.05,0.16,0.25,0.5,0.75,0.84,0.95,0.96), type=6)
lquantiles3

tab=matrix("",nrow=2,ncol=11)
tab[2,1]=paste("1949-10-1","to",enddt)
tab[2,2]=round(min(Ssub),3)
tab[2,3]=round(max(Ssub),3)
tab[1,2:3]=c("Min","Max")
tab[1,4:11]=names(lquantiles3)
tab[2,4:11]=round(lquantiles3,3)
write.csv(tab,"levstats2.csv")

windows(8,5)
recentfirst=2010
recentlast=2017
plot(as.Date(LevS$dt),LevS$level,ylab="Level (ft)",xlab="",ylim=c(4188,4200),
   xlim=as.Date(c(paste(recentfirst,"-10-1",sep=""),paste(recentlast,"-10-1",sep=""))),type="l")
lines(as.Date(LevN$dt),LevN$level,col=2)

beg_of_wyrecent=seq(as.Date(paste(recentfirst,"-1-1",sep="")),as.Date(paste(recentlast,"-1-1",sep="")),by="year")
axis(1,at=beg_of_wyrecent,labels=recentfirst:recentlast)
legend("bottomright",col=c(1,2),legend=c("South Arm USGS 10010000 Great Salt Lake at Saltair boat harbour, UT",
"North Arm USGS 10010100 Great Salt Lake near Saline, UT"),bty="n",lty=1,lwd=c(1,1),y.intersp=1.2,cex=0.9)
title("Recent GSL Level Record")

####### Read Level Area & Volume tables
bathtab=read.csv("Bathymetry/GSLM_Bathymetry_noProposedPonds.csv")

arean=bathtab$northa+bathtab$northponda
voln=bathtab$northv+bathtab$northpondv
areas=bathtab$southa+bathtab$brba+bathtab$farmba+bathtab$brmbra
vols=bathtab$southv+bathtab$brbv+bathtab$farmbv+bathtab$brmbrv

LAVN=data.frame(level=bathtab$Level*0.3048,area=arean*4047,vol=voln*1233.5256)
LAVS=data.frame(level=bathtab$Level*0.3048,area=areas*4047,vol=vols*1233.5256)
LAVC=data.frame(level=bathtab$Level*0.3048,area=(arean+areas)*4047,vol=(voln+vols)*1233.5256)  # Combined Bathymetry


LAVMAG=LAVS  # S Arm with MagCorp ponds
LAVMAG$area=LAVS$area+bathtab$maga*4047
LAVMAG$vol=LAVS$vol+bathtab$magv*1233.5256

windows(8,5)
plot(LAVN$level, LAVN$area,type="l")
plot(LAVS$level, LAVS$area,type="l")
lines(LAVMAG$level, LAVMAG$area,col=2)

plot(LAVN$level/0.3048, LAVN$area/4047,type="l")
plot(LAVS$level/0.3048, LAVS$area/4047,type="l")

plot(LAVN$level/0.3048, LAVN$vol/1233.5256,type="l")
plot(LAVS$level/0.3048, LAVS$vol/1233.5256,type="l")
lines(LAVMAG$level/0.3048, LAVMAG$vol/1233.5256,col=2)


source("GSLFunctions.R")  # run script to create functions

# Get levels data

Slevels=read.csv("S_levels.csv",header=T)
Nlevels=read.csv("N_levels.csv",header=T)
Slevels=read.csv("S_levels.csv",header=T)
Nlevels=read.csv("N_levels.csv",header=T)
plot(as.Date(Slevels$Date),Slevels$Level_ft,type="l")
lines(as.Date(Nlevels$Date),Nlevels$Level_ft,col=2)
legend("topleft",col=c(1,2),legend=c("North Arm","South Arm"),bty="n",lty=1,y.intersp=1.2)

##  Missing N levels are taken as the same as S levels, essentially prior to construction of causeway
##  Some uncertainty across transition that we have to accept

ii=is.na(Nlevels$Level_ft)
Nlevels$Level_ft[ii]=Slevels$Level_ft[ii]
Nlevels$Level_m[ii]=Slevels$Level_m[ii]


###Now getting the north areas and volume time series in metric units
###The input levels from data frame (a$Level_North_m) will be used
north_levels<-Nlevels$Level_m
north_areas<-data.frame(Area_North_sq_m=area_volume.r(LAVN,north_levels)$area)
north_volumes<-data.frame(Volume_North_cu_m=area_volume.r(LAVN,north_levels)$vol)

###Now getting the south areas and volumes time series in metric units
###The input levels from data frame (a$Level_South_m) will be used
south_levels<-Slevels$Level_m
south_areas<-data.frame(Area_South_sq_m=area_volume.r(LAVS,south_levels)$area)
south_volumes<-data.frame(Volume_South_cu_m=area_volume.r(LAVS,south_levels)$vol)

time=as.Date(Slevels$Date)
plot(time,north_areas$Area_North_sq_m,ylab="Area (sq m)",xlab="",type="l")
plot(time,south_areas$Area_South_sq_m,ylab="Area (sq m)",xlab="",type="l")

#  Adjust for magCorp ponds
# Construction started 1970 according to Tripp 2009, but production only really started 1975.  
# 1970 is used for pond closure date because the lake is rising during this period and I wanted a low level
#  to minimize the discontinuity that results when taking differences due to switching bathymetry curves
imag=as.Date(Slevels$Date) < as.Date("1970-1-1") | (as.Date(Slevels$Date) > as.Date("1986-6-1") &  as.Date(Slevels$Date) < as.Date("1994-1-1"))
south_areas$Area_South_sq_m[imag]=area_volume.r(LAVMAG,south_levels[imag])$area
lines(time,south_areas$Area_South_sq_m,col=2)

#  Show dates of MagCorp pond addition
lines(c(as.Date("1970-1-1"),as.Date("1970-1-1")),c(0,4e9))
lines(c(as.Date("1986-6-1"),as.Date("1986-6-1")),c(0,4e9))
lines(c(as.Date("1994-1-1"),as.Date("1994-1-1")),c(0,4e9))

plot(time,north_volumes$Volume_North_cu_m,ylab="Volume (cu. m)",xlab="",type="l")

south_volumes_NoMag=south_volumes
plot(time,south_volumes_NoMag$Volume_South_cu_m,ylab="Volume (cu. m)",xlab="",type="l")

south_volumes$Volume_South_cu_m[imag]=area_volume.r(LAVMAG,south_levels[imag])$vol
lines(time,south_volumes$Volume_South_cu_m,col=2)
#  Show dates of pond addition
lines(c(as.Date("1970-1-1"),as.Date("1970-1-1")),c(0,4e10))
lines(c(as.Date("1986-6-1"),as.Date("1986-6-1")),c(0,4e10))
lines(c(as.Date("1994-1-1"),as.Date("1994-1-1")),c(0,4e10))

# combined data frame
combdf=data.frame(Date=Slevels$Date, S_Level_ft=Slevels$Level_ft, N_level_ft=Nlevels$Level_ft,S_level_m=Slevels$Level_m,N_level_m=Nlevels$Level_m,
                  S_area_m2=south_areas$Area_South_sq_m,N_area_m2=north_areas$Area_North_sq_m,
                  S_vol_m3=south_volumes$Volume_South_cu_m,N_vol_m3=north_volumes$Volume_North_cu_m,Total_vol_m3=south_volumes$Volume_South_cu_m+north_volumes$Volume_North_cu_m)
plot(as.Date(combdf$Date),combdf$Total_vol_m3,type="l")
write.table(combdf,file="GSLLevelVol.csv",row.names=FALSE,sep=",")

# Get areas and volumes for original data

# Assemble data frame of daily data corresponding to S levels.  Fill in missing N levels with S levels
indn=match(as.Date(LevN$dt),as.Date(LevS$dt))
NLevelfilled=LevS$level   # Fill with S arm levels
NLevelfilled[indn]=LevN$level  # overwrite with N arm levels where they exist
Nlevel=rep(NA,length(LevS$dt))  # fill with NA
Nlevel[indn]=LevN$level  # overwrite with N arm levels where they exist

plot(as.Date(LevS$dt),Nlevel,type="l")
lines(as.Date(LevS$dt),NLevelfilled,col=2)
lines(as.Date(LevS$dt),Nlevel,col=4)

Sarea=area_volume.r(LAVS,LevS$level*.3048)$area
Svol=area_volume.r(LAVS,LevS$level*.3048)$vol
Narea=area_volume.r(LAVN,NLevelfilled*.3048)$area
NVol=area_volume.r(LAVN,NLevelfilled*.3048)$vol

#Mag Corp Bathymetry adjustments
iodmag=as.Date(LevS$dt) < as.Date("1970-1-1") | (as.Date(LevS$dt) > as.Date("1986-6-1") &  as.Date(LevS$dt) < as.Date("1994-1-1"))

plot(as.Date(LevS$dt),Sarea,type="l")
Sarea[iodmag]=area_volume.r(LAVMAG,LevS$level[iodmag]*.3048)$area
lines(as.Date(LevS$dt),Sarea,col=2)
plot(as.Date(LevS$dt),Svol,type="l")
Svol[iodmag]=area_volume.r(LAVMAG,LevS$level[iodmag]*.3048)$vol
lines(as.Date(LevS$dt),Svol,col=2)


daydf=data.frame(Date=as.Date(LevS$dt),Slevel_ft=LevS$level,Slevel_m=LevS$level*.3048,Sarea_m2=Sarea,Svol_m3=Svol,
                 Nlevel_ft=Nlevel,Nlevel_m=NLevelfilled*.3048,Narea_m2=Narea,Nvol_m3=NVol,
                 TotalArea_m2=Sarea+Narea,TotalVol_m3=Svol+NVol)

write.table(daydf,file="GSLLAV.txt",row.names=FALSE)

# Clean up unneeded files
filestodelete=c("N_Levels.csv", "S_Levels.csv", "NorthArmLevelDay.csv","SouthArmLevelDay.csv",
                "levstats.csv","SWyrLevels.csv","levstats2.csv")
for(fn in filestodelete){
  if (file.exists(fn)) file.remove(fn)
}




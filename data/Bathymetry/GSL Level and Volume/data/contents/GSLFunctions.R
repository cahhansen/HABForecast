### Area and volume interpolation function
area_volume.r= function(lev_area_vol,xin)
{
#This function takes level as input and outputs corresponding volume and area using interpolation

area_out=approx(lev_area_vol$level,lev_area_vol$area,xin) 
volume_out=approx(lev_area_vol$level,lev_area_vol$vol,xin) 

list(vol=volume_out$y,area=area_out$y)
}

# Annual total functions
annsum.r=function(x){
n=length(x)/12
sm=rep(NA,n)
for(i in 1:n)
{
i1=i*12-11
i2=i*12
sm[i]=sum(x[i1:i2])
}
sm
}
anndate.r=function(dt){
n=length(dt)/12
dtan=rep(dt[1],n)
for(i in 1:n)
{
i1=i*12-11
dtan[i]=dt[i1]
}
dtan
}

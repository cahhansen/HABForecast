Great Salt Lake Level and Volume  
David Tarboton
Last updated 4/23/17

Time series of level, area and volume in the Great Salt Lake. Volume and area of the Great Salt Lake are derived from recorded levels and bathymetry.  The bathymetry used is included.  Bathymetry is adjusted for the presence or absence of Magnesium corps pond.  The area of the evaporation pond is not regarded as part of the lake except prior to its construction and during the time it was overtopped. 

GSL_north_arm.txt is measured data from the USGS station 10010100 GREAT SALT LAKE NEAR SALINE, UT 
GSL_north_arm_2017-04-23.txt  Duplicate of above as run on 4/23/17

GSL_south_arm.txt is measured data from the USGS station 10010000 GREAT SALT LAKE AT SALTAIR BOAT HARBOR, UT
GSL_south_arm_2017-04-23.txt Duplicate of above as run on 4/23/17
GSL_south_arm_2016-03-01.txt Record downloaded 3/1/2016 that includes data prior to USGS changing format

GSLLAV.txt is time series of level, computed area and volume from level using bathymetry

Bathymetry folder.  Lake bathymetry used in these calculations.  This data is also stored separately in https://www.hydroshare.org/resource/b26090299ec947c692d4ee4651815579/    

GSLLevelVol.csv is beginning of month time series of level and volume from 1/1/1915 used for modeling 

LevelVolWork.R is the R script used to process this data

GSLLevelRecord.pptx  Powerpoint file with some figures of this data

GSLFunctions.R  R functions used by the script

Headings should be obvious.  Note that separate levels in the north arm only started being recorded in 1966 so for dates prior to that Nlevel_ft is reported as NA (no data in R).  Nlevel_m is converted from the measurement in ft, and filled in using the south arm when there is no north arm data (a few days after 1966 are also missing).  The bathymetry was then used to compute area and volume in each arm separately and add them up.
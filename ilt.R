rm(list=ls())
library("readr")
library("plyr")
library("dplyr")
library("png")
library("tidyr")
library("data.table")

folder<-paste0(getwd(),"/data/ralaha_ilt/")
waterfile<-paste0(getwd(),"/gis/Sound500m_centroid_water.csv")


file="iltsvind"

#========== Define x,y locations of individual cells and the value which the colours found there represent
use_colour_categories=TRUE
use_colour_scale=FALSE 
colourpoints<-c(1858,1960,0,
                1990,1044,1)

valuefromrgb<-function(nrgb,maprgb)
{
  maprgb$dist1<-maprgb[,1]-nrgb[1]
  maprgb$dist2<-maprgb[,2]-nrgb[2]
  maprgb$dist3<-maprgb[,3]-nrgb[3]
  maprgb$dist<-maprgb$dist1^2+maprgb$dist2^2+maprgb$dist3^2
  maprgb<-arrange(maprgb,dist)
  return(maprgb[1,4])
}

map=readPNG(paste0(folder,file,".png"), FALSE)


#Separate channels and arrange into lists

dtR<-data.table(map[,,1])
dtR$Y<-as.numeric(rownames(dtR))
dtR<-gather(dtR, "X", "R", -Y)
dtR$X<-as.numeric(substr(dtR$X,2,99999))
dtG<-data.table(map[,,2])
dtG$Y<-as.numeric(rownames(dtG))
dtG<-gather(dtG, "X", "G", -Y)
dtG$X<-as.numeric(substr(dtG$X,2,99999))
dtB<-data.table(map[,,3])
dtB$Y<-as.numeric(rownames(dtB))
dtB<-gather(dtB, "X", "B", -Y)
dtB$X<-as.numeric(substr(dtB$X,2,99999))


dtRGB<-left_join(dtR,dtG,by=c("X"="X","Y"="Y"))
dtRGB<-left_join(dtRGB,dtB,by=c("X"="X","Y"="Y"))
dtRGB<-as.data.table(dtRGB)

#If method using RGB values from specific points is active then find the colours and corresponding values
#i.e. "Categorical" colours

  mPoints=matrix(colourpoints,nrow=3) 
  mPoints=t(mPoints)
  dfPoints<-as.data.frame(mPoints)
  colnames(dfPoints)<-c("X","Y","Value")
  
  # Find RGB values for the unique points
  dfPoints<-left_join(dfPoints,dtRGB,by=c("X"="X","Y"="Y"))
  dfPoints<-select(dfPoints,SR=R,SG=G,SB=B,Value)
  
  # Add the unique points to the list of RGBs from the scale
  dt_scaleRGB2<-dfPoints

#Check that dt_scaleRGB is data.table before the heavy calculations
dt_scaleRGB2<-as.data.table(dt_scaleRGB2)

# Get all distinct combinations of R,G,B from the map
dtRGBdistinct<-as.data.table(distinct(dtRGB,R,G,B))

# Cartesian product of distinct RGB and scale RGB values
dtRGB2<-setkey(dtRGBdistinct[,c(k=1,.SD)],k)[dt_scaleRGB2[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]

# Find mean square distance between RGB (map) and RGB (scale) for all combinations
dtRGB2[, dist:=((R-SR)^2+(G-SG)^2+(B-SB)^2)]

# Find the minimum mean square distance for each RGB (map)
dtRGB3<-dtRGB2 %>%
  group_by(R,G,B) %>%
  summarise(min=min(dist))

# select the combination of RGB (map) and RGB (scale) with the smallest distance
dtRGB4<-left_join(dtRGB3,select(dtRGB2,R,G,B,dist,Value),by=c("R"="R","G"="G","B"="B","min"="dist"))

# For a few RGB (map) combinations there can more than one RGB (scale) at the same distance
# Take the average of the value for the matching RGB scale values.
dtRGB5<-dtRGB4 %>%
  group_by(R,G,B) %>%
  summarise(Value=mean(Value))

# Replace values for specific RGB combinations with values from user defined combinations
if(use_RGB_list){
  
  mRGB=matrix(RGBvalues,nrow=4)
  mRGB=t(mRGB)
  dfRGB<-as.data.frame(mRGB)
  colnames(dfRGB)<-c("R","G","B","Value")
  
  dfRGB$R<-dfRGB$R/255
  dfRGB$G<-dfRGB$G/255
  dfRGB$B<-dfRGB$B/255
  dtRGB5A<-anti_join(dtRGB5,dfRGB,by=c("R","G","B"))
  dtRGB5B<-inner_join(dtRGB5,dfRGB,by=c("R","G","B"))
  dtRGB5B$Value<-dtRGB5B$Value.y
  dtRGB5B$Value.y<-NULL
  dtRGB5B$Value.x<-NULL
  dtRGB5<-rbind(dtRGB5A,dtRGB5B)
  
}
#Remove "almost black"
dtRGB5$Value<-ifelse(dtRGB5$R+dtRGB5$G+dtRGB5$B<(100/255),NA,dtRGB5$Value)

#Remove "almost grey (land boundaries)"

ngrey<-(110/255)
dtRGB5$X<-((dtRGB5$R-ngrey)^2+(dtRGB5$G-ngrey)^2+(dtRGB5$B-ngrey)^2)

dtRGB5$Value<-ifelse(dtRGB5$X<0.02,NA,dtRGB5$Value)
dtRGB5$X<-NULL



#Check that dtRGB5 is data.table
dtRGB5<-as.data.table(dtRGB5)


dtValue<-left_join(dtRGB,dtRGB5,by=c("R","G","B"))
dtValue<-arrange(select(dtValue,X,Y,Value),X,Y)
dtValueWide<-dtValue %>% spread(X, Value)
dtValueWide$Y<-NULL

mapvalues<-as.matrix(dtValueWide)
# mapout<-map
# 
# mapout[,,1]<-mapvalues
# mapout[,,2]<-mapvalues
# mapout[,,3]<-mapvalues
# 
# writePNG(mapout,paste0(folder,file,"_conv.png"))
# save(mapout,file=paste0(folder,file,"_conv.Rda"))

# ----------------------------------------------------------

#source("regridEIM.R")
require(dplyr)
require(raster)
require(ggplot2)
require(rasterVis)

rast_in <- raster(mapvalues)
crs(rast_in)<-CRS("+init=epsg:3035")
extent(rast_in) = c(3875412.1,4751464.1,3454009.3,3931992.3)
nx1<-ncol(rast_in)
ny1<-nrow(rast_in)
plot(rast_in)


nx=109
ny=227
xmin=4459500
xmax=4514000
ymin=3578000
ymax=3691500
rast<-raster(nrow=ny,ncol=nx)
crs(rast)<-CRS("+init=epsg:3035")
extent(rast)<-c(xmin,xmax,ymin,ymax)

rast<-resample(rast_in,rast,method='bilinear') #ngb'
plot(rast)

# get the values from the raster. It's a simple list of nx * ny values
# starting from y=ny,x=1

df<-as.data.frame(rast)
df$i <- seq(1:nrow(df))
df$x <- df$i %% nx
df$y <- ny- (df$i %/% nx)
df$y <- df$y+ifelse(df$x==0,1,0)
df$x <- ifelse(df$x==0,nx,df$x)

# a%%b - remainder of a/b
# a%/%b - quotient of a/b

dx<-(xmax-xmin)/nx
dy<-(ymax-ymin)/ny


df$XCoord<-xmin+df$x*dx-(dx/2)
df$YCoord<-ymin+df$y*dy-(dy/2)

# IMPORTANT - check if EcoImpactMapper refers to cells by their centre points or their bottom-left points

var<-names(df)[1]
df<-df %>% select_(.dots=c('XCoord','YCoord',var)) %>%
  mutate(XCoord=as.integer(XCoord),YCoord=as.integer(YCoord))


# ---------------------------------------------

dfwater <- read.table(waterfile,fileEncoding = "Windows-1252",
                      sep = ",",stringsAsFactors = F,header = T)

df<-df %>% left_join(dfwater,by=c("XCoord","YCoord")) %>% 
  filter(!is.na(ID500M)) %>% select(-ID500M)


# ---------------------------------------------
fileout<-paste0(file,".csv")
write.table(df,file=paste0(folder,fileout), row.names=FALSE,quote=FALSE,sep=',', na="")


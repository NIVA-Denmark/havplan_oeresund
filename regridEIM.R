

regridEIM<-function(folder,file,fileout,waterfile,nx=109,ny=227,xmin=4459500,xmax=4514000,ymin=3578000,ymax=3691500){
  
  require(dplyr)
  require(raster)
  require(ggplot2)
  require(rasterVis)
  
  # ------ raster info --------------------------
  # nx<-109
  # ny<-227
  # xmin<-4459500
  # xmax<-4514000
  # ymin<-3578000
  # ymax-3691500
  # folder<-"C:/Data/GitHub/Havplan_Oeresund/data/"
  # file<-"cod_abundance.tif"
  # fileout<-"cod_abundance.csv"
  # waterfile<-"C:/Data/GitHub/Havplan_Oeresund/gis/Sound500m_centroid_water.csv"
  
  # ---------------------------------------------
  
  rast_in<-raster(paste0(folder,file))
  plot(rast_in)
  
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
    filter(!is.na(ID500M))
  
  
  # ---------------------------------------------
  
  write.table(df,file=paste0(folder,fileout), row.names=FALSE,quote=FALSE,sep=',', na="")
  
  return(0)
}

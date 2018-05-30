library(dplyr)

folder<-"C:/Data/GitHub/Havplan_Oeresund/gis/"

file<-"Sound500m_centroid.csv"
df1 <- read.table(paste0(folder,file),fileEncoding = "Windows-1252",
                        sep = ",",stringsAsFactors = F,header = T) %>%
  select(XCoord,YCoord,ID500M)

file<-"Sound500m_centroid_intersect.csv"
df2 <- read.table(paste0(folder,file),fileEncoding = "Windows-1252",
                 sep = ",",stringsAsFactors = F,header = T) %>% 
  select(XCoord,YCoord,ID500M) %>%
  mutate(Land=1)

df1<-df1 %>% left_join(df2,by=c("XCoord","YCoord","ID500M"))

df2 <- df1 %>% filter(is.na(Land)) %>% select(-Land)


file<-"Sound500m_centroid_water.csv"
#write.table(df2,file=paste0(folder,file), row.names=FALSE,quote=FALSE,sep=',', na="")

file<-"Sound500m_selection.csv"
df3 <- read.table(paste0(folder,file),fileEncoding = "Windows-1252",
                  sep = ",",stringsAsFactors = F,header = T) %>%
  select(XCoord,YCoord,ID500M) %>%
  mutate(drop=1)
  
df2<-df2 %>% left_join(df3,by=c("XCoord","YCoord","ID500M")) %>% 
  filter(is.na(drop)) %>% select(-drop)

file<-"Sound500m_centroid_water.csv"
#write.table(df2,file=paste0(folder,file), row.names=FALSE,quote=FALSE,sep=',', na="")



xmin<-min(df2$XCoord)-250
xmax<-max(df2$XCoord)+250
ymin<-min(df2$YCoord)-250
ymax<-max(df2$YCoord)+250
nx<-(xmax-xmin)/500
ny<-(ymax-ymin)/500



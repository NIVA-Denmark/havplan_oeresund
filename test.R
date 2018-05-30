rm(list=ls())
library(readr)
test
 

source("regridEIM.R")

 workfolder<-"C:/Data/GitHub/Havplan_Oeresund/data/"
 infile<-"cod_abundance.tif"
 outfile<-"cod_abundance.csv"
 waterfile<-"C:/Data/GitHub/Havplan_Oeresund/gis/Sound500m_centroid_water.csv"
 
 #regridEIM(folder=workfolder,file=infile,fileout=outfile,waterfile=waterfile)
 
 names <- list.files(path=workfolder,pattern="*.tif")

 for(i in names){
   outfile<-paste0(substr(i,1,nchar(i)-3),"csv")
   cat(paste0("now I'm reading file ",i,"\n"))  
   regridEIM(folder=workfolder,file=i,fileout=outfile,waterfile=waterfile)
}
library(raster)
library(ggplot2)
library(rasterVis)

folder<-"K:/Prosjekter/NIVA_DK/Havplan Øresund/HavplanOeresund/Datasæt fra HELCOM/helcom downloads/helcom downloads/Biodiversity/Ecosystem components/Fish/"
file<-"cod_abundance.tif"

rast<-raster(paste0(folder,file))

plot(rast)


gplot(rast) + geom_tile(aes(fill=value,alpha=0.8))

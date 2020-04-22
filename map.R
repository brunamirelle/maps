# install.packages("scales")
# install.packages("maptools")
# install.packages("plotly")
# install.packages("sf")
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("mapproj")
# install.packages("raster") 
rm(list=ls())
library(ggplot2)
library("scales")
library("maptools")
library("plotly")
library("sf")
library("rgdal")
library("rgeos")
library("mapproj")
library("raster") 
library("RColorBrewer")
library(stringr)
library(data.table)
library(ggthemes)
library("maps")

#import shape file


dataset_indice = read.csv2("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX", sep=";", stringsAsFactors = F)
painel_medidas_final = read.csv2("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",stringsAsFactors = FALSE)

painel_medidas_final =  painel_medidas_final[,c(4:25)]

dataset_indice$date = gsub("mar","-03-",dataset_indice$date)
dataset_indice$date = gsub("apr","-04-",dataset_indice$date)
dataset_indice$date = gsub("feb","-02-",dataset_indice$date)

split_date = (str_split(dataset_indice$date,"-", simplify = TRUE))

dataset_indice$date = paste(split_date[,3],split_date[,2],split_date[,1],sep="-")
colnames(painel_medidas_final)[2] = "name_state"
painel_covid = merge(dataset_indice,painel_medidas_final,by=c("date","name_state"), all = T)

painel_covid$indice_isolamento = as.numeric(painel_covid$indice_isolamento)

# painel_covid= painel_covid[complete.cases(painel_covid$indice_isolamento),]
#  painel_covid= painel_covid[complete.cases(painel_covid$pesotot),]

# painel_covid = data.table(painel_covid)
#painel_covid[, indice_isolamento := weighted.mean(indice_isolamento, pesotot), by=c("id_state","date")]
#painel_covid = as.data.frame(painel_covid)
#painel_covid  = setNames(aggregate(painel_covid[, c(11)], list(painel_covid$name_state, painel_covid$date, painel_covid$id_state), mean),c("name_state","date","id_state", "indice_isolamento"))

# path_shp = "/home/brunamirelle/Documentos/br_unidades_da_federacao/BRUFE250GC_SIR.shp"
path_shp = "/home/brunamirelle/Documentos/br_municipios/BRMUE250GC_SIR.shp"
setwd("/home/brunamirelle/Documentos/br_municipios/")
#mun.shp <- readOGR(dsn = path_shp, layer = "BRUFE250GC_SIR") %>% spTransform(CRS(" +init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
mun.shp <- readOGR(dsn = path_shp, layer = "BRMUE250GC_SIR") %>% spTransform(CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
#import demo data
#panel_covid from time_series.R
#  mun.shp <- gBuffer(mun.shp, byid=TRUE, width=0)
mun.dt <- painel_covid[painel_covid$date=="2020-02-04",]
colnames(mun.dt)[3] <- "id_mun"

#fortify shape file to get into dataframe 
mun.shp.f <- fortify(mun.shp, region = "CD_GEOCMU")
# mun.shp.f <- fortify(mun.shp, region = "CD_GEOCUF")

#rename the id column to match the CountryCode from the other dataset
colnames(mun.shp.f)[6] <- "id_mun"

mun.dt = mun.dt[,c("date","name_state","id_mun","name_municipality","id_state","indice_isolamento")]
mun.shp.f = as.data.frame(mun.shp.f)
mun.shp.f$id_mun = as.numeric(mun.shp.f$id_mun)

#merge with CountryCodes
merge.shp <-merge(mun.shp.f, mun.dt, by="id_mun", all.x=TRUE)
fp <-merge.shp[order(merge.shp$order), ] 
#delete countries that not match
fp$indice_isolamento = as.numeric(fp$indice_isolamento)
#final.plot <- filter(fp, indice_isolamento != "NA") 
rm(list="dataset_indice","painel_covid","split_date","merge.shp","mun.shp.f", "mun.dt","mun.shp.f")
gc()
#basic plot
ggplot()  + 
  geom_polygon(data = fp, aes(x = long, y = lat, group = group, fill =indice_isolamento), color = "black", size = 0.05) + 
  coord_map() + 
  scale_fill_distiller(name="Index", palette = "Reds", limits = c(0.18,0.7), direction = 1, na.value="white")  + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.position = c(0.82, 0.2)
  ) 
ggsave("map_mun_04_02_2020r.png")
#  if(!is.null(dev.list())) dev.off()
gc()
rm(list=ls())
.rs.restartR()  
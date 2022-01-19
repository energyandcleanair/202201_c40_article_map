library(sf)
library(readxl)
library(tidyverse)
library(sp)
library(sf)

# setwd("~/development/crea/studies/202201_c40_article_map/data/")
d <- readxl::read_xlsx("gcpt_july2021.xlsx", "Units")
d_sf <- d %>%
  filter(Status %in% c("operating")) %>%
  group_by(Plant, Parent) %>%
  summarise(lat=mean(Latitude), lon=mean(Longitude),
            capaciy_mw=sum(`Capacity (MW)`, na.rm=T)) %>%
  sf::st_as_sf(coords=c("lon","lat"))


# Clustering based on distance
# https://gis.stackexchange.com/questions/64392/finding-clusters-of-points-based-distance-rule-using-r/64395#64395
distance_km <- 200

cluster <- function(d_sf, distance_km){
  st_crs(d_sf) <- CRS("+init=epsg:4326")
  xy <- as(d_sf, "Spatial")
  xy <- spTransform(xy, CRS("+init=epsg:3857"))
  chc <- hclust(dist(data.frame(rownames=rownames(xy@data), x=coordinates(xy)[,1],
                                y=coordinates(xy)[,2])), method="complete")
  chc.d <- cutree(chc, h=distance_km*1E3) 
  xy@data <- data.frame(xy@data, cluster=chc.d)
  
  
  d_clustered <- sf::st_as_sf(xy) %>%
    group_by(cluster) %>%
    summarise(capacity_mw=sum(capaciy_mw, na.rm=T),
              geometry=sf::st_centroid(sf::st_union(geometry))) %>%
    sf::st_transform(CRS("+init=epsg:4326"))
  
  return(d_clustered)
}



lapply(c(10,50,100,200), function(distance_km){
  file_path=sprintf("gcpt_july2021_operating_clustered%dkm.geojson", distance_km)
  file.remove(file_path)
  sf::st_write(cluster(d_sf, distance_km), file_path)
})

 

library(sf)
library(readxl)
library(tidyverse)

# setwd("~/development/crea/studies/202201_c40_article_map/data/")
d <- readxl::read_xlsx("gcpt_july2021.xlsx", "Units")

d %>%
  filter(Status %in% c("operating")) %>%
  group_by(Plant, Parent) %>%
  summarise(lat=mean(Latitude), lon=mean(Longitude)) %>%
  sf::st_as_sf(coords=c("lon","lat")) %>%
  sf::write_sf("gcpt_july2021_operating.geojson")

library(tidyverse)

db <- hdms::connect_dashboard()

syn_list <- db %>% tbl("syn_list") %>% collect()
et <- db %>% tbl("et") %>% collect()
mt <- read_rds("./master_taxonomy")
mt_sci <- mt$name
mt_com <- mt$common_name

DBI::dbDisconnect(db)

basemaps <- c(
  `ESRI World Topo Map` = leaflet::providers$Esri.WorldTopoMap,
  `ESRI World Imagery` = leaflet::providers$Esri.WorldImagery,
  `ESRI Imagery with labels` = 'http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}',
  `NatGeo World Map` = leaflet::providers$Esri.NatGeoWorldMap,
  `USA Topo Maps` = 'http://server.arcgisonline.com/ArcGIS/rest/services/USA_Topo_Maps/MapServer/tile/{z}/{y}/{x}',
  `Streets` = leaflet::providers$Esri.WorldStreetMap,
  `Canvas` = leaflet::providers$Esri.WorldGrayCanvas
)

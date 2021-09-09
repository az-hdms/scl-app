library(tidyverse)

db <- DBI::dbConnect(
  odbc::odbc(), 
  Driver = "ODBC Driver 17 for SQL Server", 
  Server = Sys.getenv("AZURE_SERVER"), 
  Database = Sys.getenv("AZURE_DATABASE"), 
  UID = Sys.getenv("AZURE_UID"), 
  PWD = Sys.getenv("AZURE_PWD"), 
  port = Sys.getenv("AZURE_PORT")
)

syn_list <- db %>% tbl("syn_list") %>% collect()
sname <- db %>% tbl("sname") %>% collect()
et <- db %>% tbl("et") %>% collect()

DBI::dbDisconnect(db)

basemaps <- c(
  `ESRI World Topo Map` = providers$Esri.WorldTopoMap,
  `ESRI World Imagery` = providers$Esri.WorldImagery,
  `ESRI Imagery with labels` = 'http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer/tile/{z}/{y}/{x}',
  `NatGeo World Map` = providers$Esri.NatGeoWorldMap,
  `USA Topo Maps` = 'http://server.arcgisonline.com/ArcGIS/rest/services/USA_Topo_Maps/MapServer/tile/{z}/{y}/{x}',
  `Streets` = providers$Esri.WorldStreetMap,
  `Canvas` = providers$Esri.WorldGrayCanvas
)
make_spatial <- function(df) {
  utm11nad83  <- '+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'
  utm11nad27  <- '+proj=utm +zone=11 +ellps=GRS80 +datum=NAD27 +units=m +no_defs'
  utm12nad83  <- '+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0'
  utm12nad27  <- '+proj=utm +zone=12 +ellps=GRS80 +datum=NAD27 +units=m +no_defs'
  
  sp_central <- '+init=epsg:26949'
  
  lonlatnad27 <- '+proj=longlat +datum=NAD27'
  lonlatwgs84 <- '+proj=longlat +datum=WGS84'
  lonlatwgs72 <- '+proj=longlat +datum=WGS72'
  
  if(nrow(df) == 0) {
    message('Input data frame has zero rows. Correct errors in input data.')
    return(st_sf(bind_cols(df, converted)))
  }
  
  df <- df %>% mutate(geometry = st_sfc(st_point()))
  
  df %>% filter(coord_type == 'UTM', str_detect(utm_zone, '12'), str_detect(datum, '(83)|(84)')) -> temp
  if(nrow(temp) > 0) {
    temp <- 
      temp %>%  
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(as.numeric(temp$loc_x), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = utm12nad83) %>% 
          st_cast(to = "POINT")
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'UTM', str_detect(utm_zone, '12'), str_detect(datum, '27')) -> temp
  if(nrow(temp) > 0) {
    temp <- 
      temp %>%  
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(as.numeric(temp$loc_x), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = utm12nad27) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'UTM', str_detect(utm_zone, '11'), str_detect(datum, '(83)|(84)')) -> temp
  if(nrow(temp) > 0) {
    temp <- 
      temp %>%  
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(as.numeric(temp$loc_x), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = utm11nad83) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'UTM', str_detect(utm_zone, '11'), str_detect(datum, '27')) -> temp
  if(nrow(temp) > 0) {
    temp <- 
      temp %>%  
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(as.numeric(temp$loc_x), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = utm11nad27) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DD', str_detect(datum, '(83)|(84)')) -> temp
  if(nrow(temp) > 0) {
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(-abs(as.numeric(temp$loc_x)), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = lonlatwgs84) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DD', str_detect(datum, '27')) -> temp
  if(nrow(temp) > 0) {
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(-abs(as.numeric(temp$loc_x)), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = lonlatnad27) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DD', str_detect(datum, '72')) -> temp
  if(nrow(temp) > 0) {
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(-abs(as.numeric(temp$loc_x)), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = 4322) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DDM', str_detect(datum, '(83)|(84)')) -> temp
  if(nrow(temp) > 0) {
    x <- str_split(temp$loc_x, " ")
    y <- str_split(temp$loc_y,  " ")
    x_val <- map_dbl(x, ~-(abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60)))
    y_val <- map_dbl(y, ~abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60))
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(x_val, y_val), dim = "XY") %>% 
          st_sfc(crs = lonlatwgs84) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DDM', str_detect(datum, '27')) -> temp
  if(nrow(temp) > 0) {
    x <- str_split(temp$loc_x, " ")
    y <- str_split(temp$loc_y,  " ")
    x_val <- map_dbl(x, ~-(abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60)))
    y_val <- map_dbl(y, ~abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60))
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(x_val, y_val), dim = "XY") %>% 
          st_sfc(crs = lonlatnad27) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DDM', str_detect(datum, '72')) -> temp
  if(nrow(temp) > 0) {
    x <- str_split(temp$loc_x, " ")
    y <- str_split(temp$loc_y,  " ")
    x_val <- map_dbl(x, ~-(abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60)))
    y_val <- map_dbl(y, ~abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60))
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(x_val, y_val), dim = "XY") %>% 
          st_sfc(crs = 4322) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DMS', str_detect(datum, '(83)|(84)')) -> temp
  if(nrow(temp) > 0) {
    x <- str_split(temp$loc_x, " ")
    y <- str_split(temp$loc_y,  " ")
    x_val <- map_dbl(x, ~-(abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60) + (as.numeric(.[3]) / 3600)))
    y_val <- map_dbl(y, ~abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60) + (as.numeric(.[3]) / 3600))
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(x_val, y_val), dim = "XY") %>% 
          st_sfc(crs = lonlatwgs84) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DMS', str_detect(datum, '27')) -> temp
  if(nrow(temp) > 0) {
    x <- str_split(temp$loc_x, " ")
    y <- str_split(temp$loc_y,  " ")
    x_val <- map_dbl(x, ~-(abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60) + (as.numeric(.[3]) / 3600)))
    y_val <- map_dbl(y, ~abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60) + (as.numeric(.[3]) / 3600))
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(x_val, y_val), dim = "XY") %>% 
          st_sfc(crs = lonlatnad27) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'DMS', str_detect(datum, '72')) -> temp
  if(nrow(temp) > 0) {
    x <- str_split(temp$loc_x, " ")
    y <- str_split(temp$loc_y,  " ")
    x_val <- map_dbl(x, ~-(abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60) + (as.numeric(.[3]) / 3600)))
    y_val <- map_dbl(y, ~abs(as.numeric(.[1])) + (as.numeric(.[2]) / 60) + (as.numeric(.[3]) / 3600))
    temp <-
      temp %>% 
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(x_val, y_val), dim = "XY") %>% 
          st_sfc(crs = 4322) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  df %>% filter(coord_type == 'SP') -> temp
  if(nrow(temp) > 0) {
    temp <- 
      temp %>%  
      select(id) %>%  
      mutate(
        geometry = 
          st_multipoint(cbind(as.numeric(temp$loc_x), as.numeric(temp$loc_y)), dim = "XY") %>% 
          st_sfc(crs = sp_central) %>% 
          st_cast(to = "POINT") %>% 
          st_transform(crs = utm12nad83)
      )
    df$geometry[df$id %in% temp$id] <- temp$geometry
  }
  
  st_sf(df, sf_column_name = "geometry", crs = utm12nad83)
}
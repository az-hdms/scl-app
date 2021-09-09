library(sf)
library(DBI)
library(glue)

a0 <- auth0::auth0_info()


options(scipen = 999)  # to make R not use scientific notation, which screws up finding number of digits
options(shiny.autoreload = TRUE)  # for development
options(shiny.port = 3808)

connect_azure_sql <- function() {
  DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = Sys.getenv("AZURE_SERVER"),
    Database = Sys.getenv("AZURE_DATABASE"),
    UID = Sys.getenv("AZURE_UID"),
    PWD = Sys.getenv("AZURE_PWD"),
    port = Sys.getenv("AZURE_PORT")
  )
}

st_read_mssql <- function(db, table, email = NULL) {
  col_info <- DBI::dbGetQuery(db, glue("exec sp_columns {table}"))
  non_geom = col_info$COLUMN_NAME[!col_info$TYPE_NAME %in% c("geometry", "geography")]
  geom = col_info$COLUMN_NAME[col_info$TYPE_NAME %in% c("geometry", "geography")]
  srid = DBI::dbGetQuery(db, glue("select top 1{geom}.STSrid as srid from {table}"))
  if(is.null(email)) {
    q = glue::glue("select {glue_collapse(non_geom, ', ')}, {geom}.STAsBinary() as {geom} from {table}")
  } else {
    q = glue::glue("select {glue_collapse(non_geom, ', ')}, {geom}.STAsBinary() as {geom} from {table} where user_email = '{email}'")
  }
  
  DBI::dbGetQuery(db, q) %>% st_as_sf(., crs = unique(srid$srid))
}

generate_binary <- function(x) {
  st_as_binary(x) %>% 
    as.character() %>% 
    glue_collapse() %>% 
    str_c("0x", .)
}


convert_sf_to_hex <- function(df) {
  geom_col <- attr(df, "sf_column")
  z <- ensym(geom_col)
  df %>% 
    as.data.frame() %>% 
    mutate(!!z := !!z %>% st_as_binary() %>% rawToHex())
}

ms_get_cols <- function(db, table) {
  dbGetQuery(db, glue("exec sp_columns {table}"))
}

sf_get_cols <- function(df) {
  geom_col <- attr(df, "sf_column")
}

st_write_mssql <- function(obj, db, table) {
  geom_col <- attr(obj, "sf_column")
  norm_col <- names(obj)[!names(obj) %in% geom_col] %>% glue_collapse(", ")
  temp_table = paste0("temp_", table)
  df <- convert_sf_to_hex(obj)
  suppressWarnings(
    odbc::dbWriteTable(db, temp_table, df, overwrite = TRUE)
  )   
  
  # Take temp table, convert WKB into MSSQL geometry, write real table
  query <- glue(
    "
    select * into {table} from
    (
      select {norm_col}, geom.{geom_col} as {geom_col} from {temp_table}
      left join (
        select po_id as id,
        geometry::STGeomFromWKB(convert(varbinary, {geom_col}, 2), {st_crs(obj)$epsg}) as {geom_col}
        from {temp_table}
      ) geom on po_id = geom.id
    ) as foo
    "
  )
  res <- odbc::dbSendQuery(db, query)
  odbc::dbClearResult(res)
  
  res <- odbc::dbSendQuery(db, glue("drop table {temp_table}"))
  odbc::dbClearResult(res)
}

filter_scl <- function(df, species_filter, year_filter) {
  no_species_filter = ("--Show All--" %in% species_filter | is.null(species_filter))
  no_year_filter = ("--Show All--" %in% year_filter | is.null(year_filter))
  if(no_species_filter & no_year_filter){
    return(df)
  }
  
  if(no_species_filter & !no_year_filter){
    return(
      df %>%
        mutate(year = lubridate::ymd(obs_date) %>% lubridate::year()) %>% 
        filter(
          year %in% year_filter
        )
    )
  }
  
  if(!no_species_filter & no_year_filter){
    return(
      df %>%
        mutate(year = lubridate::ymd(obs_date) %>% lubridate::year()) %>% 
        filter(
          species %in% species_filter,
        )
    )
  }
  
  if(!no_species_filter & !no_year_filter){
    return(
      df %>%
        mutate(year = lubridate::ymd(obs_date) %>% lubridate::year()) %>% 
        filter(
          species %in% species_filter,
          year %in% year_filter
        )
    )
  }
}

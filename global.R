library(sf)
library(DBI)
library(jsonlite)
library(glue)

a0 <- auth0::auth0_info()
walkthrough <- readr::read_file("./walkthrough.html")
faq <- readr::read_file("./faq.html")

options(scipen = 999)  # to make R not use scientific notation, which screws up finding number of digits
options(shiny.autoreload = TRUE)  # for development
options(shiny.port = 3808)

modules <- list.files(pattern = "^module")

purrr::walk(modules, source)

append_submitted_data <- function(obj, db, table, submitted_id) {
  # geom_col <- attr(obj, "sf_column")
  # norm_col <- names(obj)[!names(obj) %in% geom_col] %>% glue_collapse(", ")
  temp_table = paste0("temp_", table)
  obj <- obj %>% mutate(id = submitted_id)
  st_write(obj, db, temp_table, overwrite = T)
  
  query <- glue(
    '
    insert into {table} (submitted_id, primary_observer, scientific_name_used, common_name_used, num_observed, obs_date, obs_date2, county, loc_x, loc_y, utm_zone, datum, coord_type, loc_accuracy, lifestage, sex, disposition, specimen, marked, site_survey_id, habitat_notes, directions, general_notes, name_used, sci_or_com, com_to_sci, species, geometry) 
    select id, primary_observer, scientific_name_used, common_name_used, num_observed, obs_date, obs_date2, county, loc_x, loc_y, utm_zone, datum, coord_type, loc_accuracy, lifestage, sex, disposition, specimen, marked, site_survey_id, habitat_notes, directions, general_notes, name_used, sci_or_com, com_to_sci, species, geometry
    from {temp_table}
    '
  )
  
  res <- odbc::dbSendQuery(db, query)
  odbc::dbClearResult(res)
  
  dbRemoveTable(db, temp_table)
}

submit_data_to_db <- function(og_data = NULL, passing_data = NULL, ntr = FALSE, i, is_scl_holder, creds, admin_mode = FALSE, admin_user = NULL) {  # TODO - will have to go through this one carefully
  db <- hdms::connect_dashboard()
  selectbox_value <- ifelse(is.null(i$submit_context_scl), "", i$submit_context_scl)
  id_source = ifelse(
    is.null(i$submit_context_text),
    selectbox_value, 
    i$submit_context_text
  )
  is_scl_report = is_scl_holder & id_source != "I am submitting data not related to my Scientific Collecting License"
  make_json_data <- function(data) {
    is_geometry <- ifelse(
      is.null(attr(data, "sf_column")),
      FALSE,
      TRUE
    )
    data %>%
      as.data.frame() %>%
      {
        if(is_geometry) {
          mutate(., geometry = st_as_text(geometry))
        } else {
          .
        }
      } %>% 
      jsonlite::toJSON(auto_unbox = TRUE, na = "string")
  }
  if(!ntr) {
    og_data_as_json = make_json_data(og_data)
    passing_data_as_json = make_json_data(passing_data)
  } else {
    og_data_as_json = jsonlite::toJSON(list())
    passing_data_as_json = jsonlite::toJSON(list())
  }
  
  query <- glue_sql(
    '
    insert into submitted_from_app (original_data, passing_data, submitted_user, context_id_source, is_scl, nothing_to_report)
    values (
      {og_data_as_json},
      {passing_data_as_json},
      {creds$email},
      {ifelse(is.null(i$submit_context_text),i$submit_context_scl, i$submit_context_text)},
      {is_scl_report},
      {ntr}
    )
    RETURNING id
    '
    ,
    .con = db
  )
  
  submitted_id <- dbGetQuery(db, query)$id
  print(submitted_id)
  if(admin_mode) {
    query = glue("INSERT INTO admin_submit (submitted_id, admin_user) VALUES ({submitted_id}, '{admin_user}')")
    res = dbSendQuery(db, query)
    dbClearResult(res)
  }
  if(!ntr){
    append_submitted_data(passing_data, db, "submitted_data", submitted_id = submitted_id)
  }
  
  if(is_scl_report) {
    query = glue_sql(
      '
      update scl_licenses set submitted = true where "SCLicenseID" = {i$submit_context_scl} and "PrimaryEMAIL" = {creds$email}
      ', .con = db
    )
    res <- dbSendQuery(db, query)
    dbClearResult(res)
  }
  
  dbDisconnect(db)
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

get_user <- function(email) {  # TODO - change DB connection
  db <- hdms::connect_dashboard()
  user <- DBI::dbGetQuery(db, glue("select * from user_list where email = '{email}'"))
  licenses <- DBI::dbGetQuery(db, glue("select * from scl_licenses where \"PrimaryEMAIL\" = '{email}'"))
  if(nrow(licenses) > 0) {
    is_scl_holder = TRUE
    # OVERRIDE FOR NOW, NOT USING
    activity <- DBI::dbGetQuery(db, "select * from scl_activity where false")
    locality <- DBI::dbGetQuery(db, "select * from scl_locality where false")
    # activity <- DBI::dbGetQuery(db, glue_data_sql(licenses, "select * from scl_activity where \"SCLicenseID\" IN({ids*})", .con = db, ids = SCLicenseID))
    # locality <- DBI::dbGetQuery(db, glue_data_sql(activity, "select * from scl_locality where \"ActivityID\" IN({ids*})", .con = db, ids = ActivityID))
  } else {
    is_scl_holder = FALSE
    activity <- DBI::dbGetQuery(db, "select * from scl_activity where false")
    locality <- DBI::dbGetQuery(db, "select * from scl_locality where false")
  }
  DBI::dbDisconnect(db)
  return(
    list(
      user = user,
      licenses = licenses,
      activity = activity,
      locality = locality,
      is_scl_holder = is_scl_holder
    )
  )
}

library(rlang)
# TODO - do this on Postgres instead of loading in master_taxonomy as static file
get_top_suggestions <- function(data, search_column, search_term, num = 15, method = "osa") {
  data %>% 
    mutate(
      match_sci = stringdist::stringdist(tolower(search_term), tolower({{search_column}}), method = method),
      t = as_factor(taxonomy_system) %>% fct_relevel("subnational", "global", "higher")
      # match_com = stringdist("Macostiphus sp", common_name)
    ) %>% 
    arrange(match_sci, t) %>% 
    # arrange(match_sci, match_com) %>% 
    select(id, name, common_name, parent_name, synonym_of, t, match_sci, primary_key) %>% 
    slice(1:num)
}

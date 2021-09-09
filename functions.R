# functions

# 1. read in either CSV or Excel
#  - check for existence of all (core) fields
#  - read the data appropriately
library(readxl)

template_xref <- tribble(
  ~template_name,                                           ~display_name,       ~database_name,
  "Observer (collecting, handling and/or surveying)",       "Observer",          "primary_observer",
  "Scientific Name",                                        "Scientific Name",   "scientific_name_used",
  "Common Name",                                            "Common Name",       "common_name_used",
  "Count",                                                  "Count",             "num_observed",
  "Date\r\n(YYYY-MM-DD)",                                   "Date",              "obs_date",
  "optional - Date 2\r\n(YYYY-MM-DD)",                      "Date 2",            "obs_date2",
  "County",                                                 "County",            "county",
  "X coordinate - easting or longitude (e.g., -111.1234)",  "X Coordinate",      "loc_x",
  "Y coordinate - northing or latitude (e.g., 33.1234)",    "Y Coordinate",      "loc_y",
  "Zone",                                                   "Zone",              "utm_zone",
  "Datum",                                                  "Datum",             "datum",
  "Coorindate Type (DD, DMS, DDM, UTM, or SP)",             "Coord Type",        "coord_type",
  "Location uncertanity (if known)",                        "Loc. Uncertainty",  "loc_accuracy",
  "Lifestage",                                              "Lifestage",         "lifestage",
  "Sex",                                                    "Sex",               "sex",
  "Disposition",                                            "Disposition",       "disposition",
  "Museum",                                                 "Museum",            "specimen",
  "Marked",                                                 "Marked",            "marked",
  "Field Tag",                                              "Field Tag",         "site_survey_id",
  "Habitat Description",                                    "Habitat Desc.",     "habitat_notes",
  "Other locality data",                                    "Locality",          "directions",
  "Comments (reproductive status, behavior, etc.)",         "Comments",          "general_notes"
)

database_template_name <- c("obs_date2", "id_source", "primary_observer", "species", "name_used", "num_observed", "obs_date", "county", "loc_x", "loc_y", "utm_zone", "datum", "coord_type", "lifestage", "sex", "disposition", "specimen", "marked", "evidence_type", "habitat_notes", "directions", "general_notes", "mapping_notes", "trs", "elevation", "loc_accuracy", "site_survey_id", "source_id_field", "source_unique_id", "in_hdms", "eo_id")

check_which_template <- function(header_names) {
  is_scl_template <- all(template_xref$template_name %in% header_names)
  is_db_template <- all(template_xref$database_name %in% header_names)
  
  if(is_scl_template) {
    return("scl")
  } else if(is_db_template) {
    return("db")
  } else {
    all_names <- unique(c(template_xref$template_name, template_xref$database_name, database_template_name))
    bad_names <- header_names[!header_names %in% all_names]
    stop(glue::glue("{glue::glue_collapse(bad_names, sep=', ')} {ifelse(length(bad_names) == 1, 'is not a valid header name', 'are not valid header names')}, please retry or download new template"), call. = FALSE)
  }
}

process_template <- function(df) {
  df %>% 
    mutate(name_used = case_when(
      !is.na(scientific_name_used) ~ scientific_name_used,
      TRUE ~ common_name_used
    )) %>% 
    mutate(sci_or_com = case_when(
      !is.na(scientific_name_used) ~ "sci",
      !is.na(common_name_used) ~ "com",
      is.na(scientific_name_used) & is.na(common_name_used) ~ NA_character_  # Can't return NA, has to match the class of the rest
    )) %>% 
    mutate(species = name_used) %>% 
    mutate(utm_zone = 
             case_when(
               coord_type == "UTM" & is.na(utm_zone) ~ "",
               TRUE ~ utm_zone
             )
    )
}

scl_read_template <- function(file) {
  if(!grepl("\\.(csv)|(xlsx)$", file)){
    stop("File must be a csv or xlsx", call. = FALSE)
  }
  # which template are they giving?
  
  if(grepl("\\.csv$", file)) {
    # read csv
    header_names <- names(readr::read_csv(file, n_max = 0))
    template <- check_which_template(header_names)
    if(template == "db"){
      out <- readr::read_csv(
        file,
        col_types = cols(obs_date = 'c', obs_date2 = 'c', num_observed = 'c', loc_y = 'c', loc_x = 'c')
      )
    }
    if(template == "scl"){
      # read_scl as csv, an unlikely case
    }
  }
  
  if(grepl("\\.xlsx?$", file)) {
    header_names <- names(readxl::read_excel(file, sheet = 2, n_max = 0))
    template <- check_which_template(header_names)
    if(template == "db"){
      # unlikely that pod template is in xlsx
    }
    if(template == "scl"){
      out <- read_excel(file, col_types = 'text', sheet = 2)
      names(out) <-  template_xref$database_name
      out <- out %>% 
        mutate(name_used = case_when(
          !is.na(scientific_name_used) ~ scientific_name_used,
          TRUE ~ common_name_used
        )) %>% 
        mutate(sci_or_com = case_when(
          !is.na(scientific_name_used) ~ "sci",
          !is.na(common_name_used) ~ "com",
          is.na(scientific_name_used) & is.na(common_name_used) ~ NA_character_  # Can't return NA, has to match the class of the rest
        )) %>% 
        mutate(species = name_used) %>% 
        mutate(
          is_date_numeric = !is.na(as.numeric(obs_date)),
          is_date2_numeric = !is.na(as.numeric(obs_date2))
        ) %>% 
        mutate(
          obs_date = case_when(
            is_date_numeric == TRUE ~ as.character(janitor::excel_numeric_to_date(as.numeric(obs_date))),
            TRUE ~ obs_date
          ),
          obs_date2 = case_when(
            is_date2_numeric == TRUE ~ as.character(janitor::excel_numeric_to_date(as.numeric(obs_date2))),
            TRUE ~ obs_date2
          )
        ) %>% 
        mutate(
          obs_date = as.character(obs_date),
          obs_date2 = as.character(obs_date2)
        ) %>% 
        mutate(utm_zone = 
                 case_when(
                   coord_type == "UTM" & is.na(utm_zone) ~ "",
                   TRUE ~ utm_zone
                 )
        ) %>% 
        select(-c(is_date_numeric, is_date2_numeric))
    }
  }
  return(out)
}

# run all functions on each row -------------------------------------------
# source("error_checkers.R")
# file = file.xlsx
# out <- scl_read_template(file)

# exploded <- map(1:nrow(out), function(x) {
#   out[x, ]
# })

process_template <- function(df) {
  df %>% 
    mutate(name_used = case_when(
      !is.na(scientific_name_used) ~ scientific_name_used,
      TRUE ~ common_name_used
    )) %>% 
    mutate(sci_or_com = case_when(
      !is.na(scientific_name_used) ~ "sci",
      !is.na(common_name_used) ~ "com",
      is.na(scientific_name_used) & is.na(common_name_used) ~ NA_character_  # Can't return NA, has to match the class of the rest
    )) %>% 
    mutate(species = name_used) %>% 
    mutate(
      obs_date = janitor::excel_numeric_to_date(as.numeric(obs_date)),
      obs_date2 = janitor::excel_numeric_to_date(as.numeric(obs_date2))
    ) %>% 
    mutate(
      obs_date = as.character(obs_date),
      obs_date2 = as.character(obs_date2)
    ) %>% 
    mutate(utm_zone = 
             case_when(
               coord_type == "UTM" & is.na(utm_zone) ~ "",
               TRUE ~ utm_zone
             )
    )
}

generate_errors <- function(df) {
  tax_sci_provided <- map(df$scientific_name_used, check_taxonomy_provided)
  tax_exists   <- map2(df$name_used, df$sci_or_com, function(x, y) {
    if(!is.na(y)){
      check_taxonomy_exists(x, y)
    }
  })
  
  idx_needs_suggestions <- which(map_lgl(tax_exists, ~!is.null(.)))
  unique_suggestions    <- unique(df$name_used[idx_needs_suggestions])
  tax_suggest  <- map(unique_suggestions, suggest_taxonomy)
  
  tax_est <- map(df$name_used, check_taxonomy_in_est)
  tax_synonym  <- map(df$name_used, check_taxonomy_synonym)
  
  
  coord_provided_x <- map(df$loc_x, check_coordinates_provided, type = "x")
  coord_provided_y <- map(df$loc_y, check_coordinates_provided, type = "y")
  
  coord_type_exists <- map(df$coord_type, check_coord_type_provided)
  coord_type <- map(df$coord_type, function(x) {
    if(!is.na(x)){
      check_coord_type(x)
    }
  })
  coord_datum <- map(df$datum, check_valid_datum)
  coord_utm_zone <- map2(df$coord_type, df$utm_zone, function(x, y) {
    if(!is.na(x) & x == "UTM") {
      check_utm_zone(y)
    }
  })
  
  coord_zone_needed <- map2(df$coord_type, df$utm_zone, function(x, y) {
    if(!is.na(x) & x != "UTM") {
      check_utm_zone_needed(y)
    }
  })
  
  coord_utm_e_check <- map2(df$coord_type, df$loc_x, function(x, y) {
    if(!is.na(x) & x == "UTM") {
      if(!is.na(y)){
        check_utm_coords(y, 6, type = "easting")
      }
    }
  })
  
  coord_utm_n_check <-  map2(df$coord_type, df$loc_y, function(x, y) {
    if(!is.na(x) & x == "UTM") {
      if(!is.na(y)){
        check_utm_coords(y, 7, type = "northing")
      }
    }
  })

  coord_dd_lng_check <- map2(df$coord_type, df$loc_x, function(x, y) {
    if(!is.na(x) & x == "DD") {
      check_dd_coords(y, "lng")
    }
  })
  
  coord_dd_lat_check <- map2(df$coord_type, df$loc_y, function(x, y) {
    if(!is.na(x) & x == "DD") {
      check_dd_coords(y, "lat")
    }
  })
  
  coord_dms_lng_check <- map2(df$coord_type, df$loc_x, function(x, y) {
    if(!is.na(x) & x == "DMS") {
      check_dms_coords(y, "lng")
    }
  })
  
  coord_dms_lat_check <- map2(df$coord_type, df$loc_y, function(x, y) {
    if(!is.na(x) & x == "DMS") {
      check_dms_coords(y, "lat")
    }
  })
  
  coord_ddm_lng_check <- map2(df$coord_type, df$loc_x, function(x, y) {
    if(!is.na(x) & x == "DDM") {
      check_ddm_coords(y, "lng")
    }
  })
  
  coord_ddm_lat_check <- map2(df$coord_type, df$loc_y, function(x, y) {
    if(!is.na(x) & x == "DDM") {
      check_ddm_coords(y, "lat")
    }
  })
  
  obsdate_given <- map(df$obs_date, check_obs_date_given)
  
  # TODO janitor::excel_numeric_to_date call above will wipe out any invalid formats before it even gets here. Move that logic to functions.
  obsdate_valid <- map(df$obs_date, function(x) {
    if(!is.na(x)){
      check_obs_date(x, type="date1")
    }
  })
  
  obsdate2_valid <- map(df$obs_date2, function(x) {
    if(!is.na(x)){
      check_obs_date(x, type="date2")
    }
  })

  count_valid <- map(df$num_observed, check_count_given)
  

  
  a <- pmap(
    list(
      tax_sci_provided, tax_exists, tax_est,
      coord_provided_x, coord_provided_y, coord_type_exists, coord_type, coord_datum, coord_utm_zone, coord_zone_needed, coord_utm_e_check, coord_utm_n_check,
      coord_dd_lng_check, coord_dd_lat_check, coord_dms_lng_check, coord_dms_lat_check, coord_ddm_lng_check, coord_ddm_lat_check,
      obsdate_given, obsdate_valid, obsdate2_valid, count_valid
    ), 
    rbind
  )
 
  return(a) 
}

# formatStyle(
#   'id',
#   target = 'row',
#   backgroundColor = styleEqual(
#     err,
#     rep("#ffa3a3", length(err))
#   )
# )

highlight_cell <- function(table, column, error_value, color = "#ba0000", bg = "#ffa3a3") {
  formatStyle(
    table = table,
    column,
    target = "cell",
    fontWeight = styleEqual(
      error_value,
      rep("bold", length(error_value))
    ),
    color = styleEqual(
      error_value,
      rep(color, length(error_value))
    ),
    backgroundColor = styleEqual(
      error_value,
      rep(bg, length(error_value))
    )
  )
}

hof <- function(table, column, error_value, color = "#ba0000", bg = "#ffa3a3") {
  function(table) {
    formatStyle(
      table = table,
      column,
      target = "cell",
      fontWeight = styleEqual(
        error_value,
        rep("bold", length(error_value))
      ),
      color = styleEqual(
        error_value,
        rep(color, length(error_value))
      ),
      backgroundColor = styleEqual(
        error_value,
        rep(bg, length(error_value))
      )
    )
  }
}

# highlight_errors <- compose(
#   formatStyle()
# )




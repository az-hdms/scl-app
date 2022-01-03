# Taxonomy ----------------------------------------------------------------

return_error <- function(x, type, code, column, description, action) {
  tribble(
    ~type, ~code, ~value, ~column, ~description, ~action,
    type,   code,      x,  column,  description,  action
  )
}

return_clean <- function() {
  tribble(
    ~type, ~code, ~value, ~column, ~description, ~action
  )
}

check_taxonomy_provided <- function(x, y) {
  
  if(is.na(x) & is.na(y)) {
    return_error(
      x,
      type = "error",
      code = 100,
      column = "Scientific Name",
      description = "No taxonomy provided",
      action = "Please provide a value"
    )
  }
}


check_taxonomy_exists <- function(x, y) {
  if(y == "sci") {
    # if(!is.na(x) & !x %in% sname$SCIENTIFIC_NAME) {
    if(!is.na(x) & !x %in% mt_sci) {
      ret <- return_error(
        x,
        type = "taxonomy",
        code = 101,
        column = "Scientific Name",
        description = "Scientific name provided is not valid",
        action = "Please provide a valid scientific name"
      )
      return(ret)
    }
  }
  if(y == "com") {
    if(!is.na(x) & !x %in% et$COMMON_NAME) {
    # if(!is.na(x) & !x %in% mt$common_name) {
      ret <- return_error(
        x,
        type = "taxonomy",
        code = 101,
        column = "Common Name",
        description = "Common name provided is not valid",
        action = "Please provide a valid common name"
      )
      return(ret)
    } else if(length(et$NAME[which(x == et$COMMON_NAME)]) > 1) {
      matched <- et$NAME[!is.na(et$COMMON_NAME) & et$COMMON_NAME == x]
      ret <- return_error(
        x,
        type = "error",
        code = 104,
        column = "Common Name",
        description = glue("Common name provided matches more than one species: {glue_collapse(matched, sep = ', ')}"),
        action = "Please provide a scientific name instead"
      )
      return(ret)
    }
  }
}

check_taxonomy_in_est <- function(x) {
  if(!is.na(x) & !x %in% et$NAME) {
    # TODO: turned off internals fornow
    # return_error(
    #   x,
    #   type = "internal",
    #   code = 102,
    #   column = "Scientific Name",
    #   description = "Taxon value provided is not in EST",
    #   action = "Align with EST or make allow an exception"
    # )
  }
}

suggest_taxonomy <- function(x) {
  idx <- agrep(x, et$NAME)
  et$NAME[idx]
}

suggest_taxonomy_common <- function(x) {
  idx <- agrep(x, et$COMMON_NAME)
  et$COMMON_NAME[idx]
}

suggest_synonym <- function(x) {
  idx <- which(syn_list$SYNONYM == x)
  if(length(idx) > 0) {
    accepted_names <- syn_list$ACCEPTED_NAME[idx]
    return(accepted_names)
  }
}

suggest_synonym_common <- function(x) {
  matched <- et$NAME[!is.na(et$COMMON_NAME) & et$COMMON_NAME == x]
  return(matched)
}

check_taxonomy_synonym <- function(x) {
  # idx <- which(syn_list$SYNONYM == x)
  # if(length(idx) > 0){
  #   accepted_name <- syn_list$ACCEPTED_NAME[idx]
  #   # return(accepted_name)
  #   ret <- return_error(
  #     x,
  #     type = "warning",
  #     code = 103,
  #     column = "Scientific Name",
  #     description = glue("{x} is a synonym of: {glue_collapse(accepted_name, sep = ', ')}"),
  #     action = "Change to an accepted name"
  #   )
  #   return(ret)
  # }
}

# Coordinates -------------------------------------------------------------

check_coordinates_provided <- function(x, type = "x") {
  column = ifelse(type == "x", "X Coordinate", "Y Coordinate")
  if(is.na(x)) {
    return_error(
      x,
      type = "error",
      code = 200,
      column = column,
      description = glue("No {toupper(type)} coordinate provided"),
      action = "Provide coordinate values"
    )
  }
}


check_coord_type_provided <- function(x) {
  if(is.na(x)) {
    return_error(
      x,
      type = "error",
      code = 201,
      column = "Coord Type",
      description = glue("No coordinate type provided, must be one of: DD, DMS, DDM, UTM, SP"),
      action = "Provide coordinate type value"
    )
  }
}

check_coord_type <- function(x) {
  if(!x %in% c("DD", "DMS", "DDM", "UTM", "SP")) {
    return_error(
      x,
      type = "error",
      code = 202,
      column = "Coord Type",
      description = glue("No coordinate type provided, must be one of: DD, DMS, DDM, UTM, SP"),
      action = "Provide coordinate type value"
    )
  }
}

check_valid_datum <- function(x) {
  if(!toupper(x) %in% c('WGS84', 'NAD83', 'NAD27', "WGS72")){
    return_error(
      x,
      type = "error",
      code = 203,
      column = "Datum",
      description = "Invalid datum provided",
      action = "Datum must be one of WGS84, NAD83, NAD27, WGS72 (no spaces)"
    )
  }
}

check_utm_zone <- function(x) {
  if(is.na(x) | !grepl("(11)|(12)|(13)", x)){
    return_error(
      x,
      type = "error",
      code = 204,
      column = "Zone",
      description = "UTM Zone is invalid",
      action = "UTM Zone is missing or is does not contain one of the values: 11, 12, or 13"
    )
  }
}

check_utm_zone_needed <- function(x) {
  if(!is.na(x)){
    # TODO: i've turned off internal errors for now
    # return_error(
    #   x,
    #   type = "internal",
    #   code = 205,
    #   column = "Zone",
    #   description = "UTM Zone provided, but coordinate type is not defined as UTM",
    #   action = "UTM Zone will be ignored for this value"
    # )
  }
}


# UTM Specific ------------------------------------------------------------

check_utm_coords <- function(x, digits, type="easting") {
  column <- ifelse(type == "easting", "X Coordinate", "Y Coordinate")
  if(is.null(digits)){
    stop("digits must be defined", call. = FALSE)
  }
  if(is.na(suppressWarnings(as.numeric(x)))) {
    return(
      return_error(
        x,
        type = "error",
        code = 206,
        column = column,
        description = glue("{str_to_title(type)} could not be converted to number"),
        action = "Check the coordinate value"
      )
    )
  }
  num_digits <- nchar(round(as.numeric(x)))
  if(num_digits != digits) {
    return_error(
      x,
      type = "error",
      code = 207,
      column = column,
      description = glue("Expected {digits} digits for {type}, got {num_digits}"),
      action = "Check the coordinate value"
    )
  }
}


# Degree specific ---------------------------------------------------------

check_dd_coords <- function(x, type = NULL) {
  if(is.null(type) || !type %in% c("lng", "lat")) {
    stop("type must be defined as 'lng' or 'lat'", call. = FALSE)
  }
  column = ifelse(type == "lng", "X Coordinate", "Y Coordinate")
  value = suppressWarnings(as.numeric(x))
  if(is.na(value)) {
    return_error(
      x,
      type = "error",
      code = 206,
      column = column,
      description = "Coordinate could not be converted to number",
      action = "Check the coordinate value"
    )
  }
  if(type == "lng"){
    if(abs(value) < 100 & abs(value) <= 180) {
      return_error(
        x,
        type = "error",
        code = 208,
        column = "X Coordinate",
        description = "Longitude seems off, check if coordinates are reversed",
        action = "Check the coordinate value"
      )
    } else if(between(value, -180, 180) == FALSE) {
      return_error(
        x,
        type = "error",
        code = 209,
        column = "X Coordinate",
        description = "Longitude is not between -180 and 180, please check value",
        action = "Check the coordinate value"
      )
    }
  } else if (type == "lat") {
    if(abs(value) > 90 & abs(value) <= 180) {
      return_error(
        x,
        type = "error",
        code = 208,
        column = "Y Coordinate",
        description = "Latitude seems off, check if coordinates are reversed",
        action = "Check the coordinate value"
      )
    } else if(between(value, -90, 90) == FALSE) {
      return_error(
        x,
        type = "error",
        code = 209,
        column = "Y Coordinate",
        description = "Latitude is not between -90 and 90, please check value",
        action = "Check the coordinate value"
      )
    }
  }
}

check_dms_coords <- function(x, type = NULL) {
  if(is.null(type) || !type %in% c("lng", "lat")) {
    stop("type must be defined as 'lng' or 'lat'", call. = FALSE)
  }
  column = ifelse(type == "lng", "X Coordinate", "Y Coordinate")
  split_value <- str_split(x, " ") %>% unlist()
  if(length(split_value) != 3) {
    return_error(
      x,
      type = "error",
      code = 210,
      column = column,
      description = "DMS coordinate needs to be defined with 3 numbers, separated by a space",
      action = "Check the coordinate value"
    )
  }
  
  dms_parts <- map(split_value, ~suppressWarnings(as.numeric(.))) %>% unlist()
  check_na_numeric <- dms_parts %>% map_lgl(is.na)
  if(any(check_na_numeric) == TRUE) {
    return_error(
      x,
      type = "error",
      code = 211,
      column = column,
      description = "Some part of the coordinate could not be interpreted as a number, please check value. DMS coordinate needs to be defined with 3 numbers, separated by a space (no degree, minutes, or seconds symbols allowed)",
      action = "Check the coordinate value"
    )
  }
  
  if(any(dms_parts[2:3] > 60)) {
    return_error(
      x,
      type = "error",
      code = 212,
      column = column,
      description = "Minutes or Seconds value is greater than 60. Please check coordinate",
      action = "Check the coordinate value"
    )
  }
  
  calculated_value <- map_dbl(dms_parts, abs) %>% (function(x) {x[1] + (x[2] / 60) + (x[3] / 3600)})
  
  if(is.na(calculated_value)) {
    return(
      return_error(
        x,
        type = "error",
        code = 206,
        column = column,
        description = "Coordinate could not be converted to number",
        action = "Check the coordinate value"
      )
    )
  }
  if(type == "lng"){
    if(abs(calculated_value) < 100) {
      return_error(
        x,
        type = "error",
        code = 208,
        column = column,
        description = "Longitude seems off, check if coordinates are reversed",
        action = "Check the coordinate value"
      )
    } else if(between(calculated_value, -180, 180) == FALSE) {
      return_error(
        x,
        type = "error",
        code = 209,
        column = "X Coordinate",
        description = "Longitude is not between -180 and 180, please check value",
        action = "Check the coordinate value"
      )
    }
  } else if (type == "lat") {
    if(abs(calculated_value) > 90) {
      return_error(
        x,
        type = "error",
        code = 208,
        column = "Y Coordinate",
        description = "Latitude seems off, check if coordinates are reversed",
        action = "Check the coordinate value"
      )
    } else if(between(calculated_value, -90, 90) == FALSE) {
      return_error(
        x,
        type = "error",
        code = 209,
        column = "Y Coordinate",
        description = "Latitude is not between -90 and 90, please check value",
        action = "Check the coordinate value"
      )
    }
  }
}

check_ddm_coords <- function(x, type = NULL) {
  if(is.null(type) || !type %in% c("lng", "lat")) {
    stop("type must be defined as 'lng' or 'lat'", call. = FALSE)
  }
  column = ifelse(type == "lng", "X Coordinate", "Y Coordinate")
  split_value <- str_split(x, " ") %>% unlist()
  if(length(split_value) != 2) {
    return_error(
      x,
      type = "error",
      code = 213,
      column = column,
      description = "DDM coordinate needs to be defined with 2 numbers, separated by a space",
      action = "Check the coordinate value"
    )
  }
  
  ddm_parts <- map(split_value, ~suppressWarnings(as.numeric(.))) %>% unlist()
  check_na_numeric <- ddm_parts %>% map_lgl(is.na)
  if(any(check_na_numeric) == TRUE) {
    return_error(
      x,
      type = "error",
      code = 214,
      column = column,
      description = "Some part of the coordinate could not be interpreted as a number, please check value. DDM coordinate needs to be defined with 2 numbers, separated by a space (no degree, minutes, or seconds symbols allowed).",
      action = "Check the coordinate value"
    )
  }
  
  if(grepl("\\.", split_value[1])) {
    return_error(
      x,
      type = "error",
      code = 215,
      column = column,
      description = "Degrees number must be an integer, please check value",
      action = "Check the coordinate value"
    )
  }
  
  if(ddm_parts[2] > 60) {
    return_error(
      x,
      type = "error",
      code = 216,
      column = column,
      description = "Minutes value is greater than 60. Please check coordinate",
      action = "Check the coordinate value"
    )
  }
  
  calculated_value <- map_dbl(ddm_parts, abs) %>% (function(x) {x[1] + (x[2] / 60)})
  
  if(is.na(calculated_value)) {
    return_error(
      x,
      type = "error",
      code = 217,
      column = column,
      description = "Coordinate could not be converted to number, check value",
      action = "Check the coordinate value"
    )
  }
  
  if(type == "lng"){
    if(abs(calculated_value) < 100) {
      return_error(
        x,
        type = "error",
        code = 208,
        column = "X Coordinate",
        description = "Longitude seems off, check if coordinates are reversed",
        action = "Check the coordinate value"
      )
    } else if(between(calculated_value, -180, 180) == FALSE) {
      return_error(
        x,
        type = "error",
        code = 209,
        column = "X Coordinate",
        description = "Longitude is not between -180 and 180, please check value",
        action = "Check the coordinate value"
      )
    }
  } else if (type == "lat") {
    if(abs(calculated_value) > 90) {
      return_error(
        x,
        type = "error",
        code = 208,
        column = "Y Coordinate",
        description = "Latitude seems off, check if coordinates are reversed",
        action = "Check the coordinate value"
      )
    } else if(between(calculated_value, -90, 90) == FALSE) {
      return_error(
        x,
        type = "error",
        code = 209,
        column = "Y Coordinate",
        description = "Latitude is not between -90 and 90, please check value",
        action = "Check the coordinate value"
      )
    }
  }
}

sanitize_weird_encodings <- function(x) {
  stringi::stri_trans_general(x, "latin-ascii")
}

check_obs_date_given <- function(x) {
  if(is.na(x)){
    return_error(
      x,
      type = "error",
      code = 300,
      column = "Date",
      description = "Observation date not given.",
      action = "Please provide observation date in YYYY-MM-DD format"
    )
  }
}

check_obs_date <- function(x, type = "date1") {
  value <- suppressWarnings(lubridate::as_date(x))
  # print(is.na(value))
  column = ifelse(type == "date1", "Date", "Date 2")
  if(is.na(value)) {
    return_error(
      x,
      type = "error",
      code = 301,
      column = column,
      description = "Cannot be converted into a valid date, please check format. Should be YYYY-MM-DD",
      action = "Check the date value"
    )
  } else if(value > Sys.Date()) {
    return_error(
      x,
      type = "error",
      code = 302,
      column = column,
      description = "Date given is in the future, please check value",
      action = "Check the date value"
    )
  }
}

check_count_given <- function(x) {
  if(is.na(x) | x == "") {
    return_error(
      x,
      type = "error",
      code = 400,
      column = "Count",
      description = "Count not given",
      action = "Provide the number observed/collected"
    )
  }
}
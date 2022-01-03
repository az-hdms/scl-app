library(tidyverse)
library(glue)
library(httr)
library(jsonlite)


# Get token from Auth0 ----------------------------------------------------

get_auth0_token <- function() {
  token_endpoint <- "https://hdms-agfd.auth0.com/oauth/token"
  body = list(
    client_id = Sys.getenv("AUTH0_CLIENT_ID"),
    client_secret = Sys.getenv("AUTH0_CLIENT_SECRET"),
    audience = "https://hdms-agfd.auth0.com/api/v2/",
    grant_type = "client_credentials"
  )
  
  res <- POST(token_endpoint, body = body, encode = "json")
  return(content(res)[["access_token"]])
}


# Add a user to database and Auth0 ----------------------------------------

create_user <- function(email, first_name, last_name, pw = NULL) {
  args = list(email = email, first_name = first_name, last_name = last_name)
  
  # Make sure all arguments are characters
  not_strings = which(map_lgl(args, ~!is.character(.)))
  if(any(not_strings)) {
    stop(glue("{glue_collapse(names(args[not_strings]), sep = ', ')} must be character type"), call. = FALSE)
  }
  
  # Make sure email address is valid
  email_regex = "(^[a-zA-Z0-9_\\.\\+\\-]+@[a-zA-Z0-9\\-]+\\.[a-zA-Z0-9\\-\\.]+$)"
  if(!grepl(email_regex, email)) {
    stop(glue("\"{email}\" is not a valid email address."), call. = FALSE)
  }
  
  # Test database connection  
  ms <- try(connect_azure_sql(), silent = TRUE)
  if("try-error" %in% class(ms)) {
    stop("Could not connect to Azure database, make sure `connect_azure_sql()` is working", call. = FALSE)
  }
    
  # Define some helpers
  make_pw <- function() {
    stringi::stri_rand_strings(1, length = 20, pattern = "[A-Za-z0-9!@#\\$%\\^\\&\\*\\(\\)-\\+\\?]")
  }
  
  if(is.null(pw)) {
    pw = make_pw()
  }

  token = get_auth0_token()
  
  user_endpoint = "https://hdms-agfd.auth0.com/api/v2/users"
  
  body <- list(
    connection = "Username-Password-Authentication",
    email = email,
    password = pw,
    given_name = first_name,
    family_name = last_name,
    email_verified = TRUE
  )
  
  post_res = POST(
    user_endpoint, 
    add_headers(Authorization = paste0("Bearer ", token)), 
    body = body, 
    encode = "json"
  )
  
  # Make sure we get a 200-ish status code
  if(!grepl("^2", post_res$status_code)) {
    stop("Auth0 add user request was not successful", call. = FALSE)
  }
  
  user <- list(
    first_name = first_name,
    last_name = last_name,
    email = email,
    pw = safer::encrypt_string(pw, key = Sys.getenv("AZURE_SQL_SERVER_PW_KEY")),
    is_admin = FALSE,  # if you want to make an admin, manually adjust in the database!
    is_tester = FALSE
  )
  
  query <- glue_data_sql(user,
                         '
  insert into user_list(first_name, last_name, email, pw, is_admin, is_tester)
  values({first_name}, {last_name}, {email}, {pw}, {as.integer(is_admin)}, {as.integer(is_tester)})
  ', .con = ms
  )
  
  res <- DBI::dbSendQuery(ms, query)
  DBI::dbClearResult(res)
}



# Delete a user from the database and Auth0 -------------------------------


delete_user <- function(email) {
  token = get_auth0_token()
  user_search_endpoint = glue("https://hdms-agfd.auth0.com/api/v2/users?fields=email,user_id&include_fields=true&q=email:%22{email}%22&search_engine=v3")
  res <- GET(user_search_endpoint, add_headers(Authorization = paste0("Bearer ", token)))
  user_id = map_chr(content(res), "user_id")
  if(length(user_id) == 0) {
    stop(glue("Found no users with email = {email}"), call. = FALSE)
  }
  if(length(user_id) > 1) {
    stop(glue("Found more than one user with email = {email}"), call. = FALSE)
  }
  
  user_delete_endpoint = glue("https://hdms-agfd.auth0.com/api/v2/users/{user_id}")
  delete_res <- DELETE(user_delete_endpoint, add_headers(Authorization = paste0("Bearer ", token)))
  
  ms <- try(connect_azure_sql(), silent = TRUE)
  if("try-error" %in% class(ms)) {
    stop("Could not connect to Azure database, make sure `connect_azure_sql()` is working", call. = FALSE)
  }
  
  query <- glue_sql(
  '
  delete from user_list
  where email = {val*}
  ', .con=ms, val=email
  )
  
  res <- DBI::dbSendQuery(ms, query)
  DBI::dbClearResult(res)
}

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(glue)
library(DT)
library(auth0)

source("tables.R")
source("functions.R")
source("error_checkers.R")
source("make_spatial.R")
source("map.R")

# server <- function(input, output, session) {
server <- auth0_server(function(input, output, session) {
  current_layerId       = reactiveVal()
  upload_success        = reactiveVal(NULL)
  filter_list           = reactiveVal()
  year_list             = reactiveVal()
  current_scl           = reactiveVal()
  current_table         = reactiveVal()
  force_render          = reactiveVal()
  force_update          = reactiveVal()
  
  # GETS ASSIGNED ON MOVE
  is_moving             = reactiveVal(FALSE)
  move_layerId          = reactiveVal()
  move_id               = reactiveVal()
  move_data             = reactiveVal()
  move_other_data       = reactiveVal()
  move_popup            = reactiveVal()
  move_confirm_modal_ui = reactiveVal()
  move_confirm_function = reactiveVal()
  
  # SCL Holder Info
  scl_info = reactiveValues()
  is_scl_holder = reactiveVal()
  
  state = reactiveValues()
  admin_mode = reactiveVal(FALSE)
  # session_token = reactiveVal()

  # Make a manual trigger to refresh everything after submission
  trigger <- function() {
    rv <- reactiveValues(a = 0)
    list(
      depend = function() {
        rv$a
        invisible()
      },
      trigger = function() {
        rv$a <- isolate(rv$a + 1)
      }
    )
  }
  
  refresh_trigger <- trigger()

# Auth Flow: Auth0 -> MSSQL -----------------------------------------------

auth0_creds <- reactive({
  req(session$userData)
  list(
    email = session$userData$auth0_info$name,
    first = session$userData$auth0_info$given_name,
    last = session$userData$auth0_info$family_name
  )
})

db_creds <- reactive({
  req(auth0_creds)
  email <- auth0_creds() %>% pluck("email")
  isolate({
    user = get_user(email)
    scl_info$licenses = user$licenses
    scl_info$activity = user$activity
    scl_info$locality = user$locality
    scl_info$not_submitted = scl_info$licenses %>% filter(submitted == FALSE) %>% pull(SCLicenseID)
    is_scl_holder(user$is_scl_holder)
    state$user <- user$user
  })
  return(user$user)
})

observe({
  cli::cat_boxx("OUTPUT FROM DB_CREDS()")
  print(auth0_creds())
  print(db_creds())
  print(scl_info$licenses)
  print(scl_info$activity)
  print(scl_info$locality)
})
  
  
# Reactive values ---------------------------------------------------------

  observeEvent(db_creds(), {
    output$welcome <- renderUI({
      req(db_creds())
      data = db_creds()
      insertTab(
        "nav", 
        target = "Walkthrough",
        position = "after",
        session = session,
        tab = navbarMenu(
          HTML(
            glue(
              '
              <svg id="Layer_1" data-name="Layer 1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" 
                width=15 
                height=15 
                fill="#fff">
                <path d="M5,5A5,5,0,0,1,15,5V7A5,5,0,0,1,5,7ZM0,16.68a20,20,0,0,1,20,0V20H0Z"/>
              </svg>
              {data$first_name}
              '
            )
          ),
          # tagList(
          #   img(src="./zondicons/user.svg"),
          #   data$first_name
          # ),
          menuName = "user-menu",
          tabPanel(
            "My Info",
            # tableOutput("license_table"),
            module_user_info(db_creds(), isolate(scl_info$licenses), output_object = isolate(output)),
            uiOutput("export_pre_2019"),
            uiOutput("export_app_data")
            # module_export_data(scl_data = scl_data(), submitted_data_from_app = submitted_data_from_app())
          ),
          tabPanel(
            "Logout"
          )
        ) 
      )
      is_admin = db_creds()$is_admin
      if(is_admin) {
        module_admin_welcome_ui(db_creds())
      } else if(isolate(is_scl_holder())) {
        module_welcome_scl(db_creds(), isolate(scl_info$licenses))
      } else {
        module_welcome_non_scl(db_creds())
      }
      
      
      # if(nrow(data) > 0){
      #   tagList(
      #     h2(glue("Welcome {data$first_name} {data$last_name}")),
      #     # h4("You are submitting data for ", strong(data$scl_number), ". If this info is not correct, please contact Chrissy Kondrat-Smith."),
      #     p(style="font-size: 1.5rem; text-align: justify;", "Please follow the steps below to download the SCL report template and upload your data. After uploading, you will be able to visualize, perform quality control, and submit your data directly to AGFD."),
      #     p(style="font-size: 1.5rem; text-align: justify;", "A detailed walkthrough can be found on the 'Walkthrough' tab up at the top of the screen.")
      #   )
      # }
    })
  }, once = TRUE)

  observeEvent(input$user_select, {
    removeUI("#user-sim-bar")
    if(input$user_select != "--no user selected--") {
      insertUI(
        "#app", 
        where = "afterBegin",
        tagList(
          div(
            style = "background-color: #ffd663; padding: 1rem 1rem 1rem 2rem; margin: 0;",
            id = "user-sim-bar",
            glue("You are now simulating user: {input$user_select}")
          )
        )
      )
      
      # SET USER STATE TO SELECTED USER
      email = str_extract(input$user_select, "(?<=\\().*(?=\\))")
      user = get_user(email)
      
      scl_info$licenses = user$licenses
      scl_info$activity = user$activity
      scl_info$locality = user$locality
      scl_info$not_submitted = scl_info$licenses %>% filter(submitted == FALSE) %>% pull(SCLicenseID)
      is_scl_holder(user$is_scl_holder)
      state$user <- user$user
      admin_mode(TRUE)
    } else {
      cli::cat_boxx("in the ELSE block")
      if(state$user$email != db_creds()$email) {
        state$user = db_creds()
        
        user = get_user(db_creds()$email)
        
        scl_info$licenses = user$licenses
        scl_info$activity = user$activity
        scl_info$locality = user$locality
        scl_info$not_submitted = scl_info$licenses %>% filter(submitted == FALSE) %>% pull(SCLicenseID)
        is_scl_holder(user$is_scl_holder)
        # state$user <- user$user
        admin_mode(FALSE)
      }
    }
  })
  
  observeEvent(input$reset_user_select, {
    updateSelectInput(
      session = session, 
      inputId = "user_select", 
      selected = "--no user selected--"
    )
  })
  
  output$upload <- renderUI({
    req(state$user)
    data <- state$user
    removeUI("#spinner")
    if(nrow(data) > 0) {
      tagList(
        hr(),
        fluidRow(
          div(
            id="instruction-container",
            style="display: flex; flex-direction: column; flex-wrap: wrap; align-items: stretch; padding-left: 15px;",

            # First Item
            div(
              style="flex: 0 1 auto; margin-right: 2em; margin-bottom: 1em;",
              h3(style = "margin-top: 1rem;", "1. Download the template",
                 a(
                   href="./template/scl-report-template.xlsx",
                   target="_blank",
                   img(src="./zondicons/download.svg", width=25, height=25)
                 )
              )
            ),

            # Second Item
            div(
              style="flex: 0 1 auto; margin-right: 2em; margin-bottom: 1em;",
              h3(style = "margin-top: 0;", "2. Add your data to the template")
            ),

            # Third Item
            div(
              style="display: flex; flex-direction: row; align-items: flex-start;  margin-right: 2em;  margin-bottom: 1em;",
              h3(style = "margin-top: 0; margin-right: 0.25em", "3."),
              fileInput("file", label = NULL, placeholder = "Upload Your Data")
            )
          )
        )
      )
    } else {
      tagList(
        h2("Invalid token"),
        p("Please contact the AGFD SCL Coordinator")
      )
    }
  })
  
  output$submit_default <- renderUI({
    module_submit_default(scl_info$licenses, output)
  })
  
  output$submit_ui <- renderUI({
    req(passing_spatial())
    if(is_scl_holder()) {
      module_submit_scl(passing_spatial(), scl_info$licenses, output)
    } else {
      module_submit_non_scl(passing_spatial(), output)
    }
  })
  
  scl_data <- reactive({
    req(state$user)
    data <- state$user
    db <- hdms::connect_dashboard()
    sup_emails <- dbGetQuery(db, glue("select granted_email from supplementary_access where email = '{data$email}'"))
    emails = c(data$email, sup_emails$granted_email)
    data_sources <- dbGetQuery(
      db,
      glue_sql("select * from user_data_sources where email IN ({emails*})", .con = db, emails = emails)
    )
    if(nrow(data_sources) > 0) {
      out <- st_read(db, query = glue_sql("select * from pod_clean_full where id_source IN ({sources*})", sources = data_sources$id_source, .con = db))
    } else {
      out <- st_read(db, query = "select * from pod_clean_full where false")
    }
    filter_list(
      unique(out$species)
    )
    year_list(
      unique(out$obs_date %>% lubridate::ymd() %>% lubridate::year())
    )
    current_scl(out)
    dbDisconnect(db)
    return(out)
  })
  
  submitted_data_from_app <- reactive({
    req(state$user)
    creds <- state$user
    db <- hdms::connect_dashboard()
    ids <- dbGetQuery(db, glue("select * from submitted_from_app where submitted_user = '{creds$email}'"))
    if(nrow(ids) == 0) {
      return(
        dbGetQuery(db, glue_sql("select * from submitted_data where submitted_id IN({vals*})", vals = -999, .con = db))
      )
    }
    data_query <- glue_sql("select * from submitted_data where submitted_id IN({vals*})", vals = ids$id, .con = db)
    data <- st_read(db, query = data_query) %>% left_join(ids %>% select(-c(original_data, passing_data)), by = c("submitted_id" = "id"))
    filter_list(
      unique(c(filter_list(), data$species))
    )
    years <- unique(data$obs_date %>% lubridate::ymd() %>% lubridate::year())
    year_list(
      unique(c(year_list(), years))
    )
    dbDisconnect(db)
    return(data)
  })
  
  

# Export UI and Handlers --------------------------------------------------

  ## RENDER UI IN USER PROFILE AREA
  
  output$export_pre_2019 <- renderUI({
    req(scl_data())
    module_export_data_pre_2019(scl_data())
  })
  
  output$export_app_data <- renderUI({
    req(submitted_data_from_app())
    module_export_app_data(submitted_data_from_app())
  })
  
  ## DOWNLOAD BUTTON HANDLERS FOR EXPORTING
  
  
  ## PRE 2019 DATA
  ## -------------
  
  output$btn_export_pre_2019_data_csv <- downloadHandler(
    filename = function() {
      glue("data_export_{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.csv")
    },
    content = function(file) {
      out <- scl_data() %>% 
        filter(id_source %in% input$pre_2019_data_select) %>% 
        mutate(
          coords_x = st_coordinates(st_transform(., 4326))[,1],
          coords_y = st_coordinates(st_transform(., 4326))[,2]
        ) %>% 
        st_drop_geometry() %>% 
        select(-c(in_hdms, eo_id, created_user:last_edited_date))
      
      out <- out %>% 
        map_df(
          function(x) {
            if(!is.character(x)) {
              return(x)
            } else {
              x[is.na(x)] = ""
              return(x)
            }
          }
        )
      write_csv(out, file)
    }
  )
  
  output$btn_export_pre_2019_data_gpkg <- downloadHandler(
    filename = function() {
      glue("data_export_{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.gpkg")
    },
    content = function(file) {
      out <- scl_data() %>% 
        filter(id_source %in% input$pre_2019_data_select) %>% 
        select(-c(in_hdms, eo_id, created_user:email))
      st_write(out, file, layer = "data_export")
    }
  )
  
  
  
  ## DATA SUBMITTED VIA THE APP
  ## --------------------------
  
  output$btn_export_submitted_data_csv <- downloadHandler(
    filename = function() {
      glue("data_export_{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.csv")
    },
    
    content = function(file) {
      out <- submitted_data_from_app() %>% 
        filter(context_id_source %in% input$submitted_data_from_app_select) %>% 
        mutate(
          coords_x = st_coordinates(st_transform(., 4326))[,1],
          coords_y = st_coordinates(st_transform(., 4326))[,2]
        ) %>% 
        st_drop_geometry() %>% 
        select(-c(name_used:com_to_sci, submitted_user:files))
      out <- out %>% 
        map_df(
          function(x) {
            if(!is.character(x)) {
              return(x)
            } else {
              x[is.na(x)] = ""
              return(x)
            }
          }
        )
      
      write_csv(out, file)
    }
  )
  
  output$btn_export_submitted_data_gpkg <- downloadHandler(
    filename = function() {
      glue("data_export_{format(Sys.time(), '%Y-%m-%d-%H%M%S')}.gpkg")
    },
    content = function(file) {
      out <- submitted_data_from_app() %>% 
        filter(context_id_source %in% input$submitted_data_from_app_select) %>% 
        select(-c(name_used:com_to_sci, submitted_user:files))
      st_write(out, file, layer = "data_export")
    }
  )
  

# Upload Success ----------------------------------------------------------

  
  observeEvent(upload_success(), {
    removeUI("#submit_default")
    cli::cat_boxx("upload_success observer fired")
    # removeUI("#upload_feedback")
    print(upload_success())
    
    if(upload_success() == TRUE) {
      print("in the TRUE block")
      output$upload_feedback <- renderUI({
        tagList(
          div(
            style = "diplay: flex; flex-direction: row;",
            img(
              style = "float: left; margin-right: 10px;",
              src="./zondicons/checkmark-outline-green.svg", height = 40, width = 40
            ),
            h2("Upload Successful!"),
            p("Please proceed to the 'Review Data' section.")
          )
        )
      })
    }
    
    if(upload_success() == FALSE) {
      print("in the FALSE block")
      output$upload_feedback <- renderUI({
        tagList(
          div(
            style = "diplay: flex; flex-direction: row;",
            img(
              style = "float: left; margin-right: 10px;",
              src="./zondicons/close-solid-red.svg", height = 40, width = 40
            ),
            h2("Upload Failed"),
            p("Please make sure you are using the official template provided in Step 1. "),
            p("The template was updated October 18, 2021. If you used a template file downloaded before this date, please download the official template from step 1 and transfer your data to the new template file.")
          )
        )
      })
    }
  })
  # 
  # output$upload_feedback <- renderUI({
  #   print("render upload_feedback fired!")
  #   # req(upload_success())
  #   if(is.null(upload_success())){
  #     return()
  #   }
  #   
  #   
  # })
  
  # observeEvent(input$file, {
  #   output$upload_loading <- renderUI({
  #     tagList(
  #       p("Loading data to server...")
  #     )
  #   })
  # })

  submitted_data <- reactive({
    req(input$file)
    removeUI("#upload_success")
    leafletProxy("map") %>% 
      # clearGroup("Past SCL Data") %>% 
      clearGroup("move") %>% 
      setView(-111.5, 34, 6)
    t <- try(
      out <- scl_read_template(input$file$datapath)
    )
    print(t)
    cli::cat_rule("upload_success")
    upload_success(!"try-error" %in% class(t))
    print(upload_success())
    removeUI("#upload_loading")
    if(upload_success() == FALSE){
      return(NULL)
    } else {
      if(admin_mode()){
        out_filename <- glue("{tolower(state$user$first_name)}_{tolower(state$user$last_name)}_admin_mode_{floor(as.numeric(Sys.time()))}.xlsx")
      } else {
        out_filename <- glue("{tolower(state$user$first_name)}_{tolower(state$user$last_name)}_{floor(as.numeric(Sys.time()))}.xlsx")
      }
      file.copy(input$file$datapath, file.path("./cache", out_filename))
      out %>% mutate(id = 1:nrow(.)) %>% select(id, everything())
    }
  })
  
  observeEvent(submitted_data(), {
    state$data = submitted_data()
  })
  
  # TODO: state$data needs to be re-run through `scl_read_template` to get all of the error checking, and the extra mutated columns!!
  
  # submitted_errors <- eventReactive(state$data, {
  #   print("submitted_errors() triggered")
  #   out <- generate_errors(state$data)
  #   print(out)
  #   if(length(out) == 0) {
  #     tribble(
  #       ~type, ~code, ~value, ~column, ~description, ~action
  #     )
  #   } else {
  #     out
  #   }
  # })
  
  observeEvent(state$data, {
    print("state$data observer triggered")
    # cli::cat_boxx("state$data")
    # print(state$data)
    
    state$error_list <- generate_errors(state$data)
    # print(state$error_list)
    has_errors <- any(map_lgl(state$error_list, ~!is.null(.)))
    if(has_errors) {
      out <- map2(1:length(state$error_list), state$error_list, function(x, y) {
        if(!is.null(y)){
          y <- y %>% mutate(row = x + 1) %>% select(row, everything())
        }
      })
    } else {
      out <- tribble(
        ~type, ~code, ~value, ~column, ~description, ~action
      )
    }
  
    state$error <- out %>% map_df(~.) %>% filter(type != "internal")
    # print(state$error)
    state$error_grouped_taxonomy <- 
      state$error %>% 
      count(column, value) %>% 
      filter(column %in% c("Scientific Name", "Common Name"), !is.na(value)) %>% 
      mutate(
        suggestions = case_when(
          column == "Scientific Name" ~ map(
            value, ~get_top_suggestions(mt, name, ., 50) %>%  # TODO
              filter(!duplicated(name)) %>% 
              mutate(display_name = paste0(name, " (", common_name, ")"))
          ),
          column == "Common Name" ~ map(
            value, ~get_top_suggestions(mt, common_name, ., 50) %>%  # TODO
              filter(!duplicated(name)) %>% 
              mutate(display_name = paste0(name, " (", common_name, ")"))
          )
        )
      )
    
    state$error_grouped_coordinates <- state$error %>% count(column, value) %>% filter(grepl("Coordinate", column))
    
    statement = length(state$error_grouped_taxonomy$value) > 0
    choices = switch(
      statement + 1,
      "--all taxonomy fixed--",
      state$error_grouped_taxonomy$value
    )
    selected = switch(
      statement + 1,
      "--all taxonomy fixed--",
      NULL
    )
    updateSelectInput(session, inputId = "badTaxonomySelect", choices = choices, selected = selected)    
    
    state$hl$scientific_name <- state$error %>% filter(column == "Scientific Name") %>% distinct(value) %>% pull(value)
    state$hl$common_name <- state$error %>% filter(column == "Common Name") %>% distinct(value) %>% pull(value)
    state$hl$hl_sci <- state$data %>% transmute(x = as.logical(match(scientific_name_used , state$hl$scientific_name)) & sci_or_com == "sci") %>% pull(x)
    state$hl$hl_com <- state$data %>% transmute(x = as.logical(match(common_name_used , state$hl$common_name)) & sci_or_com == "com") %>% pull(x)
    state$hl$hl_sci[is.na(state$hl$hl_sci)] = 0
    state$hl$hl_com[is.na(state$hl$hl_com)] = 0
    state$hl$num_observed <- state$error %>% filter(column == "Count") %>% distinct(value) %>% pull(value)
    state$hl$obs_date <- state$error %>% filter(column == "Date") %>% distinct(value) %>% pull(value)
    state$hl$obs_date2 <- state$error %>% filter(column == "Date 2") %>% distinct(value) %>% pull(value)
    state$hl$loc_x <- state$error %>% filter(column == "X Coordinate") %>% distinct(value) %>% pull(value)
    state$hl$loc_y <- state$error %>% filter(column == "Y Coordinate") %>% distinct(value) %>% pull(value)
    state$hl$utm_zone <- state$error %>% filter(column == "Zone") %>% distinct(value) %>% pull(value)
    state$hl$datum <- state$error %>% filter(column == "Datum") %>% distinct(value) %>% pull(value)
    state$hl$coord_type <- state$error %>% filter(column == "Coord Type") %>% distinct(value) %>% pull(value)
    # This wonderful abstraction/higher-order-function doesn't work on Shiny Server for some reason 
    # ¯\_(ツ)_/¯
    #
    # state$error_higlight_fxs <- map2(state$error_grouped$column, state$error_grouped$data, function(x, y) {
    #     hof(
    #       column = x,
    #       error_value = pull(y, value)
    #     )
    # })
  })
  
  observeEvent(c(input$badTaxonomySelect, force_update()), {
    print("@#$%^&*(*&^%$#@$%^&*(@#$%^&*(*&^%$#@$%^&*(@#$%^&*(*&^%$#@$%^&*(")
    print(input$badTaxonomySelect)
    if(is.null(input$badTaxonomySelect) | input$badTaxonomySelect %in% c("--all taxonomy fixed--", "") ) {
      shinyjs::disable("bulkEdit")
      updateSelectInput(
        session, "suggestedTaxonomySelect",
        choices = ifelse(input$badTaxonomySelect == "--all taxonomy fixed--", "--all taxonomy fixed--", ""),
        selected = ifelse(input$badTaxonomySelect == "--all taxonomy fixed--", "--all taxonomy fixed--", ""),
      )
    } else {
      shinyjs::enable("bulkEdit")
      updateSelectInput(
        session, "suggestedTaxonomySelect", 
        choices = state$error_grouped_taxonomy %>% 
          filter(value == input$badTaxonomySelect) %>% 
          pull(suggestions) %>%
          `[[`(1) %>% 
          pull(display_name)
      )
    }
  })
  
  observeEvent(input$bulkEdit, {
    req(input$badTaxonomySelect, input$suggestedTaxonomySelect)
    shinyjs::disable("bulkEdit")
    current_lookup = state$error_grouped_taxonomy %>% filter(value == input$badTaxonomySelect)
    current_suggestions = current_lookup$suggestions[[1]]

      if(current_lookup$column == "Scientific Name") {
      replacement = current_suggestions$name[current_suggestions$display_name == input$suggestedTaxonomySelect]
      idx = which(state$data$scientific_name_used == input$badTaxonomySelect)
      state$data$scientific_name_used[idx] = replacement
      state$data$name_used[idx] = replacement
      state$data$species[idx] = replacement
    }
    
    if(current_lookup$column == "Common Name") {
      replacement = current_suggestions$name[current_suggestions$display_name == input$suggestedTaxonomySelect]
      idx = which(state$data$common_name_used == input$badTaxonomySelect)
      state$data$scientific_name_used[idx] = replacement
      state$data$sci_or_com[idx] = "sci"
      state$data$common_name_used[idx] = NA
      state$data$name_used[idx] = replacement
      state$data$species[idx] = replacement
    }
    force_render(runif(1))
  })
  
  errors_idx <- eventReactive(state$error_list, {
    if(length(state$error_list) == 0){
      return(integer(0))
    } else {
      which(map_lgl(state$error_list, ~!is.null(.)))
    }
  })
  
  passing <- reactive({
    req(state$data)
    print(glue("Error IDX length :{length(errors_idx())}"))
    if(length(errors_idx()) == 0){
      state$data
    } else {
      state$data[-errors_idx(), ]
    }
  })
  
  passing_spatial <- reactive({
    req(passing())
    data = passing()
    filter_list(
      unique(c(filter_list(), data$name_used))
    )
    if(nrow(passing()) != 0){
      make_spatial(passing())
    }
  })
  
  not_passing <- reactive({
    req(state$data)
    state$data %>% slice(errors_idx())
  })
  

# Species Selection to filter map -----------------------------------------
  
  observeEvent(filter_list(), {
    updateSelectInput(
      session, "species", 
      choices = c("--Show All--", sort(filter_list()))
    )
  })
  
  observeEvent(input$species, {
    req(passing_spatial())
    if(!is_moving()){
      data <- passing_spatial()
      if(!is.null(input$species) & !"--Show All--" %in% input$species){
        data <- data %>% filter(name_used %in% input$species)  
      }
      leafletProxy("map") %>% clearGroup("move")
      pod_render_map(data, snap = F)
    }
  })
 
  

# Year Selection to Filter Past SCL ---------------------------------------

  observeEvent(year_list(), {
    updateSelectInput(
      session, "year", 
      choices = c("--Show All--", sort(year_list()))
    )
  })
  
  observeEvent(c(input$species, input$year), {
    scl <- filter_scl(scl_data(), input$species, input$year)
    prev <- filter_scl(submitted_data_from_app(), input$species, input$year)
    
    scl_render_map(scl)
    prev_render_map(prev)
    
  })
  
  # observeEvent(input$year, {
  #   req(current_scl())
  #   if(!is_moving()){
  #     scl <- current_scl()
  #     if(!is.null(input$year) & !"--Show All--" %in% input$year){
  #       scl <- scl %>% mutate(year = lubridate::ymd(obs_date) %>% lubridate::year())
  #       scl <- scl %>% filter(year %in% input$year)
  #       current_scl(scl)
  #     } else {
  #       current_scl()
  #     }
  #     leafletProxy("map") %>% clearGroup("move")
  #     scl_render_map(current_scl())
  #   }
  # })
  # 
  
# Render UI and Tables ----------------------------------------------------

  
  output$overview <- renderUI({
    req(passing(), not_passing())
    num_submitted <- nrow(state$data)
    num_passing <- nrow(passing())
    num_not_passing <- nrow(not_passing())
    tagList(
      h4(glue("You submitted {num_submitted} {ifelse(num_submitted == 1, 'record', 'records')}")),
      h4(glue("There {ifelse(num_not_passing == 1, 'is', 'are')} {num_not_passing} {ifelse(num_not_passing == 1, 'record', 'records')} with errors")),
      p("Errors mean that one or more fields have a significant issue and need to be corrected before submitting. You can find out specific details by clicking the 'Quality Control' tab."),
      if(num_not_passing == 0) {
        p("You have no errors, if you are ready to submit, go to the Submit tab at the top")
      },
      hr()
    )
  })
  
  output$past_scl_overview <- renderUI({
    req(nrow(scl_data()) > 0)
    data <- scl_data()
    counts <- data %>%
      as.data.frame() %>% 
      mutate(year = lubridate::ymd(obs_date) %>% lubridate::year()) %>% 
      count(year) %>% 
      arrange(year)
    list_output <- glue("<li><strong>{counts$year}</strong>: {counts$n} {ifelse(counts$n == 1, 'record', 'records')}</li>")
    HTML(
      glue(
        "
        <h4>You have previously submitted SCL reports for the following years</h4>
        <ul>
          {glue_collapse(list_output, sep='\n')}
        </ul>
        <hr/>
        "
      )
    )
    # tagList(
    #   h4("You have previously submitted data for the following years:"),
    #   htmltools::htmlPreserve("<h1> HI </h1>"),
    #   tagAppendChildren(p(), list = glue("{counts$year}: {counts$n}")),
    #   hr()
    # )
  })
  
  output$past_submitted_data_overview <- renderUI({
    req(nrow(submitted_data_from_app()) > 0)
    data <- submitted_data_from_app()
    counts <- data %>% 
      as.data.frame() %>%
      count(time = submitted_date %>% lubridate::with_tz("America/Phoenix"), context = context_id_source)
    list_output <- glue("<li><strong>{counts$time}</strong> <strong>{counts$context}</strong>: {counts$n} {ifelse(counts$n == 1, 'record', 'records')}</li>")
    HTML(
      glue(
        "
        <h4>You have previously submitted these records through the app</h4>
        <ul>
          {glue_collapse(list_output, sep='\n')}
        </ul>
        <hr/>
        "
      )
    )
  })
  
  
  observeEvent(state$data, {
    print("state$data has fired")
    output$table <- renderDT({
      err <- errors_idx()
      if(length(err) == 0) {
        err = NA
      }
      num <- length(err)
      print(err)
      
      # This is crazy rlang magic, unfortunately doesn't work on Shiny Server
      # fx <- map(state$error_higlight_fxs, as_mapper)
      # cp <- compose(!!!fx)
      
      highlight_error <- function(table, err, name) {
        if(length(err) > 0){
          formatStyle(
            table,
            name,
            target = "cell",
            fontWeight = styleEqual(
              err,
              rep("bold", length(err))
            ),
            color = styleEqual(
              err,
              rep("#ba0000", length(err))
            ),
            backgroundColor = styleEqual(
              err,
              rep("#ffa3a3", length(err))
            )
          )
        } else {
          table
        }
      }
      
      # highlight_tax <- function(table, err, name, value_col) {
      #   if(length(err) > 0){
      #     formatStyle(
      #       table,
      #       name,
      #       target = "cell",
      #       fontWeight = styleEqual(
      #         err,
      #         rep("bold", length(err))
      #       ),
      #       color = styleEqual(
      #         err,
      #         rep("#ba0000", length(err))
      #       ),
      #       backgroundColor = styleEqual(
      #         err,
      #         rep("#ffa3a3", length(err))
      #       )
      #     )
      #   } else {
      #     table
      #   }
      # }
      
      out_table <- state$data[, c("id", template_xref$database_name)]
      names(out_table) <- c("id", template_xref$display_name)
      out_table <- out_table %>% mutate(hl_sci = state$hl$hl_sci, hl_com = state$hl$hl_com)
      if(input$toggleOnlyErrors) {
        out_table <- out_table[err, ]
      }
      current_table(out_table)
      datatable(
        out_table,
        editable = TRUE,
        selection = "none",
        # extensions = c('Responsive')
        extensions = 'Buttons', 
        options = list(dom = 'Bfrtip', buttons = I('colvis'), 
          columnDefs = list(
            list(targets = 23, visible = FALSE),
            list(targets = 24, visible = FALSE)
          )
        ),
        rownames = F,
      ) %>% 
        formatStyle(
          'id',
          target = 'row',
          backgroundColor = styleEqual(
            err,
            rep("#edd3d4", length(err))
          )
        ) %>% 
        formatStyle(
          'Scientific Name', valueColumns = "hl_sci",
          target = "cell",
          fontWeight = styleEqual(
            1,
            "bold"
          ),
          color = styleEqual(
            1,
            "#ba0000"
          ),
          backgroundColor = styleEqual(
            1,
            "#ffa3a3"
          )
        ) %>%
        formatStyle(
          'Common Name', valueColumns = "hl_com",
          target = "cell",
          fontWeight = styleEqual(
            1,
            "bold"
          ),
          color = styleEqual(
            1,
            "#ba0000"
          ),
          backgroundColor = styleEqual(
            1,
            "#ffa3a3"
          )
        ) %>%
        # highlight_error(err = state$hl$scientific_name, name = "Scientific Name") %>% 
        highlight_error(err = state$hl$num_observed, name = "Count") %>% 
        highlight_error(err = state$hl$obs_date, name = "Date") %>% 
        highlight_error(err = state$hl$obs_date2, name = "Date 2") %>% 
        highlight_error(err = state$hl$loc_x, name = "X Coordinate") %>% 
        highlight_error(err = state$hl$loc_y, name = "Y Coordinate") %>% 
        highlight_error(err = state$hl$utm_zone, name = "Zone") %>% 
        highlight_error(err = state$hl$datum, name = "Datum") %>%
        highlight_error(err = state$hl$coord_type, name = "Coord Type")

        
      # TODO: Zone is currently colored red everywhere b/c it's NA for non-UTM coords - use `valueColumns` to limit to where CoordType == "UTM"
    })
  })
  
  output$errorTable <- renderDT({
    datatable(
      state$error
    )
  })
  

# Debugging Button --------------------------------------------------------

  
  observeEvent(input$debug, {
    cli::cat_rule("Uploaded Data")
    print(state$data)
    
    # cli::cat_rule("Submitted Errors")
    # print(submitted_errors())
    
    cli::cat_rule("state$data")
    print(state$data)
    
    cli::cat_rule("state$error")
    print(state$error)
    
    cli::cat_rule("Errors IDX")
    print(errors_idx())
    cli::cat_rule("Passing")
    print(passing())
    cli::cat_rule("Not Passing")
    print(not_passing())
    cli::cat_rule("Passing - SPATIAL")
    print(passing_spatial())
    cli::cat_rule("input$species")
    print(input$species)
  })
  

# Edit Table Logic --------------------------------------------------------

  
  observeEvent(input$table_cell_edit, {
    print("EDIT FIRED")
    row = input$table_cell_edit$row
    col = input$table_cell_edit$col
    val = input$table_cell_edit$value
    id = current_table()[row, ]$id
    col_name = template_xref$database_name[template_xref$display_name == names(current_table())[col + 1]]
    state$data[state$data$id == id, col_name] <<- val
    state$data <- state$data %>% 
      mutate(name_used = case_when(
        !is.na(scientific_name_used) ~ scientific_name_used,
        TRUE ~ common_name_used
      )) %>% 
      mutate(sci_or_com = case_when(
        !is.na(scientific_name_used) ~ "sci",
        !is.na(common_name_used) ~ "com",
        is.na(scientific_name_used) & is.na(common_name_used) ~ NA_character_  # Can't return NA, has to match the class of the rest
      )) %>% 
      mutate(
        com_to_sci = map(name_used, common_to_scientific, et = et) %>% 
          map_chr(function(x) {
            if(length(x) > 1) {
              "error_multiple"
            } else if(length(x) == 1) {
              x[1]
            } else {
              NA_character_
            }
          }
          )
      ) %>% 
      mutate(species = case_when(
        sci_or_com == "sci" ~ name_used,
        sci_or_com == "com" ~ com_to_sci,
        TRUE ~ NA_character_
      )) %>% 
      mutate(utm_zone = 
               case_when(
                 coord_type == "UTM" & is.na(utm_zone) ~ "",
                 TRUE ~ utm_zone
               )
      )
    force_render(runif(1))
  })
  
  

# Map Setup ---------------------------------------------------------------

  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      setView(-111.5, 34, 6) %>% 
      addLayersControl(
        overlayGroups = c("Uploaded Data", "Prev. Submitted", "Past SCL Data"), 
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$basemap, {
    showModal(modalDialog(
      title = "Choose Basemap",
      radioButtons('basemaps_select', label = 'CHOICES',
                   choices=basemaps,
                   selected = character(0)
      )
    ))
    
    observeEvent(input$basemaps_select, {
      if(input$basemaps_select == basemaps[5]){
        leafletProxy('map') %>% 
          clearTiles() %>% 
          addTiles(input$basemaps_select, options = tileOptions(maxZoom = 15))
      } else if (input$basemaps_select == basemaps[3]){
        leafletProxy('map') %>% 
          clearTiles() %>% 
          addProviderTiles(providers$Esri.WorldImagery) %>%
          addTiles(input$basemaps_select)
      } else {
        leafletProxy('map') %>% 
          clearTiles() %>%
          addProviderTiles(input$basemaps_select)
      }
    })
  })
  

# Re-render Map -----------------------------------------------------------
  observeEvent(input$nav, {
    req(input$nav == "Logout")
    auth0::logout()
  })
  
  observeEvent(input$nav, {
    req(input$nav == "Review Data", scl_data())
    leafletProxy("map") %>% clearGroup("Past SCL Data") %>% clearGroup("Prev. Submitted")
    scl_render_map(scl_data())
    prev_render_map(submitted_data_from_app())
  })
  
  observeEvent(input$nav, {
    
    req(input$nav == "Review Data", passing_spatial())
    leafletProxy("map") %>% clearGroup("move")  # %>% clearGroup("Past SCL Data")
    # scl_render_map(scl_data())
    pod_render_map(passing_spatial())
    
    # data <- passing_spatial() %>% st_transform(4326)
    # b <- st_bbox(data)
    # leafletProxy("map") %>% 
    #   clearMarkers() %>% 
    #   addMarkers(
    #     data = data,
    #     popup = ~do.call(pod_popup, args = list(data = data)) 
    #   ) %>% 
    #   fitBounds(b[[1]] - 1,  b[[2]] - 1, b[[3]] + 1, b[[4]] + 1)  # had to use double-brackets to get rid of names
  })  
  
  observeEvent(force_render(), {
    # req(passing_spatial(), scl_data())
    leafletProxy("map") %>% clearGroup("move")  # %>% clearGroup("Past SCL Data")
    # scl_render_map(scl_data())
    if(!is.null(passing_spatial())) {
      pod_render_map(passing_spatial(), snap = FALSE)
    }
    
    # data <- passing_spatial() %>% st_transform(4326)
    # b <- st_bbox(data)
    # leafletProxy("map") %>% 
    #   clearMarkers() %>% 
    #   addMarkers(
    #     data = data,
    #     popup = ~do.call(pod_popup, args = list(data = data)) 
    #   ) %>% 
    #   fitBounds(b[[1]] - 1,  b[[2]] - 1, b[[3]] + 1, b[[4]] + 1)  # had to use double-brackets to get rid of names
    shinyjs::enable("bulkEdit")
  })  
  
  # Map Observers -----------------------------------------------------------
  
  
  observeEvent(input$map_marker_click, {
    marker <- input$map_marker_click
    
    if(is_moving() == FALSE){
      current_layerId(marker$id)
      
      # current_annotation_ui(
      #   capture_annotation_group(marker$id, "_annotation_ui") %>% eval()
      # )
      # current_annotation_function(
      #   capture_annotation_group(marker$id, "_annotate_db") %>% eval()
      # )
    }
    
  })

  # TODO - DELETE THIS  
  # observeEvent(input$show_scl, {
  #   if(input$show_scl){
  #     scl_render_map(scl_data())
  #   } else {
  #     leafletProxy("map") %>% clearGroup("scl")
  #   }
  # })
  
  

  # Move Observers ----------------------------------------------------------
  
  observeEvent(is_moving(), {
    if(is_moving()){
      disable(id = "species")
    } else {
      enable(id = "species")
      updateSelectInput(session, "species", selected = "")
    }
  })
  
  observeEvent(input$move, {
    cli::cat_boxx("MOVE EVENT")
    is_moving(TRUE)     
    move_id(current_layerId())
    # move_popup(move_group() %>% str_c("_move_popup") %>% sym() %>% eval())  # Gives moving options in popup
    move_confirm_modal_ui(
      # move_group() %>% str_c("_move_confirm_modal_ui") %>% sym() %>% eval()
      pod_move_confirm_modal_ui
    )  # modal dialog to gather annotation info
    move_confirm_function(
      # move_group() %>% str_c("_move_confirm_function") %>% sym() %>% eval()
      pod_move_confirm_function
    )  # function to executre the annotation info and change geometry
    
    leafletProxy("map") %>% clearGroup("Uploaded Data")
    
    data = passing_spatial()
    
    move_data(
      data %>% filter(id == move_id())
    )
    
    move_other_data(
      data %>% filter(id != move_id())
    )
    
    # print(move_data())
    add_non_move_marker(data = move_other_data(), "id")
    add_move_marker(move_data(), move_popup = pod_move_popup, id_field = "id")
  })
  
  observeEvent(input$move_confirm_popup, {
    showModal(modalDialog(
      title = "Finalize move?",
      # XXX_move_modal_content
      p("The coordinates for this observation will be changed to where you've dragged the marker. The submitted location will be changed from the original coordinates to decimal degrees."),
      footer = tagList(
        actionButton("move_confirm", "Confirm"),
        actionButton("move_cancel", "Cancel")
      )
    ))
  })
  
  observeEvent(input$move_confirm, {
    session$sendCustomMessage("remove-popup", 'hello')
    # XXX_function to annotate the data
    do.call(move_confirm_function(), args = list(input = input))
    
    cli::cat_boxx("FILTERED RESULT")
    coords <- c(input$map_marker_click$lng, input$map_marker_click$lat)
    
    print(coords)
    new = move_data() %>%
      mutate(
        loc_x = round(coords[1], 5),
        loc_y = round(coords[2], 5),
        utm_zone = NA,
        datum = "WGS84",
        coord_type = "DD"
      )
    
    print(new)
    
    state$data[state$data$id == move_id(), ] <- new
    
    # leafletProxy("map") %>% clearGroup("move")
    # force_render(runif(1))
    
    is_moving(FALSE)
    # data_list()[[current_group()]] <- data_list()[[current_group()]]
    removeModal()
    force_render(runif(1))
    print("MOVE CONFIRM")
  })
  
  observeEvent(input$move_cancel, {
    session$sendCustomMessage("remove-popup", 'hello')
    removeModal()
    is_moving(FALSE)
    force_render(runif(1))
    # points symbolized back to normal
    print("MOVE CANCEL")
  })
  
  observeEvent(input$session, {
    browser()
  })
  
  observeEvent(input$submit_context_scl, {
    if(input$submit_context_scl == "I am submitting data not related to my Scientific Collecting License") {
      disable("nothing_to_report")
      output$ui_if_scl_holder_submits_general <- renderUI({
        textInput("submit_context_text", "Brief description of the data")
      })
    } else {
      enable("nothing_to_report")
      output$ui_if_scl_holder_submits_general <- renderUI(tagList())
    }
  })
  
  
  observeEvent(input$button_nothing_to_report, {
    showModal(
      modalDialog(
        title = "Are you sure?", 
        p(glue("You will be submitting 'Nothing to report' for your permit: {input$submit_context_scl}")),
        footer = tagList(
          actionButton("confirm_ntr", "Confirm"),
          actionButton("cancel_ntr", "Cancel")
        ),
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$confirm_ntr, {
    creds = db_creds()
    if(creds$is_admin && admin_mode()) {
      submit_data_to_db(NULL, NULL, ntr = TRUE, i = input, is_scl_holder = is_scl_holder(), creds =  state$user,  admin_mode = TRUE, admin_user = creds$email)
    } else {
      submit_data_to_db(NULL, NULL, ntr = TRUE, i = input, is_scl_holder = is_scl_holder(), creds =  state$user)
    }
    showModal(
      modalDialog(
        title = "Success", 
        p(glue("You submitted 'Nothing to report' to {input$submit_context_scl}. Please click OK to confirm, and you will be sent back to the home screen")),
        tags$a(tags$h1("OK"), href = "#", onclick = "location.replace(location.pathname)"),
        footer = NULL,
        easyClose = FALSE
      )
    )
  })
  
  observeEvent(input$cancel_ntr, {
    removeModal()
  })
  
  observeEvent(input$submit_button, {
    print("User pressed the 'Submit Button'")
    creds = db_creds()
    if(creds$is_admin && admin_mode()) {
      ## IS AN ADMIN SUBMITTING ON BEHALF OF SOMEONE ELSE?
      # TODO - this entire section
      if(is_scl_holder()){
        submit_data_to_db(state$data, passing_spatial(), ntr = input$nothing_to_report, i = input, is_scl_holder = is_scl_holder(), creds = state$user, admin_mode = TRUE, admin_user = creds$email)
      } else {
        submit_data_to_db(state$data, passing_spatial(), ntr = FALSE, i = input, is_scl_holder = is_scl_holder(), creds = state$user, admin_mode = TRUE, admin_user = creds$email)
      }
    } else {
      # NORMAL SUBMISSION
      if(is_scl_holder()){
        submit_data_to_db(state$data, passing_spatial(), ntr = input$nothing_to_report, i = input, is_scl_holder = is_scl_holder(), creds = state$user)
      } else {
        submit_data_to_db(state$data, passing_spatial(), ntr = FALSE, i = input, is_scl_holder = is_scl_holder(), creds = state$user)
        # TODO - end section
      }
    }
      showModal(
        modalDialog(
          title = "Success", 
          p("Your data was submitted successfully. Please click OK to confirm, and you will be sent back to the home screen"),
          tags$a(tags$h1("OK"), href = "#", onclick = "location.replace(location.pathname)"),
          footer = NULL,
          easyClose = FALSE
        )
      )
    # refresh_trigger$trigger()
  })
# End of Server -----------------------------------------------------------
# }
}, info = a0)
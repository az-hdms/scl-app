library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(shinyjs)
library(auth0)

# ui <-
ui <- auth0_ui(
  fluidPage(
  id="app",
  useShinyjs(),
  theme = shinytheme("flatly"),
  # Workaround to get new token when user hits refresh on the page - https://github.com/curso-r/auth0/issues/54
  # tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');},2000);")),
  tags$style(
    "
    #toggleOnlyErrors {
      width: 30px;
      height: 30px;
    }
    .checkbox label{
      display: flex;
    }
    .checkbox label span {
      margin-left: 3rem;
      font-size: 2.2rem;
    }
    "
  ),
  navbarPage(
    id="nav",
    title = "AGFD SCL Report Submission Tool",

# Upload Tab --------------------------------------------------------------

    tabPanel(
      "Upload",
      div(
        id = "spinner", 
        style = "display: flex; justify-content: center; align-items: center;",
        div(
          style = "margin-top: 35vh",
          img(src="./img/spinner.gif")
        )
        
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("welcome")
        )
      ),
      uiOutput("upload"),
      uiOutput("upload_loading"),
      uiOutput("upload_feedback")
    ),


# Review Data Tab ---------------------------------------------------------


    tabPanel(
      "Review Data",
      tabsetPanel(
        id="current_tab",
        tabPanel(
          "Overview",
          fluidRow(
            column(
              width = 5,
                actionButton("debug", "Debug"),
                actionButton("session", "Session"),
                h3("Map View"),
                p("The map shows all observations that are error-free. You can click on individual points to get the data associated with that point."),
                hr(),
                uiOutput("overview"),
                uiOutput("past_scl_overview"),
                uiOutput("past_submitted_data_overview"),
                h4(strong("See a point that's not in the correct location?")),
                p("You can manually move points to the correct location by clicking on the marker, clicking 'Move', and dragging the marker to the correct location. Typos in coordinates can also by modified in the 'Quality Control' and 'View All Records' areas."),
                p("Click the 'Basemaps' button to switch to a different basemap, and can filter which species you want to be visible on the map.")
            ),
            column(
              width = 7,
              fluidRow(
                style = "display: flex; flex-wrap: wrap; align-items: center; justify-content: flex-start;",
                div(
                  # style = "margin-right: 10px; margin-bottom: 5px",
                  actionButton('basemap', 'Basemaps', icon=icon('globe', 'fa-lg'), class = "btn-sm",
                               style="color: #000; background-color: #fff;")
                  # actionButton('library', 'Map Lib', icon=icon('book'),
                  #              style="color: #000; background-color: #fff;"),
                  # actionButton('coordinate', 'Add Observation', icon=icon('map-pin'),
                  #              style="color: #000; background-color: #fff;")
                ),
                div(
                  style = "margin-top: 0px;",
                  selectInput("species", label = "Filter by Species", multiple = T, choices = NULL, selectize = T)
                ),
                div(
                  selectInput("year", label = "Filter by Year", multiple = T, choices = NULL, selectize = TRUE)
                )
                # TODO DELETE THIS
                # ,
                # div(
                #   checkboxInput("show_scl", "Show past SCL Data", value = TRUE)
                # )
              ),
              fluidRow(
                leafletOutput("map", height = 600)
              )
            )
          )
        ),
        tabPanel(
          "Quality Control",
          h2("Taxonomy"),
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "margin-right: 2rem;",
              selectInput("badTaxonomySelect", label = "Taxonomy not found", choices = NULL)
            ),
            div(
              style = "margin-right: 2rem;",
              selectInput("suggestedTaxonomySelect", label = "Suggested names", choices = NULL)
            ),
            actionButton("bulkEdit", "Change all")
          ),
          h2("Coordinates/Others"),
          DTOutput("errorTable")
        ),
        tabPanel(
          "View All Records",
          h2("Table of all records"),
          p("This table displays all records submitted through the template."),
          tags$ul(
            tags$li(
              style="margin-top: 2px;", "Records with errors will be highlighted in ", 
              span(style = "background-color: #edd3d4; padding: 2px;", "red")," with the specific fields highlighted in ", 
              span(style = "background-color: #ffa3a3; color: #ba0000; padding: 2px;", strong("darker red."))
            )
          ),
          h3("Modfiying values"),
          p("You can always correct data in the Excel template and resubmit. However, you can also modify the value of a field directly on this table by double-clicking the value you wish to change."),
          p("Please note that you must scroll to the right to see the entire contents of the table."),
          checkboxInput("toggleOnlyErrors", "Show only errors?", value = FALSE),
          DT::dataTableOutput("table")
        )
      )
    ),
    tabPanel(
      "Submit Data",
      uiOutput("submit_default"),
      uiOutput("submit_ui")
    ),
    tabPanel(
      "FAQ",
      div(
        style = "max-width: 80rem;",
        HTML(faq)
      )
    ),
    tabPanel(
      "Walkthrough",
      div(
        style = "max-width: 80rem;",
        HTML(walkthrough)
      )
    )
  ),
  tags$script("
    Shiny.addCustomMessageHandler('remove-popup', function(message) {
      //console.log(message)
      const popup = document.querySelectorAll('.leaflet-popup-close-button')
      console.log(popup)
      popup.forEach(function(p) {
        p.click()
      })
    });
    
    //Shiny.addCustomMessageHandler('disable-select', function(message) {
    //  
    //})
    
    const a = document.querySelector('#upload')
    const b = document.querySelector('#upload_loading')
    a.addEventListener('change', function(e) {
      b.innerText = 'Loading data to server...'
    })
  ")
# )
), info = a0)
# This is where an acutal Shiny module might come in handy, since the UI is the same
# I would just need different namespaces...but the database calls are different so there would be different server logic ¯\_(ツ)_/¯
module_export_data_pre_2019 <- function(scl_data) {
  print(scl_data)
  return(
    tagList(
      if(nrow(scl_data) > 0) {
        tagList(
          h3(strong("Export Pre-2019 Data")),
          
          div(
            style = "display: flex;",
            div(
              selectInput(
                "pre_2019_data_select",
                label = NULL,
                choices = unique(scl_data$id_source),
                multiple = TRUE
              )
            ),
            
            div(
              style = "padding-left: 1rem;",
              downloadButton("btn_export_pre_2019_data_csv", label = "Export CSV")
            ),
            
            div(
              style = "padding-left: 1rem;",
              downloadButton("btn_export_pre_2019_data_gpkg", label = "Export GPKG")
            )
          )
        )
      }
    )
  )
}

module_export_app_data <- function(submitted_data_from_app) {
  return(
    tagList(
      if(nrow(submitted_data_from_app) > 0) {
        tagList(
          h3(strong("Export Data submitted via this application")),
          div(
            style = "display: flex;",
            
            div(
              selectInput(
                "submitted_data_from_app_select",
                label = NULL,
                choices = unique(submitted_data_from_app$context_id_source),
                multiple = TRUE
              )
            ),
            
            div(
              style = "padding-left: 1rem;",
              downloadButton("btn_export_submitted_data_csv", label = "Export CSV")
            ),
            
            div(
              style = "padding-left: 1rem;",
              downloadButton("btn_export_submitted_data_gpkg", label = "Export GPKG")
            )
          )
        )
      }
    )
  )
}
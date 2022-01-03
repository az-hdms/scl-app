module_submit_scl <- function(passed, license_data, output_object) {
  removeUI("#button_nothing_to_report")
  choices <- license_data %>% filter(submitted == FALSE) %>% pull(SCLicenseID)
  choices <- c(choices, "I am submitting data not related to my Scientific Collecting License")
  if(is.null(passed) | nrow(passed) == 0) {
    tagList(
      h2("You have no data to submit, or no currently uploaded data is valid")
    )
  } else {
    tagList(
      h2(glue("You are about to submit {nrow(passed)} rows")),
      output_object$submit_context <- renderUI({
        if(length(choices) > 0) {
          tagList(
            selectInput("submit_context_scl", label = "Select a license or general submission", choices = choices),
            checkboxInput("nothing_to_report", label = "Nothing to report", value = FALSE),
            uiOutput("ui_if_scl_holder_submits_general"),
            actionButton("submit_button", "Submit Data")
          )
        } else {
          h3("All SCL reports have been submitted")
        }
      })
    )
  }
}


module_submit_non_scl <- function(passed, output_object) {
  if(is.null(passed) | nrow(passed) == 0) {
    tagList(
      h2("You have no data to submit, or no currently uploaded data is valid")
    )
  } else {
    tagList(
      h2(glue("You are about to submit {nrow(passed)} rows")),
      output_object$submit_context <- renderUI({
        textInput("submit_context_text", label = "Description of data")
      }),
      actionButton("submit_button", "Submit Data")
    )
  }
}

module_submit_default <- function(license_data, output_object) {
  if(nrow(license_data) > 0) {
      choices <- license_data %>% filter(submitted == FALSE) %>% pull(SCLicenseID)
      tagList(
          output_object$submit_context <- renderUI({
            if(length(choices) > 0) {
              tagList(
                h2("You haven't uploaded any data yet."),
                p("If you have nothing to report, please select the SCL ID number and mark 'Nothing to report' below"),
                selectInput("submit_context_scl", label = "Which SCL?", choices = choices),
                actionButton("button_nothing_to_report", label = "Nothing to report")
                # actionButton("submit_button", "Submit Data")
              )
            } else {
              h3("All SCL reports have been submitted")
            }
          })
        )
  } else {
    tagList(
      h2("You haven't uploaded any data."),
      p("Please return to the 'Upload' tab")
    )
  }
}
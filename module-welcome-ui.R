p_style = "font-size: 1.5rem; text-align: justify;"

module_welcome_scl <- function(user_data, license_data) {
  return(
    tagList(
      h2(glue("Welcome {user_data$first_name} {user_data$last_name}")),
      if(any(license_data$submitted == FALSE)){
        reports_due = license_data %>% filter(!submitted) %>% select(SCLicenseID, LicensedYear) %>% arrange(LicensedYear)
        reports_due_text <- glue_data(reports_due, "{SCLicenseID} ({LicensedYear})") %>% glue_collapse(sep = ", ")
        tagList(
          h3(glue("The following reports are due: {reports_due_text}")),
          p(style = p_style, "Please follow the steps below to download the SCL report template and upload your data. After uploading, you will be able to visualize, perform quality control, and submit your data directly to AGFD."),
          p(style = p_style, "A detailed walkthrough can be found on the 'Walkthrough' tab up at the top of the screen.")
        )
      } else {
        tagList(
          p("All SCL reports have been submitted, thank you. You are welcome to submit other observations directly to the Natural Heritage Program (HDMS)"),
          p("If you would like to resubmit data please contact the AGFD Permits Biologist.")
        )
      }
    )
  )
}

module_welcome_non_scl <- function(user_data) {
  return(
    tagList(
      h2(glue("Welcome {user_data$first_name} {user_data$last_name}")),
      p("You can submit data directly to the HDMS using the template below.")
    )
  )
}
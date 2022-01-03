module_user_info <- function(data, licenses, activity = NULL, locality = NULL, output_object) {
  
  return(
    tagList(
      h2("User Info"),
      p(glue("Name: {data$first_name} {data$last_name}")),
      p(glue("Email: {data$email}")),
      if(data$is_admin) p(strong("You are an administrator")),
      if(data$is_tester) p(strong("You are a tester")),
      if(nrow(licenses) > 0) {
        tagList(
          h2("Licenses"),
          output_object$license_table <- renderTable({
            licenses %>% select(SCLicenseID, LicensedYear, submitted) %>% arrange(LicensedYear)
          }, striped = TRUE)
        )
      },
      if(data$is_admin) {
        db <- hdms::connect_dashboard()
        outstanding <- dbGetQuery(db, "select first_name, last_name, email, \"SCLicenseID\", \"LicensedYear\", submitted from user_list left join scl_licenses on email = \"PrimaryEMAIL\" where submitted = false order by last_name")
        dbDisconnect(db)
        tagList(
          h2("Outstanding Licenses"),
          output_object$outstanding_licenses <- renderTable({
            outstanding
          }, striped = TRUE)
        )
      }
    )
  )
}
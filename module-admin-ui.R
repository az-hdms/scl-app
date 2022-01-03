module_admin_welcome_ui <- function(creds) {
  db <- hdms::connect_dashboard()
  user_list <- dbGetQuery(db, "select * from user_list")
  dbDisconnect(db)
  user_list <- user_list %>% 
    mutate(display_name = str_c(first_name, " ", last_name, " (", email, ")", sep = ""))
  return(
    tagList(
      h2(glue("Welcome {creds$first_name}")),
      div(
        style = "display: flex;",
        div(
          style = "padding-right: 2rem;",
          selectInput(
            "user_select", 
            "Select User", 
            choices = c("--no user selected--", user_list$display_name),
            width = "400px"
          )
        ),
        div(
          style = "padding-right: 2rem; margin-top: 1.5rem",
          actionButton("reset_user_select", "Reset")
        )
      )
    )
  )
}
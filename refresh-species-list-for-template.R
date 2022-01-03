# 1. programmatically refreshed the species list in the XLSX file

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)

db <- hdms::connect_dashboard()

template <- loadWorkbook("www/template/scl-report-template.xlsx")

mt <- db %>% tbl("master_taxonomy") %>% collect()

# Get subnational taxonomy, then get parents (up to family)
sub <- mt %>% filter(taxonomy_system == "subnational")
genus <- mt %>% filter(id %in% unique(sub$parent_id))
family <- mt %>% filter(id %in% unique(genus$parent_id))

# Merge lists and remove duplicates
taxa_list <- bind_rows(sub, genus, family) %>% 
  select(primary_key, name, common_name, synonym_of) %>% 
  filter(!duplicated(name)) %>% 
  arrange(name)

# Turn NA into blank for Excel so you don't get literal "NA"
taxa_list <- taxa_list %>% map_df(function(x) {
  if(is.character(x)) {
    x[is.na(x)] = ''
    return(x)
  } else {
    return(x)
  }
})

# Replace the "Species List" worksheet
openxlsx::removeWorksheet(template, "Species List")
openxlsx::addWorksheet(template, "Species List")
openxlsx::writeDataTable(template, "Species List", x = taxa_list, withFilter = FALSE)
visible_sheets <- rep(TRUE, length(names(template)))
visible_sheets[names(template) == "Drop-downs"] = FALSE
sheetVisibility(template) <- visible_sheets

# Run this will create a new template, but it deletes the actual report sheet since the species sheet and report sheet are linked, it's annoying
## The work around is to just paste the new species sheet into the original scl-report-template.xlsx   

openxlsx::saveWorkbook(template, "www/template/scl-report-template1-new.xlsx", overwrite = TRUE)





##### DO NOT RUN THIS CODE BELOW YET NOT FINISHED IN DEVELOPMENT ------------------------------------------------------------------------

# MANUAL CHECK (write out to clipboard)
# taxa_list %>% write.table('clipboard-16384', row.names = FALSE)

# 2. adds, commits, and pushes to Git
#library(gert)

#gert::git_status()
#?git_push
# 3. Triggers the server to do a Git pull to get the latest file.
#con <- ssh::ssh_connect("ec2-user@10.2.10.30", keyfile = "C:/Users/mking/.ssh/authorized_keys")
#res <- ssh::ssh_exec_wait(con, command = "sudo su")
#print_stdout <- function(x) {
#  rawToChar(x)
#}
#res <- ssh::ssh_exec_wait(con)
#res <- ssh::ssh_exec_wait(con, command = c("cd /srv/shiny-server/hdms/scl-app", "git status"))
#res <- ssh::ssh_exec_wait(con, c("sudo su", "whoami"))

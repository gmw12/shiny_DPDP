cat(file = stderr(), "Shiny_Startup.R", "\n")

app_startup <- function(session, input, output) {
  cat(file = stderr(), "Function - app_startup", "\n")
  
  #Check if database file present
  
  
  shinyFileChoose(input, 'sfb_design_file', session = session, roots = volumes, filetypes = c('', 'xlsx'))
  
  shinyFileChoose(input, 'sfb_data_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  shinyFileChoose(input, 'sfb_database_file', session = session, roots = volumes, filetypes = c('', 'db'))
  
  cat(file = stderr(), "Function - app_startup...end", "\n")
}
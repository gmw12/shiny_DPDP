cat(file = stderr(), "Shiny_Hide.R", "\n")

hide_enable <- function(session, input, output) {
  cat(file = stderr(), "Function - hide_enable", "\n")
  
  # observe({
  #   params$ptm <<- input$ptm
  #   if (input$ptm) {
  #     shinyjs::show("norm_ptm")
  #     shinyjs::show("norm_ptm_grep")
  #     shinyjs::show("impute_ptm")
  #     shinyjs::show("impute_ptm_grep")
  #   } else {
  #     shinyjs::hide("norm_ptm")
  #     shinyjs::hide("norm_ptm_grep")
  #     shinyjs::hide("impute_ptm")
  #     shinyjs::hide("impute_ptm_grep")
  #   }
  # })
  
  observe({
    if (params$data_source == "PD") {
      shinyjs::show("peptide_select")
      shinyjs::show("use_isoform")
    } else {
      shinyjs::hide("peptide_select")
      shinyjs::hide("use_isoform")
    }
  })
  
  
  
}
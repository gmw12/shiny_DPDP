cat(file = stderr(), "load Shiny_Render.R", "\n")

#-------------------------------------------------------------------------------------------
render_data_info <- function(session, input, output) {
  cat(file = stderr(), "Function render_data_info", "\n")
  output$data_source <- renderText({str_c("Source:  ", params$data_source)})
  output$data_format <- renderText({str_c("Input format:  ", params$raw_data_format)})
  output$data_table_format <- renderText({str_c("Table format:  ", params$data_table_format)})
  if (params$ptm) {
    params_ptm <- "Yes"
  }else{
    params_ptm <- "No"
  }
  output$data_ptm <- renderText({str_c("PTM?:  ", params_ptm)})
  }


#-------------------------------------------------------------------------------------------
render_parameters_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_graphs", "\n")
  
  output$raw_bar <- renderImage({
    list(src = str_c(params$qc_path,"Raw_Precursor_barplot.png"), contentType = 'image/png', width = 600, height = 500, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$raw_box <- renderImage({
    list(src = str_c(params$qc_path,"Raw_Precursor_boxplot.png"), contentType = 'image/png', width = 600, height = 500, alt = "this is alt text")
  }, deleteFile = FALSE)

  output$meta_raw_precursor <- renderText({str_c('Raw Precursors:  ', params$meta_raw_precursor)})
  output$meta_raw_peptide <- renderText({str_c('Raw Peptides:  ', params$meta_raw_peptide)})
  output$meta_raw_protein <- renderText({str_c('Raw Protein:  ', params$meta_raw_protein)})
  
}


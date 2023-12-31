cat(file = stderr(), "load Shiny_UI_Update.R", "\n")

#-------------------------------------------------------------------------------------------
ui_render_load_design <- function(session, input, output) {
  cat(file = stderr(), "Function ui_render_load_design", "\n")
  
  output$data_source <- renderText({str_c("Source:  ", params$data_source)})
  output$data_format <- renderText({str_c("Input format:  ", params$raw_data_format)})
  output$data_table_format <- renderText({str_c("Table format:  ", params$data_table_format)})
  
  if (params$ptm) {
    params_ptm <- "Yes"
  }else{
    params_ptm <- "No"
  }
  output$data_ptm <- renderText({str_c("PTM?:  ", params_ptm)})
  
  output$design_file_name <- renderText({params$design_file})

  #create design table
  create_design_table(session, input, output)
  
  }

#-------------------------------------------------------------------------------------------
ui_render_load_data <- function(session, input, output) {
  cat(file = stderr(), "Function ui_render_load_data", "\n")

  output$data_file_name <- renderText({params$data_file})
  
}


#------------------------------------------------------------------------
ui_render_parameters <- function(session, input, output) {
  cat(file = stderr(), "Function ui_render_parameters", "\n")
  
    render_parameters_graphs(session, input, output)
  
    output$meta_precursor_raw <- renderText({str_c('Raw Precursors:  ', params$meta_precursor_raw)})
    output$meta_peptide_raw <- renderText({str_c('Raw Peptides:  ', params$meta_peptide_raw)})
    output$meta_protein_raw <- renderText({str_c('Raw Protein:  ', params$meta_protein_raw)})

}

#-------------------------------------------------------------------------------------------
render_parameters_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_graphs", "\n")
  
  output$raw_bar <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Raw_barplot.png"), contentType = 'image/png', width = 600, height = 500, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$raw_box <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Raw_boxplot.png"), contentType = 'image/png', width = 600, height = 500, alt = "this is alt text")
  }, deleteFile = FALSE)

}


#------------------------------------------------------------------------
ui_render_filter <- function(session, input, output) {
  cat(file = stderr(), "Function ui_update_parameters", "\n")
  
  render_filter_graphs(session, input, output)
  
  output$meta_precursor_filter <- renderText({str_c('Raw Precursors:  ', params$meta_precursor_filter)})
  output$meta_peptide_filter <- renderText({str_c('Raw Peptides:  ', params$meta_peptide_filter)})
  output$meta_protein_filter <- renderText({str_c('Raw Protein:  ', params$meta_protein_filter)})
  
}

#-------------------------------------------------------------------------------------------
render_filter_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_filter_graphs", "\n")
  
  output$filter_bar <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Filter_barplot.png"), contentType = 'image/png', width = 400, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$filter_box <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Filter_boxplot.png"), contentType = 'image/png', width = 400, height = 250, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  
  #same graph reused in norm tab
  output$norm_data_start_bar <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Filter_barplot.png"), contentType = 'image/png', width = 480, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
}


#-------------------------------------------------------------------------------------------
render_norm_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_norm_graphs", "\n")

  output$norm_normdata_bar <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_NormData_barplot.png"), contentType = 'image/png', width = 480, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
}

#-------------------------------------------------------------------------------------------
render_norm_apply_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_norm_apply_graphs", "\n")
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  output$norm_bar <- renderImage({
    list(src = str_c(params$qc_path, "Precursor_", norm_type[1],"_barplot.png"), contentType = 'image/png', width = 480, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
}

#-------------------------------------------------------------------------------------------
render_impute_parameter_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_impute_parameter_graphs", "\n")
  
  output$impute_histogram <- renderImage({
    list(src = str_c(params$qc_path, "Precursor_Filtered_Histogram.png"), contentType = 'image/png', width = 600, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$impute_total_na <- renderText({str_c("Total missing:  ", params$total_na)})
  output$impute_total_misaligned <- renderText({str_c("Total misaligned:  ", params$total_misaligned)})
  
}
#-----------------------------------------------------------------------------------


update_widgets <- function(session, input, output) {
  cat(file = stderr(), "Function - update_widgets", "\n")
  
  if (exists("params")) {
    
    #Load-----------------------------------------------------------------
    updateTextInput(session, 'file_prefix', value = params$file_prefix)
    
    #Parameters--------------------------------------------------------
    updateCheckboxInput(session, 'ptm', value = params$ptm)
    updateCheckboxInput(session, 'norm_ptm', value = params$norm_ptm)
    updateTextInput(session, 'norm_ptm_grep', value = params$norm_ptm_grep)
    updateCheckboxInput(session, 'impute_ptm', value = params$impute_ptm)
    updateTextInput(session, 'impute_ptm_grep', value = params$impute_ptm_grep)
    updateSelectInput(session, 'peptide_select', selected = params$peptide_select)
    updateCheckboxInput(session, 'multi_tmt', value = params$multi_tmt)
    updateCheckboxInput(session, 'use_isoform', value = params$use_isoform)
    
    #Filter---------------------------------------------------
    updateNumericInput(session, 'filter_min_measured_all', value = params$filter_min_measured_all) 
    updateCheckboxInput(session, 'filter_x_percent', value = params$filter_x_percent) 
    updateNumericInput(session, 'filter_x_percent_value', value = params$filter_x_percent_value)
    updateCheckboxInput(session, 'filter_cv', value = params$filter_cv) 
    updateSelectInput(session, 'filter_cv_group', selected = params$filter_cv_group)
    updateNumericInput(session, 'filter_cv_value', value = params$filter_cv_value)
    updateCheckboxInput(session, 'checkbox_misaligned', value = params$checkbox_misaligned) 
    updateNumericInput(session, 'misaligned_cutoff', value = params$misaligned_cutoff)
    updateCheckboxInput(session, 'custom_intensity_cutoff', value = params$custom_intensity_cutoff) 
    updateNumericInput(session, 'intensity_cutoff_sd', value = params$intensity_cutoff_sd)
    
    #Norm---------------------------------------------------
    updateCheckboxInput(session, 'norm_exclude', value = params$norm_exclude) 
    updateTextInput(session, "exclude_norm_grep", value = params$exclude_norm_grep)
    updateCheckboxInput(session, 'norm_include', value = params$norm_include) 
    updateTextInput(session, "include_norm_grep", value = params$include_norm_grep)
    updateCheckboxInput(session, 'norm_ptm', value = params$norm_ptm) 
    updateTextInput(session, "ptm_norm_grep", value = params$ptm_norm_grep)
    if (params$norm_type != "") {
      norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])[[1]]
      updateSelectInput(session, "norm_type", selected = norm_type)
    }
    #Impute---------------------------------------------------
    updateSelectInput(session, "impute_type", selected = params$impute_type)
    updateCheckboxInput(session, 'impute_ptm', value = params$impute_ptm) 
    updateTextInput(session, "ptm_impute_grep", value = params$ptm_impute_grep)
    updateNumericInput(session, 'bottom_x', value = params$bottom_x)
    updateNumericInput(session, 'missing_cutoff', value = params$missing_cutoff)

    
  }
  
  
}

#-----------------------------------------------------------------------------------
parameter_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - parameter_widget_save...", "\n")
  
  names <- c('primary_group', 'data_output', 'ptm', 'multi_tmt', 'peptide_select', 'use_isoform')
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save_to_database()
}

#-----------------------------------------------------------------------------------
filter_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - parameter_widget_save...", "\n")
  
  names <- c('filter_min_measured_all', 'filter_x_percent', 'filter_x_percent_value', 'filter_cv', 'filter_cv_group', 'filter_cv_value',
             "checkbox_misaligned", "misaligned_cutoff", "custom_intensity_cutoff", "intensity_cutoff_sd")
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save_to_database()
  
}

#-----------------------------------------------------------------------------------
norm_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - norm_widget_save...", "\n")
  
  names <- c("norm_exclude", "exclude_norm_grep", "norm_include", "include_norm_grep", "norm_ptm", "norm_ptm_grep")
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save_to_database()
}
#-----------------------------------------------------------------------------------
norm_apply_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - norm_apply_widget_save...", "\n")
  
  if (params$norm_type == "") {
    params$norm_type <<- input$norm_type
  } else {
    norm_type <- str_c(input$norm_type, ",", params$norm_type)
    norm_type <- str_replace_all(norm_type, " ", "")
    norm_type <- as.list(strsplit(norm_type, ",")[[1]])
    norm_type <- unique(norm_type)
    params$norm_type <<- toString(norm_type)
  }
  
  param_save_to_database()
}


impute_apply_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - impute_apply_widget_save...", "\n")
  
  names <- c("impute_type", "impute_ptm", "impute_ptm_grep", "bottom_x", "intensity_cutoff_sd", "missing_cutoff")
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save_to_database()
}


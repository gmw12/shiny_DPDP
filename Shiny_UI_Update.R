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
  
    output$meta_parameters_precursor_raw <- renderText({str_c('Raw Precursors:  ', params$meta_precursor_raw)})
    output$meta_parameters_peptide_raw <- renderText({str_c('Raw Peptides:  ', params$meta_peptide_raw)})
    output$meta_parameters_protein_raw <- renderText({str_c('Raw Protein:  ', params$meta_protein_raw)})

    output$meta_filter_precursor_raw <- renderText({str_c('Raw Precursors:  ', params$meta_precursor_raw)})
    output$meta_filter_peptide_raw <- renderText({str_c('Raw Peptides:  ', params$meta_peptide_raw)})
    output$meta_filter_protein_raw <- renderText({str_c('Raw Protein:  ', params$meta_protein_raw)})
}

#-------------------------------------------------------------------------------------------
render_parameters_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_graphs", "\n")
  
  output$raw_bar <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Start_barplot.png"), contentType = 'image/png', width = 400, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$raw_box <- renderImage({
    list(src = str_c(params$qc_path,"Precursor_Start_boxplot.png"), contentType = 'image/png', width = 400, height = 250, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$start_histogram <- renderImage({
    list(src = str_c(params$qc_path, "Precursor_Start_Histogram.png"), contentType = 'image/png', width = 600, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)

}


#-------------------------------------------------------------------------------------------
render_filter_histogram_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_filter_histogram_graphs", "\n")
  
  output$impute_histogram <- renderImage({
    list(src = str_c(params$qc_path, "Precursor_NoiseFiltered_Histogram.png"), contentType = 'image/png', width = 600, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  output$impute_total_na <- renderText({str_c("Total missing:  ", params$total_na)})
  output$impute_total_misaligned <- renderText({str_c("Total misaligned:  ", params$total_misaligned)})
  
}
#------------------------------------------------------------------------
ui_render_filter <- function(session, input, output) {
  cat(file = stderr(), "Function ui_update_parameters", "\n")
  
  render_filter_graphs(session, input, output)
  
  output$meta_filter_precursor_filtered <- renderText({str_c('Filtered Precursors:  ', params$meta_precursor_filter)})
  output$meta_filter_peptide_filtered <- renderText({str_c('Filtered Peptides:  ', params$meta_peptide_filter)})
  output$meta_filter_protein_filtered <- renderText({str_c('Filtered Protein:  ', params$meta_protein_filter)})
  
}


#-------------------------------------------------------------------------------------------
render_noise_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_filter_graphs", "\n")
  
  output$noise_total <- renderText({str_c('Total data points:  ', params$noise_total)})
  output$noise_count <- renderText({str_c('Noise data points:  ', params$noise_count)})
  output$noise_percent <- renderText({str_c('Percent noise:  ', round((params$noise_count/params$noise_total*100), digits = 2) )})
  output$noise_inflection <- renderText({str_c('Inflection point:  ', params$noise_inflection)})
  
  output$noise_plot <- renderImage({
    list(src = str_c(params$qc_path,"Inflection_Point.png"), contentType = 'image/png', width = 800, height = 600, alt = "this is alt text")
  }, deleteFile = FALSE)
  
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
render_impute_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_impute_graphs", "\n")
  
  output$missing_bar_plot <- renderImage({
    list(src = str_c(params$qc_path,"missing_bar_plot.png"), contentType = 'image/png', width = 400, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
 
  output$missing_percent_plot <- renderImage({
    list(src = str_c(params$qc_path,"missing_percent_plot.png"), contentType = 'image/png', width = 420, height = 480, alt = "this is alt text")
  }, deleteFile = FALSE)
   
}

#-------------------------------------------------------------------------------------------
render_qc_graphs <- function(session, input, output) {
  cat(file = stderr(), "Function render_impute_graphs", "\n")
  
  output$cv_plot <- renderImage({
    list(src = str_c(params$qc_path,"CV_barplot.png"), contentType = 'image/png', width = 800, height = 400, alt = "this is alt text")
  }, deleteFile = FALSE)
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  counter <- seq(1:length(norm_type))

  image_render_loop = function(){
    lapply(counter, 
           function(x) {
             norm <- norm_type[x]
             norm <- stringr::str_replace_all(norm, " ", "")
             plot_name <- str_c(params$qc_path, "protein_", norm, "_barplot.png")
             output_name <- str_c("qc_norm_comp", x)
             output[[output_name]] <- renderImage({
               list(src = plot_name, contentType = 'image/png', width = 300, height = 300, alt = "this is alt text")
             }, deleteFile = FALSE)
           })
  }
  
  image_render_loop()
    
  # for (norm in norm_type) {
  #   norm <- stringr::str_replace_all(norm, " ", "")
  #   plot_name <- str_c(params$qc_path, "protein_", norm, "_barplot.png")
  #   output_name <- str_c("qc_norm_comp", i)
  #   cat(file = stderr(), str_c("norm = ", norm, "   i=", i, "   plot_name=", plot_name, "   output_name=", output_name), "\n")
  #   
  #   output[[output_name]] <- renderImage({
  #     list(src = plot_name, contentType = 'image/png', width = 300, height = 300, alt = "this is alt text")
  #   }, deleteFile = FALSE)
  #   
  #   i <- i + 1
  # }
   

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
    
    #Noise---------------------------------------------------
    updateNumericInput(session, 'noise_baseline_value', value = params$noise_baseline_value)
    updateSelectInput(session, 'noise_type', selected = params$noise_type)
    
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
    #updateSelectInput(session, "impute_plot_norm", named_list(params$norm_type))
    
    #Rollup---------------------------------------------------
    updateRadioButtons(session, "rollup_method", selected = params$rollup_method)
    updateSelectInput(session, "rollup_topn", selected = params$rollup_topn)
    
    #QC
    qc_norm_types <- as.list(strsplit(params$norm_type, ",")[[1]])
    updateSelectInput(session, "qc_norm_type", choices = qc_norm_types)
    updateSelectInput(session, "spike_norm_type", choices = qc_norm_types)
    updateSelectInput(session, "stats_norm_type", choices = qc_norm_types)
    
    #stats
    update_stat_choices(session, input, output)
    
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
noise_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - noise_widget_save...", "\n")
  
  names <- c('noise_type', 'noise_baseline_value')
  
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
  
  norm_type <- str_c(input$norm_type, ",", params$norm_type)
  norm_type <- str_replace_all(norm_type, " ", "")
  norm_type <- as.list(strsplit(norm_type, ",")[[1]])
  norm_type <- unique(norm_type)
  params$norm_type <<- toString(norm_type)

  param_save_to_database()
}

#-----------------------------------------------------------------------------------
impute_apply_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - impute_apply_widget_save...", "\n")
  
  names <- c("impute_type", "impute_ptm", "impute_ptm_grep", "bottom_x", "missing_cutoff")
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save_to_database()
}

#-----------------------------------------------------------------------------------
rollup_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - rollup_widget_save...", "\n")
  
  names <- c("rollup_method", "rollup_topn")
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save_to_database()
  
}

#----------------------------------------------------------------------
update_stat_choices <- function(session, input, output){
  cat(file = stderr(), "function update_stat_choices...", "\n")

  unique_groups <- params$unique_groups
  unique_groups <- str_replace_all(unique_groups, " ", "")
  unique_groups <- as.list(strsplit(unique_groups, ",")[[1]])
  unique_groups <- unique(unique_groups)
  
  updatePickerInput(session, "comp_1N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_2N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_3N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_4N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_5N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_6N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_7N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_8N", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_9N", choices = unique_groups, selected = "-")

  updatePickerInput(session, "comp_1D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_2D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_3D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_4D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_5D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_6D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_7D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_8D", choices = unique_groups, selected = "-")
  updatePickerInput(session, "comp_9D", choices = unique_groups, selected = "-")

  updatePickerInput(session, "comp_spqc", choices = c(unique_groups, "None"), selected = "-")  
  
  # updateTextInput(session, "comp1_name", value = dpmsr_set$y$stats$comp1_name)
  # updateTextInput(session, "comp2_name", value = dpmsr_set$y$stats$comp2_name)
  # updateTextInput(session, "comp3_name", value = dpmsr_set$y$stats$comp3_name)
  # updateTextInput(session, "comp4_name", value = dpmsr_set$y$stats$comp4_name)
  # updateTextInput(session, "comp5_name", value = dpmsr_set$y$stats$comp5_name)
  # updateTextInput(session, "comp6_name", value = dpmsr_set$y$stats$comp6_name)
  # updateTextInput(session, "comp7_name", value = dpmsr_set$y$stats$comp7_name)
  # updateTextInput(session, "comp8_name", value = dpmsr_set$y$stats$comp8_name)
  # updateTextInput(session, "comp9_name", value = dpmsr_set$y$stats$comp9_name)

  cat(file = stderr(), "function update_stat_choices...end", "\n")
}

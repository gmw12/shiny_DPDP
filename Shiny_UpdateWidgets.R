cat(file = stderr(), "Shiny_UpdateWidgets.R", "\n")


#-----------------------------------------------------------------------------------

update_widgets <- function(session, input, output) {
  cat(file = stderr(), "Function - update_widgets", "\n")
  
  if (exists("params")) {
    
    updateTextInput(session, 'file_prefix', value = params$file_prefix)
    
    updateCheckboxInput(session, 'ptm', value = params$ptm)
    updateCheckboxInput(session, 'norm_ptm', value = params$norm_ptm)
    updateTextInput(session, 'norm_ptm_grep', value = params$norm_ptm_grep)
    updateCheckboxInput(session, 'impute_ptm', value = params$norm_ptm)
    updateTextInput(session, 'impute_ptm_grep', value = params$norm_ptm_grep)
    
    updateSelectInput(session, 'peptide_select', selected = params$peptide_select)
    
    updateCheckboxInput(session, 'multi_tmt', value = params$multi_tmt)
    
    updateCheckboxInput(session, 'use_isoform', value = params$use_isoform)
    
  }
  
  
}

#-----------------------------------------------------------------------------------
parameter_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - parameter_widget_save...", "\n")
  
  names <- c('primary_group', 'data_output', 'ptm', 'norm_ptm', 'grep_norm_ptm', 'impute_ptm', 'grep_impute_ptm', 'multi_tmt', 
             'peptide_select', 'use_isoform')
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save()
}

#-----------------------------------------------------------------------------------
filter_widget_save <- function(session, input, output){
  cat(file = stderr(), "Function - parameter_widget_save...", "\n")
  
  names <- c('filter_min_measured_all', 'filter_x_percent', 'filter_x_percent_value', 'filter_cv', 'filter_cv_group', 'filter_cv_value')
  
  for (name in names) {
    params[[name]] <<- input[[name]]
  }
  
  param_save()
  
}
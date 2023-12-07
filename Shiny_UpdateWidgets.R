cat(file = stderr(), "Shiny_UpdateWidgets.R", "\n")

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
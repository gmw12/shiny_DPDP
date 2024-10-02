cat(file = stderr(), "Shiny_Hide.R", "\n")

hide_enable <- function(session, input, output) {
  cat(file = stderr(), "Function - hide_enable", "\n")

  observe({
    if (params$raw_data_format == "protein") {
      updateCheckboxInput(session, "peptide_refilter", value = FALSE)
      updateCheckboxInput(session, "peptide_missing_filter", value = FALSE)
      params$peptide_refilter <<- FALSE
      shinyjs::hide("peptide_refilter")
      shinyjs::hide("peptide_missing_filter")
      shinyjs::hide("peptide_missing_factor")
      #protein_menu(session, input, output)
    } else {
      shinyjs::show("peptide_refilter")
      shinyjs::show("peptide_missing_filter")
      shinyjs::show("peptide_missing_factor")
    }
  })

  observe({
    if (input$impute_type == "duke") {
      shinyjs::show("missing_cutoff")
    } else {
      shinyjs::hide("missing_cutoff")
    }
  })
  
  
  observe({
    if (input$rollup_method == "topn") {
      shinyjs::show("rollup_topn")
    } else {
      shinyjs::hide("rollup_topn")
    }
  })
  
  
  observe({
    if (input$rollup_method == "iq_maxlfq") {
      shinyjs::show("maxlfq_scale")
    } else {
      shinyjs::hide("maxlfq_scale")
    }
  })
  
  observe({
    if (input$norm_type == "protein") {
      shinyjs::show("protein_norm_grep")
      shinyjs::show("protein_norm_search_field")
    } else {
      shinyjs::hide("protein_norm_grep")
      shinyjs::hide("protein_norm_search_field")
    }
  })
  
  
  observe({
    if (params$data_source == "PD") {
      shinyjs::show("peptide_select")
      shinyjs::show("use_isoform")
    } else {
      shinyjs::hide("peptide_select")
      shinyjs::hide("use_isoform")
    }
  })
  
  observe({
    if (input$checkbox_adjpval) {
      shinyjs::show("padjust_options") 
      shinyjs::show("checkbox_filter_adjpval") 
    }else{
      shinyjs::hide("padjust_options") 
      shinyjs::hide("checkbox_filter_adjpval") 
    }
  })
  
  
    observe({
    if (input$peptide_refilter) {
      shinyjs::show("peptide_missing_filter") 
      shinyjs::show("peptide_cv_filter") 
    }else{
      shinyjs::hide("peptide_missing_filter") 
      shinyjs::hide("peptide_cv_filter") 
      shinyjs::hide("peptide_cv_factor") 
      shinyjs::hide("peptide_missing_factor") 
    }
  })
  
  
  observe({
    if (input$peptide_missing_filter) {
      shinyjs::show("peptide_missing_factor") 
    }else{
      shinyjs::hide("peptide_missing_factor") 
    }
  })
  
  observe({
    if (input$peptide_cv_filter) {
      shinyjs::show("peptide_cv_factor") 
    }else{
      shinyjs::hide("peptide_cv_factor") 
    }
  })
  
  observe({
    if (input$stats_spqc_cv_filter) {
      shinyjs::show("stats_spqc_cv_filter_factor") 
    }else{
      shinyjs::hide("stats_spqc_cv_filter_factor") 
    }
  })
  
  observe({
    if (input$stats_comp_cv_filter) {
      shinyjs::show("stats_comp_cv_filter_factor") 
    }else{
      shinyjs::hide("stats_comp_cv_filter_factor") 
    }
  })
  
  observe({
    if (input$stats_peptide_minimum) {
      shinyjs::show("stats_peptide_minimum_factor") 
    }else{
      shinyjs::hide("stats_peptide_minimum_factor") 
    }
  })
  
  
  observe({
    if (input$checkbox_report_ptm) {
      shinyjs::show("peptide_report_grep") 
    }else{
      shinyjs::hide("peptide_report_grep") 
    }
  })
  
  observe({
    if (input$checkbox_report_accession) {
      shinyjs::show("report_accession") 
    }else{
      shinyjs::hide("report_accession") 
    }
  })
  
  observe({
    for (i in (1:9)) {
      if (i > input$comp_number) {
        shinyjs::hide(str_c("comp", i, "_text"))
        shinyjs::hide(str_c("comp_", i, "N"))
        shinyjs::hide(str_c("comp_", i, "D"))
        shinyjs::hide(str_c("comp", i, "_name"))
      }else{
        shinyjs::show(str_c("comp", i, "_text"))
        shinyjs::show(str_c("comp_", i, "N"))
        shinyjs::show(str_c("comp_", i, "D"))
        shinyjs::show(str_c("comp", i, "_name")) 
      }
      
    }
  })
    

  
}
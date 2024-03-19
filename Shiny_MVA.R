cat(file = stderr(), "Shiny_MVA.R", "\n")

#----------------------------------------------------------------------------------------- 
#create data frame for comparisons
set_stat_groups <- function(session, input, output){
  cat(file = stderr(), "function set_stat_groups....", "\n")
  
  table_name <- str_c("protein_", input$stats_norm_type)
  params$stat_norm <<- input$stats_norm_type
  params$comp_spqc <<- input$comp_spqc
  
  bg_stat_groups <- callr::r_bg(func = set_stat_groups_bg, args = list(params, table_name), stderr = stringr::str_c(params$error_path, "//stat_groups.txt"), supervise = TRUE)
  bg_stat_groups$wait()
  print_stderr("stat_groups.txt")
  
  param_save_to_database()
  cat(file = stderr(), "function set_stat_groups....end", "\n")
}
  
#----------------------------------------------------------------------------------------- 

#create data frame for comparisons
set_stat_groups_bg <- function(params, table_name){
  cat(file = stderr(), "function set_stat_groups_bg....", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_design <- RSQLite::dbReadTable(conn, "design")
  stats_data <- RSQLite::dbReadTable(conn, table_name)
  
  # test for comp descriptions over 31 characters - limit for excel sheet name
  comp_warning <- FALSE
  comp_warning_list <- list()

  #reduce dataframe to data only
  stats_data <- stats_data[(ncol(stats_data) - params$sample_number + 1):ncol(stats_data)]
  
  if (input$select_final_data_stats == "impute") {
    colnames(stats_data) <- df_design$Header3
  }else{
    colnames(stats_data) <- df_design$Header2
  }
  
  #save spqc information
  spqc_df <- stats_data %>% dplyr::select(contains(input$comp_spqc))
  dpmsr_set$y$stats$comp_spqc_sample_numbers <<- list(match(colnames(spqc_df), names(stats_data)))
  dpmsr_set$y$stats$comp_spqc_number <<- length(unlist(dpmsr_set$y$stats$comp_spqc_sample_numbers))
  dpmsr_set$y$stats$comp_spqc_sample_headers <<- list(colnames(spqc_df))
  
  comp_groups <- data.frame(seq(from = 1, to = as.numeric(input$comp_number)))
  colnames(comp_groups) <- "CompNumber"
  color_choices <- randomcoloR::distinctColorPalette(as.numeric(input$comp_number)*2)
  color_choices_N <- color_choices[c(TRUE, FALSE)]
  color_choices_D <- color_choices[c(FALSE, TRUE)]
  cat(file = stderr(), "set_stat_groups....2", "\n")
  
  for (i in 1:input$comp_number) {
    cat(file = stderr(), str_c("begin stat_comp ", i, " of ", input$comp_number), "\n")
    stats_data_N <- stats_data
    stats_data_D <- stats_data
    
    # adding section to reduce complexity of grouping, Min.Samples will not allow other factors into the comparision
    factorsN <- input[[stringr::str_c('comp_',i,'N')]]
    factorsD <- input[[stringr::str_c('comp_',i,'D')]]
    
    # find sample position numbers
    samples_N <- which(df_design$Group %in% as.list(factorsN))
    samples_D <- which(df_design$Group %in% as.list(factorsD))
    cat(file = stderr(), stringr::str_c("samples_N = ", samples_N), "\n")
    cat(file = stderr(), stringr::str_c("samples_D = ", samples_D), "\n")
    
    # reduce dataframe to samples of interest
    stats_data_N <- stats_data_N[c(samples_N)]
    stats_data_D <- stats_data_D[c(samples_D)]
    cat(file = stderr(), stringr::str_c("stats_data_N = ", ncol(stats_data_N)), "\n")
    cat(file = stderr(), stringr::str_c("stats_data_D = ", ncol(stats_data_D)), "\n")
    
    # set comp group names
    comp_groups$comp_N[i] <- paste(unique(unlist(str_split(factorsN, "_"))), collapse = "_")
    comp_groups$comp_D[i] <- paste(unique(unlist(str_split(factorsD, "_"))), collapse = "_")
    comp_groups$comp_name[i] <- str_c(comp_groups$comp_N[i], "_v_", comp_groups$comp_D[i])
    if (nchar(comp_groups$comp_name[i]) > 31) {
      comp_groups$comp_name[i] <- substr(comp_groups$comp_name[i], 1, 31)
      comp_warning <- TRUE
      comp_warning_list = c(comp_warning_list,i)
      cat(file = stderr(), str_c("Comparison ", i," is over 31 characters"), "\n")
    }
    
    if (dpmsr_set$x$primary_group) {
      comp_groups$primary_comp_N[i] <- str_split(comp_groups$comp_N[i], "_")[[1]][1]
      comp_groups$primary_comp_D[i] <- str_split(comp_groups$comp_D[i], "_")[[1]][1]
    }
    
    comp_groups$N_count[i] <- ncol(stats_data_N)
    comp_groups$D_count[i] <- ncol(stats_data_D)
    comp_groups$N_color[i] <- color_choices_N[i] #first i was 1
    comp_groups$D_color[i] <- color_choices_D[i] #first i was 1
    
    #update screen to display the number of samples in the comparison
    updatePickerInput(session, inputId = str_c('comp_',i,'N'), label = str_c("Numerator selected -> ", ncol(stats_data_N)) )
    updatePickerInput(session, inputId = str_c('comp_',i,'D'), label = str_c("Denominator selected -> ", ncol(stats_data_D)) )
    
    updateTextInput(session, inputId = str_c("comp",i,"_name"),  value = comp_groups$comp_name[i])
    
    #save original sample selection
    dpmsr_set$y$stats[[str_c("comp", i, "_N")]] <<- input[[str_c('comp_',i,'N')]]
    dpmsr_set$y$stats[[str_c("comp", i, "_D")]] <<- input[[str_c('comp_',i,'D')]]
    
    comp_groups$sample_numbers_N[i] <- list(samples_N)
    comp_groups$sample_numbers_D[i] <- list(samples_D)
    
    cat(file = stderr(), str_c("end stat_comp ", i, " of ", input$comp_number), "\n")
  }
  
  cat(file = stderr(), "set_stats_groups....3", "\n")
  dpmsr_set$y$stats$groups <<- comp_groups
  dpmsr_set$y$stats$comp_number <<- input$comp_number
  
  # update UI widgets after stat comparisons are selected
  updateTextInput(session, "stats_barplot_title", value = str_c("Barplot ", dpmsr_set$y$stats$groups$comp_name[input$mva_plot_comp]))
  updateTextInput(session, "stats_boxplot_title", value = str_c("Boxplot ", dpmsr_set$y$stats$groups$comp_name[input$mva_plot_comp]))
  update_comparisons(session, input, output)
  
  
  if (comp_warning) {
    comp_warning_list <- paste(unlist(comp_warning_list), collapse = ",")
    shinyalert("Oops!", str_c("Comparison(", unlist(comp_warning_list), ") too long, max=31"), type = "error")
  }
  
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "set_stat_groups_bg....end", "\n")
}

#---------------------------------------------------------------------------------------------------------------------------
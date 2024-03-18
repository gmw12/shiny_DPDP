cat(file = stderr(), "Shiny_MVA.R", "\n")

#----------------------------------------------------------------------------------------- 

#create data frame for comparisons
check_comp_names <- function(session, input, output){
  cat(file = stderr(), "function check_comp_names....", "\n")
  
  for (i in 1:input$comp_number) {
    
    factorsN <- input[[str_c('comp_',i,'N')]]
    factorsD <- input[[str_c('comp_',i,'D')]]
    
    # set comp group names
    comp_groups$comp_N[i] <- paste(unique(unlist(str_split(factorsN, "_"))), collapse = "_")
    comp_groups$comp_D[i] <- paste(unique(unlist(str_split(factorsD, "_"))), collapse = "_")
    comp_groups$comp_name[i] <- str_c(comp_groups$comp_N[i], "_v_", comp_groups$comp_D[i])
    
    if (nchar(comp_groups$comp_name[i]) > 31) {
      comp_groups$comp_name[i] <- substr(comp_groups$comp_name[i], 1, 31)
      comp_warning <- TRUE
      comp_warning_list = c(comp_warning_list,i)
      cat(file = stderr(), str_c("Comparison ", i," is over 31 characters"), "\n")
    }else{
      cat(file = stderr(), str_c("Comparison ", i," name OK"), "\n")
    }
    
    #update screen to display the number of samples in the comparison
    updatePickerInput(session, inputId = str_c('comp_',i,'N'), label = str_c("Numerator selected -> ", ncol(stats_data_N)) )
    updatePickerInput(session, inputId = str_c('comp_',i,'D'), label = str_c("Denominator selected -> ", ncol(stats_data_D)) )
    
    updateTextInput(session, inputId = str_c("comp",i,"_name"),  value = comp_groups$comp_name[i])
     
  }
  
  cat(file = stderr(), "function check_comp_names....end", "\n")
}

#----------------------------------------------------------------------------------------- 

#create data frame for comparisons
set_stat_groups <- function(session, input, output){
  cat(file = stderr(), "function set_stat_groups....", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_design <- RSQLite::dbReadTable(conn, "design")
  RSQLite::dbDisconnect(conn)
  
  
  # test for comp descriptions over 31 characters - limit for excel sheet name
  comp_warning <- FALSE
  comp_warning_list <- list()
  params$uniquegroups <- unique(df_design$Group)
  
  stats_data <- dpmsr_set$data$final[[input$select_final_data_stats]]
  stats_data <- stats_data[(dpmsr_set$y$info_columns_final + 1):(dpmsr_set$y$info_columns_final + dpmsr_set$y$sample_number)]
  
  if (input$select_final_data_stats == "impute") {
    colnames(stats_data) <- dpmsr_set$design$Header3
  }else{
    colnames(stats_data) <- dpmsr_set$design$Header2
  }
  
  #save spqc information
  dpmsr_set$y$stats$comp_spqc <<- input$comp_spqc
  spqc_df <- stats_data %>% dplyr::select(contains(input$comp_spqc))
  dpmsr_set$y$stats$comp_spqc_sample_numbers <<- list(match(colnames(spqc_df), names(stats_data)))
  dpmsr_set$y$stats$comp_spqc_number <<- length(unlist(dpmsr_set$y$stats$comp_spqc_sample_numbers))
  dpmsr_set$y$stats$comp_spqc_sample_headers <<- list(colnames(spqc_df))
  
  
  comp_groups <- data.frame(seq(from = 1, to = as.numeric(input$comp_number)))
  colnames(comp_groups) <- "CompNumber"
  color_choices <- distinctColorPalette(as.numeric(input$comp_number)*2)
  color_choices_N <- color_choices[c(TRUE, FALSE)]
  color_choices_D <- color_choices[c(FALSE, TRUE)]
  cat(file = stderr(), "set_stat_groups....2", "\n")
  
  for (i in 1:input$comp_number) {
    cat(file = stderr(), str_c("begin stat_comp ", i, " of ", input$comp_number), "\n")
    stats_data_N <- stats_data
    stats_data_D <- stats_data
    
    #REBUILDING COMPARISON SELECTOR
    # using contains instead of matches, trying to find all inclusive sample group with not SPQC type
    #for(stats_group in input[[str_c('comp_',i,'N')]]){stats_data_N <- stats_data_N %>% dplyr::select(contains(stats_group))   }
    #for(stats_group in input[[str_c('comp_',i,'D')]]){stats_data_D <- stats_data_D %>% dplyr::select(contains(stats_group))   }
    
    # adding section to reduce complexity of grouping, Min.Samples will not allow other factors into the comparision
    factorsN <- input[[str_c('comp_',i,'N')]]
    factorsD <- input[[str_c('comp_',i,'D')]]
    
    #trouble shooting
    #dfD <<- stats_data_D
    #dfN <<- stats_data_N
    #fN <<- factorsN
    #fD <<- factorsD
    # stats_data_D <- dfD
    # stats_data_N <- dfN
    # factorsN <- fN
    # factorsD <- fD
    # rm(stats_data_D, stats_data_N, factorsN, factorsD, n_max, n_min, d_max, d_min)
    
    # find sample position numbers
    samples_N <- which(dpmsr_set$design$Group %in% as.list(factorsN))
    samples_D <- which(dpmsr_set$design$Group %in% as.list(factorsD))
    cat(file = stderr(), str_c("samples_N = ", samples_N), "\n")
    cat(file = stderr(), str_c("samples_D = ", samples_D), "\n")
    
    
    # reduce dataframe to samples of interest
    stats_data_N <- stats_data_N[c(samples_N)]
    stats_data_D <- stats_data_D[c(samples_D)]
    cat(file = stderr(), str_c("stats_data_N = ", ncol(stats_data_N)), "\n")
    cat(file = stderr(), str_c("stats_data_D = ", ncol(stats_data_D)), "\n")
    
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
    
    
    # set column headers for comparison stat columns
    # comp_groups$fc[i] <- str_c(comp_groups$comp_name[i], "_FC")
    # comp_groups$fc2[i] <- str_c(comp_groups$comp_name[i], "_FC2")
    # comp_groups$pval[i] <- str_c(comp_groups$comp_name[i], "_pval")
    # comp_groups$limma_pval[i] <- str_c(comp_groups$comp_name[i], "_limma_pval")
    # comp_groups$exactTest[i] <- str_c(comp_groups$comp_name[i], "_exactTest")
    # comp_groups$adjpval[i] <- str_c(comp_groups$comp_name[i], "_adjpval")
    # comp_groups$cohensd[i] <- str_c(comp_groups$comp_name[i], "_cohensd")
    # comp_groups$mf[i] <- str_c(comp_groups$comp_name[i], "_MF")
    # comp_groups$cv_N[i] <- str_c(comp_groups$comp_N[i], "_CV")  
    # comp_groups$cv_D[i] <- str_c(comp_groups$comp_D[i], "_CV")  
    
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
  
  cat(file = stderr(), "set_stat_groups....end", "\n")
}

#---------------------------------------------------------------------------------------------------------------------------
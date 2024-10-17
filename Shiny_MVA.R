cat(file = stderr(), "Shiny_MVA.R", "\n")

#----------------------------------------------------------------------------------------- 
#create data frame for comparisons
check_comp_names <- function(session, input, output){
  cat(file = stderr(), "function check_comp_names....", "\n")
  showModal(modalDialog("Setting Stat groups...", footer = NULL))  
  source('Shiny_File.R')
  
  table_name <- str_c("protein_", input$stats_norm_type)
  table_name_peptide <- str_c("peptide_impute_", input$stats_norm_type)
  table_name <- gsub(" ", "", table_name, fixed = TRUE)
  table_name_peptide <- gsub(" ", "", table_name_peptide, fixed = TRUE) 
  
  params$stat_norm <<- input$stats_norm_type
  params$comp_spqc <<- toString(input$comp_spqc)
  params$comp_number <<- input$comp_number
  
  #create df to store comparision info
  stats_comp <- data.frame(matrix(ncol = 13, nrow = 0))

  write_table_try("stats_comp", stats_comp, params)
  
  for (comp_number in 1:input$comp_number) {
    factorsN <- input[[stringr::str_c('comp_',comp_number,'N')]]
    factorsD <- input[[stringr::str_c('comp_',comp_number,'D')]]
    bg_stat_groups <- callr::r_bg(func = check_comp_names_bg, args = list(params, table_name, table_name_peptide, comp_number, factorsN, factorsD), 
                                  stderr = stringr::str_c(params$error_path, "//stat_groups.txt"), supervise = TRUE)
    bg_stat_groups$wait()
    print_stderr("stat_groups.txt")
  }
  
  stats_comp <- read_table_try("stats_comp", params)
  
  for (comp_number in 1:input$comp_number) {
    cat(file = stderr(), str_c("name length = ", nchar(stats_comp$Name[comp_number]) ), "\n")
    if (nchar(stats_comp$Name[comp_number]) > 31) {
      shinyalert("Oops!", str_c("Comparison(", stats_comp$Name[comp_number], ") too long, max=31"), type = "error")
    }
  }

  update_stat_comparisons(session, input, output)
  
  final_stats_name <- table_name <- gsub(" ", "", stringr::str_c("Final_", params$stat_norm,  "_stats.xlsx"), fixed = TRUE)
  updateTextInput(session, "final_stats_name", value = final_stats_name)
  
  cat(file = stderr(), "function check_comp_names....end", "\n")
  removeModal()
}

#----------------------------------------------------------------------------------------- 

#create data frame for comparisons
check_comp_names_bg <- function(params, table_name, table_name_peptide, comp_number, factorsN, factorsD){
  cat(file = stderr(), "function check_comp_names_bg....", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_design <- RSQLite::dbReadTable(conn, "design")
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  
  if (params$data_output == "Protein") {
    stats_data <- RSQLite::dbReadTable(conn, table_name)
  }else {
    stats_data <- RSQLite::dbReadTable(conn, table_name_peptide)
  }
  
  
  #reduce dataframe to data only
  stats_data_info <- stats_data[1:(ncol(stats_data) - params$sample_number)]
  stats_data <- stats_data[(ncol(stats_data) - params$sample_number + 1):ncol(stats_data)]
  stats_data_N <- stats_data
  stats_data_D <- stats_data
  
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
  
  # find SPQC numbers
  stats_data_SPQC <- stats_data
  samples_SPQC <- which(df_design$Group %in% as.list(params$comp_spqc))
  cat(file = stderr(), stringr::str_c("samples_SPQC = ", samples_SPQC), "\n")
    
  # reduce dataframe to SPQC
  stats_data_SPQC <- stats_data_SPQC[c(samples_SPQC)]
  cat(file = stderr(), stringr::str_c("stats_data_SPQC = ", ncol(stats_data_SPQC)), "\n")

  
  #combine info and samples to dataframe for stats later
  if (params$comp_spqc !=0) {
    stats_data_out <- cbind(stats_data_info, stats_data_N, stats_data_D, stats_data_SPQC)
  }else {
    stats_data_out <- cbind(stats_data_info, stats_data_N, stats_data_D)
  }
  #save(samples_D, file="samplesD"); save(samples_N, file="samples_N"); save(samples_SPQC, file="samplesSPQC")
  #load(file="samplesD"); load(file="samples_N"); load(file="samplesSPQC")
  
  # set comp group names
  comp_N <- paste(unique(unlist(stringr::str_split(factorsN, "_"))), collapse = "_")
  comp_D <- paste(unique(unlist(stringr::str_split(factorsD, "_"))), collapse = "_")
  comp_name <- stringr::str_c(comp_N, "_v_", comp_D)
  stats_out_name <- stringr::str_c(table_name, "_", comp_name)
  final_stats_out_name <- stringr::str_c(table_name, "_", comp_name, "_final")
  final_stats_out_name_peptide <- stringr::str_c(table_name_peptide, "_", comp_name, "_final")
  
  total_samples <- ncol(stats_data_N) + ncol(stats_data_D) + ncol(stats_data_SPQC)
  new_row <- c(comp_number, toString(factorsN), toString(factorsD), comp_name, ncol(stats_data_N), ncol(stats_data_D), ncol(stats_data_SPQC), 
               total_samples, toString(samples_N), toString(samples_D), toString(samples_SPQC), stats_out_name, final_stats_out_name,
               final_stats_out_name_peptide)
  stats_comp <- rbind(stats_comp, new_row)
  col_names <- c("Comp", "FactorsN", "FactorsD", "Name", "N", "D", "SPQC", "Total", "N_loc", "D_loc", "SPQC_loc", "Table_Name", 
                 "Final_Table_Name", "Final_Table_Name_Peptide")
  names(stats_comp) <- col_names
  
  RSQLite::dbWriteTable(conn, "stats_comp", stats_comp, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, stats_out_name, stats_data_out, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "set_check_comp_names_bg....end", "\n")
}

#---------------------------------------------------------------------------------------------------------------------------

#create data frame for comparisons
stat_calc <- function(session, input, output){
  cat(file = stderr(), "function stat_calc....", "\n")
  showModal(modalDialog("Calculating stats...", footer = NULL))  
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  RSQLite::dbDisconnect(conn)

  for (comp_number in 1:nrow(stats_comp)) {
    arg_list <- list(params, comp_number, stats_comp)
    bg_stat_calc <- callr::r_bg(func = stat_calc_bg, args = arg_list, stderr = stringr::str_c(params$error_path, "//stat_calc.txt"), supervise = TRUE)
    bg_stat_calc$wait()
    print_stderr("stat_calc.txt")
  }
  
  cat(file = stderr(), "function stat_calc....end", "\n\n")
  removeModal()
}



#----------------------------------------------------------------------------------------- 

#create data frame for comparisons
stat_calc_bg <- function(params, comp_number, stats_comp){
  cat(file = stderr(), "function stat_calc_bg....", "\n")
  source('Shiny_MVA_Functions.R')
  source('Shiny_File.R')

  #save(comp_number, file="z10"); save(stats_comp, file="z11");
  #. load(file="z10"); load(file="z11");
  # stats_comp <- read_table_try("stats_comp", params); comp_number = 3

  
  df_design <- read_table_try("design", params) 
  
  if (!params$peptide_refilter) {
    cat(file = stderr(), "data NOT refiltered at peptide/precursor level....", "\n")
    df <- read_table_try(stats_comp$Table_Name[comp_number], params)
    #add imputed column
    if (params$raw_data_format != "protein"){
      cat(file = stderr(), "raw format NOT protein...", "\n")
      df_list <- add_imputed_df(df, params, stats_comp, comp_number, "protein_missing")
      df <- df_list[[1]]
      df_missing <- df_list[[2]]
    }else{
      cat(file = stderr(), "raw format IS protein...", "\n")
      df_missing <- ""
    }
  } else {
    cat(file = stderr(), "data IS refiltered at peptide/precursor level....", "\n")
    table_name <- stringr::str_replace_all(stringr::str_c("precursor_impute_", params$stat_norm), " ", "")
    df <- read_table_try(table_name, params)  
    df_missing <- read_table_try("precursor_missing", params) 
    
    # reduce precursor df to samples of interest
    df_list <- stat_create_comp_df(df, stats_comp, comp_number, params, df_design)
    
    # reduce missing df to samples of interest
    df_missing_list <- stat_create_comp_missing_df(df_missing, stats_comp, comp_number, params, df_design)
    
    #refilter precursors/peptides
    df_filter_list <- precursor_refilter(df_list, df_missing_list, params)
    
    #rollup, and unpack list
    df_rollup_list <- precursor_refilter_rollup(df_filter_list, df_design, params) 
    df <- df_rollup_list[[1]]
    df_missing <- df_rollup_list[[2]]
    
    #rollup up filtered precursors to peptide to save for graphs
    df_peptide <- rollup_sum_peptide(df_filter_list[[1]], df_design)
    df_peptide_name <- stringr::str_c(stats_comp$Final_Table_Name_Peptide[comp_number])
    
    write_table_try(df_peptide_name, df_peptide, params)
  }
  
  #add stats to df
  df <- stat_add(df, df_missing, params, comp_number, stats_comp, df_design) 
  
  if(params$data_output == "Protein") {
    stats_out_name <- stringr::str_c(stats_comp$Final_Table_Name[comp_number])
  }else{
    stats_out_name <- stringr::str_c(stats_comp$Final_Table_Name_Peptide[comp_number])
  }
  write_table_try(stats_out_name, df, params)
  
  cat(file = stderr(), "function stat_calc_bg....end", "\n")
}

#-------------------------------------------------------------------------------
save_stat_options <- function(session, input, output, params) {
  cat(file = stderr(), "function save_stat_options...", "\n")
  
  params$pvalue_cutoff <- input$pvalue_cutoff
  params$pair_comp <- input$pair_comp
  params$checkbox_adjpval <- input$checkbox_adjpval
  params$padjust_options <- input$padjust_options
  params$foldchange_cutoff <- input$foldchange_cutoff
  params$missing_factor <- input$missing_factor
  params$peptide_refilter <- input$peptide_refilter
  params$peptide_missing_filter <- input$peptide_missing_filter
  params$peptide_missing_factor <- input$peptide_missing_factor
  params$peptide_cv_filter <- input$peptide_cv_filter
  params$peptide_cv_factor <- input$peptide_cv_factor
  params$stats_spqc_cv_filter <- input$stats_spqc_cv_filter
  params$stats_spqc_cv_filter_factor <- input$stats_spqc_cv_filter_factor
  params$stats_comp_cv_filter <- input$stats_comp_cv_filter
  params$stats_comp_cv_filter_factor <- input$stats_comp_cv_filter_factor
  params <<- params

  write_table_try("params", params, params)
}

#----------------------------------------------------------------------------------------
# create final excel documents
stats_Final_Excel <- function(session, input, output, params) {
  cat(file = stderr(), "function stats_Final_Excel...", "\n")
  showModal(modalDialog("Creating/Saving excel file...", footer = NULL))  
  
  require(openxlsx)
  
  file_dir <- stringr::str_c(params$data_path, input$stats_norm_type) 
  filename <- stringr::str_c(params$data_path, input$stats_norm_type, "//", input$final_stats_name)
  filename_params <- stringr::str_c(params$data_path, input$stats_norm_type, "//", params$file_prefix, "_Parameters.xlsx")
  
  if (!fs::is_dir(file_dir)) {
    cat(file = stderr(), str_c("create_dir...", file_dir), "\n")
    dir_create(file_dir)
  }
  
  bg_excel <- callr::r_bg(func = stats_Final_Excel_bg, args = list(file_dir, filename, filename_params, params), stderr = stringr::str_c(params$error_path, "//error_finalexcel.txt"), supervise = TRUE)
  bg_excel$wait()
  print_stderr("error_finalexcel.txt")

  cat(file = stderr(), "function stats_Final_Excel...end", "\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
# create final excel documents
stats_Final_Excel_bg <- function(file_dir, filename, filename_params, params) {
  cat(file = stderr(), "Function stats_Final_Excel_bg...", "\n")
  require(openxlsx)

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  
  cat(file = stderr(), "Creating Excel Output File...2", "\n")
  # Excel worksheets for Precursor/Protein
  if (params$raw_data_format == "precursor" && params$data_output == "Protein") {
    excel_list <- list('precursor_start', 'raw_peptide', stringr::str_c("precursor_impute_", params$stat_norm), 
                       stringr::str_c("protein_", params$stat_norm, "_final"))
    excel_list_name <- list('Raw Precursor Data', 'Raw Peptide Data', 'Imputed Precursor Data', 'Normalized Data')
  }else if (params$raw_data_format == "protein" && params$data_source == "SP" ) {
    excel_list <- list('protein_raw', 'protein_impute')
    excel_list_name <- list('SP Protein Data', "Protein Data")
  }
  
  # add stat comparisons to excel list
  for (i in 1:nrow(stats_comp))  {
    excel_list <- c(excel_list, stats_comp$Final_Table_Name[i])
    excel_list_name <- c(excel_list_name, stats_comp$Name[i])
  }
  
  cat(file = stderr(), "Creating Excel Output File...3", "\n")
  # build Excel file
  nextsheet <- 1
  wb <- createWorkbook()
  for (i in 1:length(excel_list)) {
    addWorksheet(wb, excel_list_name[i])
    table_name <- unlist(excel_list[i])
    excel_df <- RSQLite::dbReadTable(conn, table_name)
    writeData(wb, sheet = nextsheet, excel_df)
    nextsheet <- nextsheet + 1 
  }  

  RSQLite::dbDisconnect(conn)

  cat(file = stderr(), "writting excel to disk...", "\n")
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat(file = stderr(), stringr::str_c("Creating Excel Output File...", filename), "\n")
  
  #save parameters
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Parameters")
  openxlsx::writeData(wb, sheet=1, params)  
  openxlsx::saveWorkbook(wb, filename_params, overwrite = TRUE)
  
  
  cat(file = stderr(), "Function stats_Final_Excel_bg...", "\n")
}

#----------------------------------------------------------------------------------------
create_stats_data_table <- function(session, input, output, params) {
  cat(file = stderr(), "Function create_stats_data_table...", "\n")
  showModal(modalDialog("Creating stats data table...", footer = NULL))  

  arg_list <- list(input$stats_norm_type, input$stats_select_data_comp, input$stats_add_filters, 
                   input$stats_search_field, input$stats_data_description, params)
  
  bg_create_stats_data_table <- callr::r_bg(func = create_stats_data_table_bg, args = arg_list, stderr = str_c(params$error_path, "//error_create_stats_data_table.txt"), supervise = TRUE)
  bg_create_stats_data_table$wait()
  print_stderr("error_create_stats_data_table.txt")
  
  DT_list <- bg_create_stats_data_table$get_result()
  df_DT <- DT_list[[1]]
  options_DT <- DT_list[[2]]
  
  output$stats_data_final <-  DT::renderDataTable(df_DT, rownames = FALSE, extensions = c("FixedColumns"), 
                                                  selection = 'single', options=options_DT,
                                                  callback = DT::JS('table.page(3).draw(false);')
                                                  )
  
  if (params$data_output == "Protein") {
    output$stats_data_final_protein <- renderPrint(df_DT$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] )
    observe({
      updateTextInput(session, "stats_oneprotein_accession", 
                      value = df_DT$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]  )
      updateSelectInput(session, "stats_oneprotein_plot_comp", selected = input$stats_select_data_comp)
      accession_stat <- df_DT$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] 
    })
  }
  
  if (params$data_output == "Peptide") {
    output$stats_data_final_protein <- renderPrint(stringr::str_c(
      df_DT$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])], "  ",
      df_DT$Sequence[as.numeric(unlist(input$stats_data_final_rows_selected)[1])], "  "
    ))
    observe({
      updateTextInput(session, "stats_onepeptide_accession",
                      value = df_DT$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]  )
      updateTextInput(session, "stats_onepeptide_sequence",
                      value = df_DT$Sequence[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]  )
      updateSelectInput(session, "stats_onepeptide_plot_comp", selected = input$stats_select_data_comp)
      accession_stat <- df_DT$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]
    })
  }


  cat(file = stderr(), "Function create_stats_data_table...1", "\n")
  output$download_stats_data_save <- downloadHandler(
    file = function(){
      input$stats_data_filename
    },
    content = function(file){
      fullname <- stringr::str_c(params$data_path, input$stats_norm_type, "//", input$stats_data_filename)
      cat(file = stderr(), stringr::str_c("download_stats_data fullname = ", fullname), "\n")
      file.copy(fullname, file)
    }
  )
  
  removeModal() 
  cat(file = stderr(), "Function create_stats_data_table...end", "\n") 
}

#----------------------------------------------------------------------------------------
create_stats_data_table_bg <- function(input_stats_norm_type, input_stats_select_data_comp, input_stats_add_filters, input_stats_search_field,
                                       input_stats_data_description, params) {
  cat(file = stderr(), "Function create_stats_data_table_bg...", "\n")
  source('Shiny_File.R')
  source('Shiny_Tables.R')
  source('Shiny_MVA_Functions.R')
  
  # save(input_stats_norm_type, file="z5"); save(input_stats_select_data_comp, file="z4"); save(input_stats_add_filters, file="z3"); save(input_stats_search_field, file="z2"); save(input_stats_data_description, file="z1");
  #  load(file="z5"); load(file="z4"); load(file="z3"); load(file="z2"); load(file="z1");
  
  #confirm data exists in database
  if (params$data_output == "Protein") {
    data_name <- stringr::str_c("protein_", input_stats_norm_type, "_", input_stats_select_data_comp, "_final")
  } else {
    data_name <- stringr::str_c("peptide_impute_", input_stats_norm_type, "_", input_stats_select_data_comp, "_final")
  }
  
  cat(file = stderr(), stringr::str_c("data_name --> ", data_name), "\n")
  
  stats_comp <- read_table_try("stats_comp", params)
  
  if (data_name %in% list_tables(params)) {
    cat(file = stderr(), stringr::str_c(data_name, " is in database"), "\n") 
    
    #load data
    df <- read_table_try(data_name, params)
  }else {
    cat(file = stderr(), stringr::str_c(data_name, " is NOT in database"), "\n") 
  }
  
  cat(file = stderr(), "Function create_stats_data_table_bg...1", "\n")
  comp_number <- which(stats_comp$Name == input_stats_select_data_comp)
  sample_number <- as.integer(stats_comp$N[comp_number]) + as.integer(stats_comp$D[comp_number])
  start_sample_col <- min(grep(stats_comp$FactorsN[comp_number], names(df)), grep(stats_comp$FactorsD[comp_number], names(df)))
  spqc_number <- as.integer(stats_comp$SPQC[comp_number])
  
  #filter data for display
  df <- stats_data_table_filter(df, sample_number, start_sample_col, input_stats_add_filters, input_stats_search_field, input_stats_data_description)
  
  cat(file = stderr(), "Function create_stats_data_table_bg...2", "\n")
  if (params$data_output == "Protein") {
    stats_DT <- protein_table(df, start_sample_col, sample_number, spqc_number)
  }else{
    stats_DT <- peptide_table(df, start_sample_col, sample_number, spqc_number)
  }
  
  write_table_try("stats_DT", stats_DT[[1]], params)
  
  return(stats_DT)
  
  cat(file = stderr(), "Function create_stats_data_table_bg...end", "\n") 
}

#-------------------------------------------------------------------------------------------------------------  
stats_data_save_excel <- function(session, input, output, params) {
  cat(file = stderr(), "Function stats_data_save_excel...", "\n")
  showModal(modalDialog("Saving data table to excel...", footer = NULL))  

  arg_list <- list(input$stats_norm_type,  input$stats_data_filename, params)

  bg_stats_data_save_excel <- callr::r_bg(func = stats_data_save_excel_bg , args = arg_list, stderr = str_c(params$error_path, "//error_stats_data_save_excel.txt"), supervise = TRUE)
  bg_stats_data_save_excel$wait()
  print_stderr("error_stats_data_save_excel.txt")
  
  cat(file = stderr(), "Function stats_data_save_excel...end", "\n")
  removeModal()
  
}
#-------------------------------------------------------------------------------------------------------------  
stats_data_save_excel_bg <- function(input_stats_norm_type,  input_stats_data_filename, params) {
  cat(file = stderr(), "Function stats_data_save_excel_bg...", "\n")
  source('Shiny_File.R')
  
  filename <- stringr::str_c(params$data_path, input_stats_norm_type, "//", input_stats_data_filename)
  file_dir <- stringr::str_c(params$data_path, input_stats_norm_type) 
  
  if(!fs::is_dir(file_dir)) {
    cat(file = stderr(), stringr::str_c("create_dir...", file_dir), "\n")
    dir_create(file_dir)
  }
  
  cat(file = stderr(), stringr::str_c("filename = ", filename) , "\n")
  
  stats_DT <- read_table("stats_DT", params)
  Simple_Excel(stats_DT, "data", filename)
  
  cat(file = stderr(), "Function stats_data_save_excel_bg...end", "\n")
  
}
  
#-------------------------------------------------------------------------------------------------------------  
stats_table_select <- function(session, input, output, input_stats_data_final_rows_selected, stats_DT, params) {
  cat(file = stderr(), "Function stats_table_select...", "\n")

  # get selections from data table for protein or peptide formats
  if (params$data_output == "Protein") {
    output$stats_data_final_protein <- renderPrint(stats_DT$x$data$Accession[as.numeric(unlist(input_stats_data_final_rows_selected)[1])] )
    observe({
      updateTextInput(session, "stats_oneprotein_accession", 
                      value = stats_DT$x$data$Accession[as.numeric(unlist(input_stats_data_final_rows_selected)[1])]  )
      updateSelectInput(session, "stats_oneprotein_plot_comp", selected = input$stats_select_data_comp)
      accession_stat <<- stats_DT$x$data$Accession[as.numeric(unlist(input_stats_data_final_rows_selected)[1])] 
    })
  }else{
    output$stats_data_final_protein <- renderPrint(str_c(
      stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])], "  ", 
      stats_DT$x$data$Sequence[as.numeric(unlist(input$stats_data_final_rows_selected)[1])], "  ", 
      stats_DT$x$data$Modification[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] 
    ))
    
    cat(file = stderr(), "Function create_stats_data_table...3", "\n")
    observe({
      updateTextInput(session, "stats_onepeptide_accession", 
                      value = stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]  )
      updateTextInput(session, "stats_onepeptide_sequence", 
                      value = stats_DT$x$data$Sequence[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] ) 
      updateTextInput(session, "stats_onepeptide_modification", 
                      value = stats_DT$x$data$Modification[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] ) 
      updateSelectInput(session, "stats_onepeptide_plot_comp", selected = input$stats_select_data_comp)
      params$accession_stat <<- stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] 
      params$sequence_stat <<- stats_DT$x$data$Sequence[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] 
      params$modification_stat <<- stats_DT$x$data$Modifications[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]
      #protein plot is still used for peptide output
      updateTextInput(session, "stats_oneprotein_accession", 
                      value = stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]  )
      updateSelectInput(session, "stats_oneprotein_plot_comp", selected = input$stats_select_data_comp)
    })
  } 
}

#-------------------------------------------------------------------------------------------------------------  
  create_stats_oneprotein_plots <- function(session, input, output, params) {
    cat(file = stderr(), "Function create_stats_oneprotein_plots...", "\n")
    showModal(modalDialog("Creating stats oneprotein plots...", footer = NULL))  
    
    df_design <- read_table("design", params)
    stats_comp <- read_table("stats_comp", params)
    
    arg_list <- list(input$stats_norm_type, input$stats_oneprotein_plot_comp, input$stats_oneprotein_accession, input$stats_oneprotein_plot_spqc,
                     input$stats_use_zscore, df_design, stats_comp, params)
    
    create_stats_oneprotein_plots <- callr::r_bg(func = create_stats_oneprotein_plots_bg , args = arg_list, stderr = str_c(params$error_path, "//error_create_stats_oneprotein_plots.txt"), supervise = TRUE)
    create_stats_oneprotein_plots$wait()
    print_stderr("error_create_stats_oneprotein_plots.txt")
  
    cat(file = stderr(), "Function create_stats_oneprotein_plots...1", "\n")
    bg_plot_list <- create_stats_oneprotein_plots$get_result()
    df <- bg_plot_list[[1]]
    namex <- bg_plot_list[[2]]
    color_list <- bg_plot_list[[3]]
    peptide_pos_lookup <- bg_plot_list[[4]]
    grouped_color_list <- bg_plot_list[[5]]
    new_df <- bg_plot_list[[6]]
    df_peptide <- bg_plot_list[[7]]
    options_DT <- bg_plot_list[[8]]

    interactive_barplot(session, input, output, df, namex, color_list, "stats_oneprotein_barplot", input$stats_oneprotein_plot_comp, plot_number=1)
    
    interactive_grouped_barplot(session, input, output, new_df, input$stats_oneprotein_plot_comp, grouped_color_list)
    
    output$oneprotein_peptide_table <-  DT::renderDataTable(df_peptide, rownames = FALSE, extensions = c("FixedColumns"), 
                                                    selection = 'single', options=options_DT,
                                                    callback = DT::JS('table.page(3).draw(false);')
    )
    
    output$download_stats_oneprotein_data_save <- downloadHandler(
      file = function(){
        input$stats_oneprotein_data_filename
      },
      content = function(file){
        fullname <- stringr::str_c(params$data_path, input$stats_norm_type, "//", input$stats_oneprotein_data_filename)
        cat(file = stderr(), stringr::str_c("download_stats_oneprotein_data fullname = ", fullname), "\n")
        file.copy(fullname, file)
      }
    )
    
    
    cat(file = stderr(), "Function create_stats_oneprotein_plots...end", "\n")
    removeModal()
    
  }
  
  
  #------------------------------------------------------------------------------------------------------------------
  create_stats_oneprotein_plots_bg <- function(input_stats_norm_type, input_stats_oneprotein_plot_comp, 
                                               input_stats_oneprotein_accession, input_stats_oneprotein_plot_spqc,
                                               input_stats_use_zscore, df_design, stats_comp, params) {
    
    cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...", "\n")
    
    source('Shiny_File.R')
    source('Shiny_Misc_Functions.R')
    source('Shiny_MVA_Functions.R')
    source('Shiny_Tables.R')
    
    #confirm data exists in database
    data_name <- stringr::str_c("protein_", input_stats_norm_type, "_", input_stats_oneprotein_plot_comp, "_final")
    data_name_peptide <- stringr::str_c("peptide_", input_stats_norm_type, "_", input_stats_oneprotein_plot_comp, "_final")
    
    #protein plot
    if (data_name %in% list_tables(params) & data_name_peptide %in% list_tables(params)  ) {
      cat(file = stderr(), stringr::str_c(data_name, " & ", data_name_peptide, " are in database"), "\n") 
      
      df <- filter_db(data_name, "Accession", input_stats_oneprotein_accession, params)
      df_peptide <- filter_db(data_name_peptide, "Accession", input_stats_oneprotein_accession, params)

      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...1", "\n")
      comp_number <- which(stats_comp$Name == input_stats_oneprotein_plot_comp)
      sample_number <- as.integer(stats_comp$N[comp_number]) + as.integer(stats_comp$D[comp_number])
      start_sample_col <- min(grep(stats_comp$FactorsN[comp_number], names(df)), grep(stats_comp$FactorsD[comp_number], names(df)))
      start_sample_col_peptide <- min(grep(stats_comp$FactorsN[comp_number], names(df_peptide)), grep(stats_comp$FactorsD[comp_number], names(df)))
      spqc_number <- as.integer(stats_comp$SPQC[comp_number])
      
      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...2", "\n")
      if(input_stats_oneprotein_plot_spqc) {
        df <- df[,(start_sample_col:(start_sample_col + sample_number + spqc_number - 1))]
        design_pos <- stringr::str_c(unlist(stats_comp$N_loc[comp_number]), ", ", unlist(stats_comp$D_loc[comp_number]), ", ", unlist(stats_comp$SPQC_loc[comp_number]))
      }else{
        df <- df[,(start_sample_col:(start_sample_col + sample_number - 1))]
        design_pos <- stringr::str_c(unlist(stats_comp$N_loc[comp_number]), ", ", unlist(stats_comp$D_loc[comp_number]))
      }
      
      design_pos <- str_to_numlist(design_pos)
      namex <- df_design$Label[design_pos]
      color_list <- df_design$colorlist[design_pos]
      
      #grouped peptide 
      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...3", "\n")
      if (input_stats_use_zscore) {df_peptide <- peptide_zscore(df_peptide, start_sample_col_peptide)}
      peptide_pos_lookup <-  peptide_position_lookup(df_peptide, params)
      grouped_color <- unique(color_list)

      start <- start_sample_col_peptide
      stop <- start + as.numeric(stats_comp$N[comp_number]) -1
      df_N <- cbind(df_peptide$Sequence, df_peptide[, start:stop])
      
      start <- start + as.numeric(stats_comp$N[comp_number])
      stop <- start + as.numeric(stats_comp$D[comp_number]) -1
      df_D <- cbind(df_peptide$Sequence, df_peptide[, start:stop])
      
      start <- start + as.numeric(stats_comp$D[comp_number])
      stop <- start + as.numeric(stats_comp$SPQC[comp_number]) - 1
      df_SPQC <- cbind(df_peptide$Sequence, df_peptide[, start:stop])
      
      stats_data_N <- tidyr::gather(df_N, test, y, unlist(2:ncol(df_N)))
      colnames(stats_data_N) <- c("Sequence", "Name", "y")
      
      stats_data_D <- tidyr::gather(df_D, test, y, unlist(2:ncol(df_D)))
      colnames(stats_data_D) <- c("Sequence", "Name", "y")
      
      stats_data_SPQC <- tidyr::gather(df_SPQC, test, y, unlist(2:ncol(df_SPQC)))
      colnames(stats_data_SPQC) <- c("Sequence", "Name", "y")
      
      #may not need this, need to test running primary group
      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...4", "\n")
      if (params$primary_group == "Primary"){
        stats_data_N$Comp <- stats_comp$FactorsN[comp_number]
        stats_data_D$Comp <- stats_comp$FactorsD[comp_number]
      }else{
        stats_data_N$Comp <- stats_comp$FactorsN[comp_number]
        stats_data_D$Comp <- stats_comp$FactorsD[comp_number]
      }
      
      stats_data_SPQC$Comp <- params$comp_spqc
      stats_data_N$Order <- "1"
      stats_data_D$Order <- "2"
      stats_data_SPQC$Order <- "3"
      
      if(input_stats_oneprotein_plot_spqc){
        stats_data_all <- rbind(stats_data_N, stats_data_D, stats_data_SPQC)
      }else{
        stats_data_all <- rbind(stats_data_N, stats_data_D)
      }
      
      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...5", "\n")
      stats_data_all$Sequence <- gsub("_", "", stats_data_all$Sequence)
      
      new_df <- merge(stats_data_all, peptide_pos_lookup, by="Sequence")
      new_df$Position <- stringr::str_c(new_df$Start, "-", new_df$Stop)
      
      new_df$Name <- NULL
      new_df$Accession <- NULL
      
      new_df$Comp<- as.character(new_df$Comp)
      new_df$Position <- as.character(new_df$Position )
      new_df$Sequence <- as.character(new_df$Sequence)
     
      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...6", "\n") 
      new_df2 <- new_df |> dplyr::group_by(Order, Comp, Sequence, Position, Start, Stop) |> dplyr::summarise(y_mean=mean(y), sd=sd(y))
      
      new_df2 <- data.frame(dplyr::ungroup(new_df2))
      new_df2$Start<- as.numeric(new_df2$Start)
      new_df2$Stop<- as.numeric(new_df2$Stop)
      new_df2 <- new_df2[order(new_df2$Order,new_df2$Start, new_df2$Stop), ]
      
      new_df2$Position <- as.character(new_df2$Position)
      new_df2_sort <- unique(new_df2$Position)
      new_df2$Position <- factor(new_df2$Position, levels = new_df2_sort)
      
      new_df2$Comp <- as.character(new_df2$Comp)
      new_df2_sort2 <- unique(new_df2$Comp)
      new_df2$Comp <- factor(new_df2$Comp, levels = new_df2_sort2)
      
      unique_colors <- unique(color_list)
      grouped_color_list <- rep(unique_colors, nrow(new_df2)/length(unique_colors))

      #table data
      table_data <- protein_peptide_table(df_peptide, peptide_pos_lookup, start_sample_col_peptide)
      df_peptide <- table_data[[1]]
      options_DT <- table_data[[2]]
      write_table_try("oneprotein_peptide_data", df_peptide, params)

      cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...end", "\n")
      return(list(df, namex, color_list, peptide_pos_lookup, grouped_color_list, new_df2, df_peptide, options_DT))

    }
  }

#-------------------------------------------------------------------------------------------------------------  
oneprotein_data_save_excel <- function(session, input, output, params) {
  cat(file = stderr(), "Function oneprotein_data_save_excel...", "\n")
  showModal(modalDialog("Saving data table to excel...", footer = NULL))  
  
  arg_list <- list(input$stats_norm_type,  input$stats_oneprotein_data_filename, params)
  
  bg_stats_data_save_excel <- callr::r_bg(func = oneprotein_data_save_excel_bg , args = arg_list, stderr = str_c(params$error_path, "//error_oneprotein_data_save_excel.txt"), supervise = TRUE)
  bg_stats_data_save_excel$wait()
  print_stderr("error_oneprotein_data_save_excel.txt")
  
  cat(file = stderr(), "Function oneprotein_data_save_excel...end", "\n")
  removeModal()
  
}
#-------------------------------------------------------------------------------------------------------------  
oneprotein_data_save_excel_bg <- function(input_stats_norm_type,  input_stats_oneprotein_data_filename, params) {
  cat(file = stderr(), "Function oneprotein_data_save_excel_bg...", "\n")
  source('Shiny_File.R')
  
  filename <- stringr::str_c(params$data_path, input_stats_norm_type, "//", input_stats_oneprotein_data_filename)
  file_dir <- stringr::str_c(params$data_path, input_stats_norm_type) 
  
  if(!fs::is_dir(file_dir)) {
    cat(file = stderr(), stringr::str_c("create_dir...", file_dir), "\n")
    dir_create(file_dir)
  }
  
  cat(file = stderr(), stringr::str_c("filename = ", filename) , "\n")
  
  df_oneprotein <- read_table_try("oneprotein_peptide_data", params)
  Simple_Excel(df_oneprotein, "data", filename)
  
  cat(file = stderr(), "Function oneprotein_data_save_excel_bg...end", "\n")
  
}


#-------------------------------------------------------------------------------------------------------------  
create_stats_onepeptide_plots <- function(session, input, output, params) {
  cat(file = stderr(), "Function create_stats_onepeptide_plots...", "\n")
  showModal(modalDialog("Creating stats onepeptide plots...", footer = NULL))  
  
  df_design <- read_table_try("design", params)
  stats_comp <- read_table_try("stats_comp", params)
  
  arg_list <- list(input$stats_norm_type, input$stats_onepeptide_plot_comp, input$stats_onepeptide_accession, input$stats_onepeptide_sequence, 
                   input$stats_onepeptide_plot_spqc, input$stats_onepeptide_use_zscore, df_design, stats_comp, params)
  
  create_stats_onepeptide_plots <- callr::r_bg(func = create_stats_onepeptide_plots_bg , args = arg_list, stderr = str_c(params$error_path, "//error_create_stats_onepeptide_plots.txt"), supervise = TRUE)
  create_stats_onepeptide_plots$wait()
  print_stderr("error_create_stats_onepeptide_plots.txt")
  
  cat(file = stderr(), "Function create_stats_onepeptide_plots...1", "\n")
  bg_plot_list <- create_stats_onepeptide_plots$get_result()
  
  save(bg_plot_list, file="z103")
  #.  load(file = "z103")
  
  df_peptide <- bg_plot_list[[1]]
  namex <- bg_plot_list[[2]]
  color_list <- bg_plot_list[[3]]
  peptide_pos_lookup <- bg_plot_list[[4]]
  grouped_color_list <- bg_plot_list[[5]]
  new_df <- bg_plot_list[[6]]
  df <- bg_plot_list[[7]]
  options_DT <- bg_plot_list[[8]]
  

  
  cat(file = stderr(), "Function create_stats_onepeptide_plots...2", "\n")
  interactive_barplot(session, input, output, df_peptide, namex, color_list, "stats_onepeptide_barplot", input$stats_onepeptide_plot_comp, plot_number=1)
  
  cat(file = stderr(), "Function create_stats_onepeptide_plots...3", "\n")
  interactive_grouped_peptide_barplot(session, input, output, new_df, input$stats_onepeptide_plot_comp, grouped_color_list)


  cat(file = stderr(), "Function create_stats_onepeptide_plots...4", "\n")
  output$onepeptide_peptide_table <-  DT::renderDataTable(df, rownames = FALSE, extensions = c("FixedColumns"),
                                                          selection = 'single', options=options_DT,
                                                          callback = DT::JS('table.page(3).draw(false);')
  )

  output$download_stats_onepeptide_data_save <- downloadHandler(
    file = function(){
      input$stats_onepeptide_data_filename
    },
    content = function(file){
      fullname <- stringr::str_c(params$data_path, input$stats_norm_type, "//", input$stats_onepeptide_data_filename)
      cat(file = stderr(), stringr::str_c("download_stats_onepeptide_data fullname = ", fullname), "\n")
      file.copy(fullname, file)
    }
  )
  
  cat(file = stderr(), "Function create_stats_onepeptide_plots...end", "\n")
  removeModal()
  
}

#------------------------------------------------------------------------------------------------------------------
create_stats_onepeptide_plots_bg <- function(input_stats_norm_type, input_stats_onepeptide_plot_comp, 
                                             input_stats_onepeptide_accession, 
                                             input_stats_onepeptide_sequence, input_stats_onepeptide_plot_spqc,
                                             input_stats_onepeptide_use_zscore, df_design, stats_comp, params) {
  
  save(input_stats_norm_type, file="z6"); save(input_stats_onepeptide_plot_comp, file="z5"); save(input_stats_onepeptide_accession, file="z4"); save(input_stats_onepeptide_sequence, file="z4b"); save(input_stats_onepeptide_plot_spqc, file="z3"); save(input_stats_onepeptide_use_zscore, file="z2"); save(df_design, file="z7"); save(stats_comp, file="z1")
  #load(file="z6"); load(file="z5"); load(file="z4"); load(file="z4b"); load(file="z3"); load(file="z2"); load(file="z7"); load(file="z1")
  
  cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...", "\n")
  
  source('Shiny_File.R')
  source('Shiny_Misc_Functions.R')
  source('Shiny_MVA_Functions.R')
  source('Shiny_Tables.R')
  
  #confirm data exists in database
  data_name <- stringr::str_c("peptide_impute_", input_stats_norm_type, "_", input_stats_onepeptide_plot_comp, "_final")

  #peptide plot
  if (data_name %in% list_tables(params) & data_name %in% list_tables(params)  ) {
    cat(file = stderr(), stringr::str_c(data_name, " is in database"), "\n") 
    
    df <- read_table_try(data_name, params)
    
    if(input_stats_onepeptide_accession != "") {
      df <- df[grepl(input_stats_onepeptide_accession, df$Accession, ignore.case = TRUE),]
    }
    
    if(input_stats_onepeptide_sequence != "") {
      grep_seq <- gsub("\\[", "\\\\[", input_stats_onepeptide_sequence)
      grep_seq <- gsub("\\]", "\\\\]", grep_seq)
      grep_seq <- gsub("\\(", "\\\\(", grep_seq)
      grep_seq <- gsub("\\)", "\\\\)", grep_seq)
      df <- df[grepl(grep_seq, df$Sequence, ignore.case = TRUE),]
    }
    
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...1", "\n")
    comp_number <- which(stats_comp$Name == input_stats_onepeptide_plot_comp)
    sample_number <- as.integer(stats_comp$N[comp_number]) + as.integer(stats_comp$D[comp_number])
    start_sample_col <- min(grep(stats_comp$FactorsN[comp_number], names(df)), grep(stats_comp$FactorsD[comp_number], names(df)))
    spqc_number <- as.integer(stats_comp$SPQC[comp_number])
    
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...2", "\n")
    if(input_stats_onepeptide_plot_spqc) {
      df_peptide <- df[,(start_sample_col:(start_sample_col + sample_number + spqc_number - 1))]
      design_pos <- stringr::str_c(unlist(stats_comp$N_loc[comp_number]), ", ", unlist(stats_comp$D_loc[comp_number]), ", ", unlist(stats_comp$SPQC_loc[comp_number]))
    }else{
      df_peptide <- df[,(start_sample_col:(start_sample_col + sample_number - 1))]
      design_pos <- stringr::str_c(unlist(stats_comp$N_loc[comp_number]), ", ", unlist(stats_comp$D_loc[comp_number]))
    }
    
    design_pos <- str_to_numlist(design_pos)
    namex <- df_design$Label[design_pos]
    color_list <- df_design$colorlist[design_pos]
    
    #grouped peptide 
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...3", "\n")
    if (input_stats_onepeptide_use_zscore) {df <- peptide_zscore(df, start_sample_col)}
    peptide_pos_lookup <-  peptide_position_lookup(df, params)
    grouped_color <- unique(color_list)
   
    start <- start_sample_col
    stop <- start + as.numeric(stats_comp$N[comp_number]) -1
    df_N <- cbind(df$Sequence, df[, start:stop])
     
    start <- start + as.numeric(stats_comp$N[comp_number])
    stop <- start + as.numeric(stats_comp$D[comp_number]) -1
    df_D <- cbind(df$Sequence, df[, start:stop])
     
    start <- start + as.numeric(stats_comp$D[comp_number])
    stop <- start + as.numeric(stats_comp$SPQC[comp_number]) - 1
    df_SPQC <- cbind(df$Sequence, df[, start:stop])
     
    stats_data_N <- tidyr::gather(df_N, test, y, unlist(2:ncol(df_N)))
    colnames(stats_data_N) <- c("Sequence", "Name", "y")
     
    stats_data_D <- tidyr::gather(df_D, test, y, unlist(2:ncol(df_D)))
    colnames(stats_data_D) <- c("Sequence", "Name", "y")
     
    stats_data_SPQC <- tidyr::gather(df_SPQC, test, y, unlist(2:ncol(df_SPQC)))
    colnames(stats_data_SPQC) <- c("Sequence", "Name", "y")
     
    #may not need this, need to test running primary group
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...4", "\n")
    if (params$primary_group == "Primary"){
      stats_data_N$Comp <- stats_comp$FactorsN[comp_number]
      stats_data_D$Comp <- stats_comp$FactorsD[comp_number]
      }else{
      stats_data_N$Comp <- stats_comp$FactorsN[comp_number]
      stats_data_D$Comp <- stats_comp$FactorsD[comp_number]
    }
     
    stats_data_SPQC$Comp <- params$comp_spqc
    stats_data_N$Order <- "1"
    stats_data_D$Order <- "2"
    stats_data_SPQC$Order <- "3"
     
    if(input_stats_onepeptide_plot_spqc){
      stats_data_all <- rbind(stats_data_N, stats_data_D, stats_data_SPQC)
    }else{
      stats_data_all <- rbind(stats_data_N, stats_data_D)
    }
     
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...5", "\n")
    stats_data_all$Sequence <- gsub("_", "", stats_data_all$Sequence)
     
    new_df <- merge(stats_data_all, peptide_pos_lookup, by="Sequence")
    new_df$Position <- stringr::str_c(new_df$Start, "-", new_df$Stop)
     
    new_df$Name <- NULL
    new_df$Accession <- NULL
     
    new_df$Comp<- as.character(new_df$Comp)
    new_df$Position <- as.character(new_df$Position )
    new_df$Sequence <- as.character(new_df$Sequence)
     
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...6", "\n") 
    new_df2 <- new_df |> dplyr::group_by(Order, Comp, Sequence, Position, Start, Stop) |> dplyr::summarise(y_mean=mean(y), sd=sd(y))
    
    new_df2 <- data.frame(dplyr::ungroup(new_df2))
    new_df2$Start<- as.numeric(new_df2$Start)
    new_df2$Stop<- as.numeric(new_df2$Stop)
    new_df2 <- new_df2[order(new_df2$Order,new_df2$Start, new_df2$Stop), ]
    
    new_df2$Position <- as.character(new_df2$Position)
    new_df2_sort <- unique(new_df2$Position)
    new_df2$Position <- factor(new_df2$Position, levels = new_df2_sort)
    
    new_df2$Comp <- as.character(new_df2$Comp)
    new_df2_sort2 <- unique(new_df2$Comp)
    new_df2$Comp <- factor(new_df2$Comp, levels = new_df2_sort2)
     
    unique_colors <- unique(color_list)
    grouped_color_list <- rep(unique_colors, nrow(new_df2)/length(unique_colors))
    
    #table data
    table_data <- protein_peptide_table(df, peptide_pos_lookup, start_sample_col)
    df_peptide_table <- table_data[[1]]
    options_DT <- table_data[[2]]
    write_table_try("onepeptide_peptide_data", df, params)
     
    cat(file = stderr(), "Function create_stats_onepeptide_plots_bg...end", "\n")
    return(list(df_peptide, namex, color_list, peptide_pos_lookup, grouped_color_list, new_df2, df_peptide_table, options_DT))
    
  }
}

#---------------------------------------------------------------------
stupid <- function() { 
  cat(file = stderr(), "create_stats_onepeptide_plots...", "\n")
  
  comp_test <- try(which(dpmsr_set$data$stats[[input$stats_onepeptide_plot_comp]] == input$stats_onepeptide_accession), silent =TRUE)
  comp_string <- input$stats_onepeptide_plot_comp
  comp_number <- which(dpmsr_set$y$stats$groups$comp_name == comp_string)
  
  cat(file = stderr(), "create_stats_onepeptide_plots...1", "\n")
  cat(file = stderr(), str_c("comp_number = ", comp_number, " -v- ",  length(dpmsr_set$y$stats$groups$comp_name)  ) , "\n")
  check_qc <- TRUE
  
  #test for all.samples v qc and adding qc
  # if(input$stats_onepeptide_plot_spqc & comp_number==length(dpmsr_set$y$stats$groups$comp_name)) {
  #   cat(file = stderr(), "Reset SPQC", "\n")
  #   updateCheckboxInput(session, "stats_onepeptide_plot_spqc",  value = FALSE)
  #   check_qc <- FALSE
  # }
  
  #test for SPQC group already in comparison, if so turn off and warn
  cat(file = stderr(), "create_stats_onepeptide_plots...2", "\n")
  if(input$stats_onepeptide_plot_spqc & grepl(dpmsr_set$y$stats$comp_spqc, dpmsr_set$y$stats$groups$comp_name[comp_number]) ) {
    cat(file = stderr(), "Reset SPQC", "\n")
    updateCheckboxInput(session, "stats_onepeptide_plot_spqc",  value = FALSE)
    check_qc <- FALSE
  }
  
  cat(file = stderr(), "create_stats_onepeptide_plots...3", "\n")
  if (length(comp_test)!=0 & check_qc){  
    
    df_list <- onepeptide_data(session, input, output)
    #df_peptide in df_list
    for(j in names(df_list)){assign(j, df_list[[j]]) }
    
    cat(file = stderr(), "create_stats_onepeptide_plots...4", "\n")
    interactive_barplot(session, input, output, df, namex, color_list, "stats_onepeptide_barplot", comp_string)
    
    peptide_pos_lookup <-  peptide_position_lookup(session, input, output, as.character(input$stats_onepeptide_accession))
    grouped_color <- unique(color_list)
    #interactive_grouped_peptide_barplot(session, input, output, comp_string, df_peptide, info_columns, comp_name, peptide_pos_lookup, grouped_color)
    interactive_grouped_peptide_barplot(session, input, output, comp_string, df_peptide, info_columns, input$stats_onepeptide_plot_comp, peptide_pos_lookup, grouped_color)
    
    cat(file = stderr(), "create_stats_onepeptide_plots...5", "\n")
    sample_col_numbers <- seq(from=12, to = ncol(df_peptide) )
    df_peptide <- cbind(df_peptide, df_peptide_stats)
    
    df_peptide <- merge(df_peptide, peptide_pos_lookup, by=(c("Accession", "Sequence"))    )
    df_peptide$Start <- as.numeric(df_peptide$Start)
    df_peptide$Stop <- as.numeric(df_peptide$Stop)
    df_peptide<- df_peptide %>% dplyr::select(Stop, everything())
    df_peptide <- df_peptide %>% dplyr::select(Start, everything())
    df_peptide <- df_peptide[order(df_peptide$Start, df_peptide$Stop), ]
    
    cat(file = stderr(), "create_stats_onepeptide_plots...6", "\n")
    onepeptide_peptide_DT <-  DT::datatable(df_peptide,
                                            rownames = FALSE,
                                            extensions = c("FixedColumns"), #, "Buttons"),
                                            options=list(
                                              #dom = 'Bfrtipl',
                                              autoWidth = TRUE,
                                              scrollX = TRUE,
                                              scrollY=500,
                                              scrollCollapse=TRUE,
                                              columnDefs = list(list(targets = c(0,1), visibile = TRUE, "width"='30', className = 'dt-center'),
                                                                list(targets = c(2), visible = TRUE, "width"='20', className = 'dt-center'),
                                                                list(
                                                                  targets = c(5),
                                                                  width = '250',
                                                                  render = JS(
                                                                    "function(data, type, row, meta) {",
                                                                    "return type === 'display' && data.length > 35 ?",
                                                                    "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                                                    "}")
                                                                ),
                                                                list(
                                                                  targets = c(6),
                                                                  width = '150',
                                                                  render = JS(
                                                                    "function(data, type, row, meta) {",
                                                                    "return type === 'display' && data.length > 35 ?",
                                                                    "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                                                    "}")
                                                                ),
                                                                list(
                                                                  targets = c(10),
                                                                  width = '100',
                                                                  render = JS(
                                                                    "function(data, type, row, meta) {",
                                                                    "return type === 'display' && data.length > 20 ?",
                                                                    "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                                    "}")
                                                                )
                                              ),
                                              ordering = TRUE,
                                              orderClasses= TRUE,
                                              fixedColumns = list(leftColumns = 2),
                                              pageLength = 100, lengthMenu = c(10,50,100,200)),
                                            #buttons=c('copy', 'csv', 'excelHtml5', 'pdf')),
                                            callback = JS('table.page(3).draw(false);'
                                            ))
    
    onepeptide_peptide_DT <- onepeptide_peptide_DT %>%  formatRound(columns=c(sample_col_numbers), digits=2)
    
    output$onepeptide_peptide_table<-  DT::renderDataTable({onepeptide_peptide_DT })
    
    
  }else{
    shinyalert("Oops!", "No Accession or SPQC add error", type = "error")
  }
  removeModal()
  cat(file = stderr(), "create_stats_onepeptide_plots...end", "\n")
  
}





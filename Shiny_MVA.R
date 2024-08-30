cat(file = stderr(), "Shiny_MVA.R", "\n")

#----------------------------------------------------------------------------------------- 
#create data frame for comparisons
check_comp_names <- function(session, input, output){
  cat(file = stderr(), "function check_comp_names....", "\n")
  showModal(modalDialog("Setting Stat groups...", footer = NULL))  
  
  table_name <- str_c("protein_", input$stats_norm_type)
  table_name <- gsub(" ", "", table_name, fixed = TRUE) 
  
  params$stat_norm <<- input$stats_norm_type
  params$comp_spqc <<- toString(input$comp_spqc)
  params$comp_number <<- input$comp_number
  
  #create df to store comparision info
  stats_comp <- data.frame(matrix(ncol = 13, nrow = 0))

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "stats_comp", stats_comp, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  for (comp_number in 1:input$comp_number) {
    factorsN <- input[[stringr::str_c('comp_',comp_number,'N')]]
    factorsD <- input[[stringr::str_c('comp_',comp_number,'D')]]
    bg_stat_groups <- callr::r_bg(func = check_comp_names_bg, args = list(params, table_name, comp_number, factorsN, factorsD), 
                                  stderr = stringr::str_c(params$error_path, "//stat_groups.txt"), supervise = TRUE)
    bg_stat_groups$wait()
    print_stderr("stat_groups.txt")
  }
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  RSQLite::dbDisconnect(conn)
  
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
check_comp_names_bg <- function(params, table_name, comp_number, factorsN, factorsD){
  cat(file = stderr(), "function check_comp_names_bg....", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_design <- RSQLite::dbReadTable(conn, "design")
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  stats_data <- RSQLite::dbReadTable(conn, table_name)
  
  #reduce dataframe to data only
  stats_data_info <- stats_data[1:(ncol(stats_data) - params$sample_number)]
  stats_data <- stats_data[(ncol(stats_data) - params$sample_number + 1):ncol(stats_data)]
  stats_data_N <- stats_data
  stats_data_D <- stats_data
  stats_data_SPQC <- stats_data
    
  # find sample position numbers
  samples_N <- which(df_design$Group %in% as.list(factorsN))
  samples_D <- which(df_design$Group %in% as.list(factorsD))
  cat(file = stderr(), stringr::str_c("samples_N = ", samples_N), "\n")
  cat(file = stderr(), stringr::str_c("samples_D = ", samples_D), "\n")
   
  # find SPQC numbers
  samples_SPQC <- which(df_design$Group %in% as.list(params$comp_spqc))
  cat(file = stderr(), stringr::str_c("samples_SPQC = ", samples_SPQC), "\n")
   
  # reduce dataframe to samples of interest
  stats_data_N <- stats_data_N[c(samples_N)]
  stats_data_D <- stats_data_D[c(samples_D)]
  cat(file = stderr(), stringr::str_c("stats_data_N = ", ncol(stats_data_N)), "\n")
  cat(file = stderr(), stringr::str_c("stats_data_D = ", ncol(stats_data_D)), "\n")
  
  # reduce dataframe to SPQC
  stats_data_SPQC <- stats_data_SPQC[c(samples_SPQC)]
  cat(file = stderr(), stringr::str_c("stats_data_SPQC = ", ncol(stats_data_SPQC)), "\n")
  
  #combine info and samples to dataframe for stats later
  stats_data_out <- cbind(stats_data_info, stats_data_N, stats_data_D, stats_data_SPQC)
    
  # set comp group names
  comp_N <- paste(unique(unlist(stringr::str_split(factorsN, "_"))), collapse = "_")
  comp_D <- paste(unique(unlist(stringr::str_split(factorsD, "_"))), collapse = "_")
  comp_name <- stringr::str_c(comp_N, "_v_", comp_D)
  stats_out_name <- stringr::str_c(table_name, "_", comp_name)
  final_stats_out_name <- stringr::str_c(table_name, "_", comp_name, "_final")
  
  total_samples <- ncol(stats_data_N) + ncol(stats_data_D) + ncol(stats_data_SPQC)
  new_row <- c(comp_number, toString(factorsN), toString(factorsD), comp_name, ncol(stats_data_N), ncol(stats_data_D), ncol(stats_data_SPQC), 
               total_samples, toString(samples_N), toString(samples_D), toString(samples_SPQC), stats_out_name, final_stats_out_name)
  stats_comp <- rbind(stats_comp, new_row)
  col_names <- c("Comp", "FactorsN", "FactorsD", "Name", "N", "D", "SPQC", "Total", "N_loc", "D_loc", "SPQC_loc", "Table_Name", "Final_Table_Name")
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
    bg_stat_calc <- callr::r_bg(func = stat_calc_bg, args = list(params, comp_number, stats_comp), stderr = stringr::str_c(params$error_path, "//stat_calc.txt"), supervise = TRUE)
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

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  
  if (!params$peptide_refilter) {
    df <- RSQLite::dbReadTable(conn, stats_comp$Table_Name[comp_number])
    #add imputed column
    df_list <- add_imputed_df(df, params, stats_comp, comp_number, "protein_missing")
    df <- df_list[[1]]
    df_missing <- df_list[[2]]
  } else {
    table_name <- stringr::str_c("precursor_impute_", params$stat_norm)
    df <- RSQLite::dbReadTable(conn, table_name)  
    df_design <- RSQLite::dbReadTable(conn, "design") 
    df_missing <- RSQLite::dbReadTable(conn, "precursor_missing") 
    
    # reduce precursor df to samples of interest
    df_list <- stat_create_comp_df(df, stats_comp$FactorsN[comp_number], stats_comp$FactorsD[comp_number], params, df_design)
    
    # reduce missing df to samples of interest
    df_missing_list <- stat_create_comp_missing_df(df_missing, stats_comp$FactorsN[comp_number], stats_comp$FactorsD[comp_number], params, df_design)
    
    #refilter precursors/peptides
    df_filter_list <- peptide_refilter(df_list, df_missing_list, params)
    
    #rollup, and unpack list
    df_rollup_list <- peptide_refilter_rollup(df_filter_list, params, df_design)
    df <- df_rollup_list[[1]]
    df_missing <- df_rollup_list[[2]]
  }
  
  #add stats to df
  df <- stat_add(df, df_missing, params, comp_number, stats_comp, df_design) 
  
  
  
  
  stats_out_name <- stringr::str_c(stats_comp$Table_Name[comp_number], "_final")
  RSQLite::dbWriteTable(conn, stats_out_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "function stat_calc_bg....end", "\n")
}

#-------------------------------------------------------------------------------
save_stat_options <- function(session, input, output, params) {
  cat(file = stderr(), "function save_stat_options...", "\n")
  
  params$pvalue_cutoff <<- input$pvalue_cutoff
  params$pair_comp <<- input$pair_comp
  params$checkbox_adjpval <<- input$checkbox_adjpval
  params$padjust_options <<- input$padjust_options
  params$foldchange_cutoff <<- input$foldchange_cutoff
  params$missing_factor <<- input$missing_factor
  
  param_save_to_database()
  
}

#---------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# create final excel documents
stats_Final_Excel <- function(session, input, output, params) {
  cat(file = stderr(), "function stats_Final_Excel...", "\n")
  showModal(modalDialog("Creating/Saving excel file...", footer = NULL))  
  
  require(openxlsx)
  
  file_dir <- stringr::str_c(params$data_path, input$stats_norm_type) 
  filename <- stringr::str_c(params$data_path, input$stats_norm_type, "//", input$final_stats_name)
  
  if (!fs::is_dir(file_dir)) {
    cat(file = stderr(), str_c("create_dir...", file_dir), "\n")
    dir_create(file_dir)
  }
  
  bg_excel <- callr::r_bg(func = stats_Final_Excel_bg, args = list(file_dir, filename, params), stderr = stringr::str_c(params$error_path, "//error_finalexcel.txt"), supervise = TRUE)
  bg_excel$wait()
  print_stderr("error_finalexcel.txt")

  cat(file = stderr(), "function stats_Final_Excel...end", "\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
# create final excel documents
stats_Final_Excel_bg <- function(file_dir, filename, params) {
  cat(file = stderr(), "Creating Excel Output File...1", "\n")
  require(openxlsx)

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  
  cat(file = stderr(), "Creating Excel Output File...2", "\n")
  # Excel worksheets for Precursor/Protein
  if (params$raw_data_format == "precursor" && params$data_output == "Protein") {
    excel_list <- list('precursor_start', 'raw_peptide', stringr::str_c("precursor_impute_", params$stat_norm), 
                       stringr::str_c("protein_", params$stat_norm, "_final"))
    excel_list_name <- list('Raw Precursor Data', 'Raw Peptide Data', 'Imputed Precursor Data', 'Normalized Data')
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
}

#----------------------------------------------------------------------------------------
create_stats_data_table <- function(session, input, output, params) {
  cat(file = stderr(), "Function create_stats_data_table...", "\n")
  showModal(modalDialog("Creating stats data table...", footer = NULL))  

  arg_list <- list(input$stats_norm_type, input$stats_select_data_comp, input$stats_add_filters, input$stats_data_topn, input$stats_data_accession, input$stats_data_description, params)
  
  bg_create_stats_data_table <- callr::r_bg(func = create_stats_data_table_bg, args = arg_list, stderr = str_c(params$error_path, "//error_create_stats_data_table.txt"), supervise = TRUE)
  bg_create_stats_data_table$wait()
  print_stderr("error_create_stats_data_table.txt")
  
  stats_DT <- bg_create_stats_data_table$get_result()
  output$stats_data_final <-  DT::renderDataTable(stats_DT, selection = 'single' )
  
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
  
  # get selections from data table for protein or peptide formats
  if (params$data_output == "Protein") {
    output$stats_data_final_protein <- renderPrint(stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] )
    observe({
      updateTextInput(session, "stats_oneprotein_accession", 
                      value = stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])]  )
      updateSelectInput(session, "stats_oneprotein_plot_comp", selected = input$stats_select_data_comp)
      params$accession_stat <<- stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] 
    })
  }else{
    output$stats_data_final_protein <- renderPrint(str_c(
      stats_DT$x$data$Accession[as.numeric(unlist(input$stats_data_final_rows_selected)[1])], "  ", 
      stats_DT$x$data$Sequence[as.numeric(unlist(input$stats_data_final_rows_selected)[1])], "  ", 
      stats_DT$x$data$Modification[as.numeric(unlist(input$stats_data_final_rows_selected)[1])] 
    ))
    
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
  
  
  
  
  
  removeModal() 
  cat(file = stderr(), "Function create_stats_data_table...end", "\n") 
}

#----------------------------------------------------------------------------------------
create_stats_data_table_bg <- function(input_stats_norm_type, input_stats_select_data_comp, input_stats_add_filters, input_stats_data_topn, input_stats_data_accession,
                                       input_stats_data_description, params) {
  cat(file = stderr(), "Function create_stats_data_table_bg...", "\n")
  source('Shiny_File.R')
  source('Shiny_Tables.R')
  source('Shiny_MVA_Functions.R')
  
  #confirm data exists in database
  data_name <- stringr::str_c("protein_", input_stats_norm_type, "_", input_stats_select_data_comp, "_final")
  if (data_name %in% list_tables(params)) {
    cat(file = stderr(), stringr::str_c(data_name, " is in database"), "\n") 
    
    #load data
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
    df <- RSQLite::dbReadTable(conn, data_name)
    stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
    RSQLite::dbDisconnect(conn)
  }
  
  comp_number <- which(stats_comp$Name == input_stats_select_data_comp)
  sample_number <- as.integer(stats_comp$N[comp_number]) + as.integer(stats_comp$D[comp_number])
  start_sample_col <- min(grep(stats_comp$FactorsN[comp_number], names(df)), grep(stats_comp$FactorsD[comp_number], names(df)))
  
  #filter data for display
  df <- stats_data_table_filter(df, sample_number, start_sample_col, input_stats_add_filters, input_stats_data_topn, input_stats_data_accession, input_stats_data_description)
  
  if (params$data_output == "Protein") {
    stats_DT <- protein_table(df)
  }else{
    stats_DT <- peptide_table(df)
  }
  
  write_table("stats_DT", stats_DT$x$data, params)
  
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
  create_stats_oneprotein_plots <- function(session, input, output, params) {
    cat(file = stderr(), "Function create_stats_oneprotein_plots...", "\n")
    showModal(modalDialog("Createing stats oneprotein plots...", footer = NULL))  
    
    arg_list <- list(input$stats_norm_type,  input$stats_data_filename, params)
    
    create_stats_oneprotein_plots <- callr::r_bg(func = create_stats_oneprotein_plots_bg , args = arg_list, stderr = str_c(params$error_path, "//error_create_stats_oneprotein_plots.txt"), supervise = TRUE)
    create_stats_oneprotein_plots$wait()
    print_stderr("error_create_stats_oneprotein_plots.txt")
    
    cat(file = stderr(), "Function create_stats_oneprotein_plots...end", "\n")
    removeModal()
    
  }
  
  
  #------------------------------------------------------------------------------------------------------------------
  create_stats_oneprotein_plots_bg <- function(params) {
    cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...", "\n")

    comp_number <- try(which(dpmsr_set$data$stats[[input$stats_oneprotein_plot_comp]] == input$stats_oneprotein_accession), silent = TRUE)
    
    if (length(comp_number) != 0) {  
      
      # do not run peptide section if TMT SPQC Norm
      if (!dpmsr_set$x$tmt_spqc_norm) {
        
        df_list <- oneprotein_data(session, input, output)
        #
        #test_df_list <<- df_list
        
        for (j in names(df_list)) {assign(j, df_list[[j]]) }
        
        interactive_barplot(session, input, output, df, namex, color_list, "stats_oneprotein_barplot", input$stats_oneprotein_plot_comp)
        
        peptide_pos_lookup <-  peptide_position_lookup(session, input, output, as.character(input$stats_oneprotein_accession))
        
        grouped_color <- unique(color_list)
        interactive_grouped_barplot(session, input, output, comp_string, df_peptide, peptide_info_columns, 
                                    input$stats_oneprotein_plot_comp, peptide_pos_lookup, grouped_color)
        
        #test_df_peptide <<- df_peptide
        #test_peptide_pos_lookup <<- peptide_pos_lookup
        
        df_peptide <- merge(df_peptide, peptide_pos_lookup, by = (c("Accession", "Sequence"))    )
        df_peptide$Start <- as.numeric(df_peptide$Start)
        df_peptide$Stop <- as.numeric(df_peptide$Stop)
        df_peptide <- df_peptide %>% dplyr::select(Stop, everything())
        df_peptide <- df_peptide %>% dplyr::select(Start, everything())
        df_peptide <- df_peptide[order(df_peptide$Start, df_peptide$Stop), ]
        
        sample_col_numbers <- seq(from = 14, to = ncol(df_peptide) )
        
        dpmsr_set$data$oneprotein_peptide_DT <<- df_peptide
        
        oneprotein_peptide_DT <-  DT::datatable(df_peptide,
                                                rownames = FALSE,
                                                extensions = c("FixedColumns"), #, "Buttons"),
                                                options = list(
                                                  #dom = 'Bfrtipl',
                                                  autoWidth = TRUE,
                                                  scrollX = TRUE,
                                                  scrollY = 500,
                                                  scrollCollapse = TRUE,
                                                  columnDefs = list(list(targets = c(0,1), visibile = TRUE, "width" = '30', className = 'dt-center'),
                                                                    list(targets = c(2), visible = TRUE, "width" = '20', className = 'dt-center'),
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
                                                                      targets = c(12),
                                                                      width = '100',
                                                                      render = JS(
                                                                        "function(data, type, row, meta) {",
                                                                        "return type === 'display' && data.length > 20 ?",
                                                                        "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                                        "}")
                                                                    )
                                                  ),
                                                  ordering = TRUE,
                                                  orderClasses = TRUE,
                                                  fixedColumns = list(leftColumns = 2),
                                                  pageLength = 100, lengthMenu = c(10,50,100,200)),
                                                #buttons=c('copy', 'csv', 'excelHtml5', 'pdf')),
                                                callback = JS('table.page(3).draw(false);'
                                                ))
        
        oneprotein_peptide_DT <- oneprotein_peptide_DT %>%  formatRound(columns = c(sample_col_numbers), digits = 2)
        
        output$oneprotein_peptide_table <-  DT::renderDataTable({oneprotein_peptide_DT })
      }else {
        
        df_list <- TMT_IRS_protein_data(session, input, output)
        for (j in names(df_list)) {assign(j, df_list[[j]]) }
        
        interactive_barplot(session, input, output, df, namex, color_list, "stats_oneprotein_barplot", input$stats_oneprotein_plot_comp)
        
      } #end of TMT SPQC if 
      
    }else{
      shinyalert("Oops!", "No Accession...", type = "error")
    }
    removeModal()
    cat(file = stderr(), "Function create_stats_oneprotein_plots_bg...end", "\n")
  }
  
  
  #--------------------------------------------------

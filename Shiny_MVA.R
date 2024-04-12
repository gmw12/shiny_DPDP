cat(file = stderr(), "Shiny_MVA.R", "\n")

#----------------------------------------------------------------------------------------- 
#create data frame for comparisons
check_comp_names <- function(session, input, output){
  cat(file = stderr(), "function check_comp_names....", "\n")
  
  table_name <- str_c("protein_", input$stats_norm_type)
  table_name <- gsub(" ", "", table_name, fixed = TRUE) 
  
  params$stat_norm <<- input$stats_norm_type
  params$comp_spqc <<- toString(input$comp_spqc)
  params$comp_number <<- input$comp_number
  
  #create df to store comparision info
  stats_comp <- data.frame(matrix(ncol = 12, nrow = 0))

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
  
  final_stats_name <- table_name <- gsub(" ", "", str_c("Final_", params$stat_norm,  "_stats.xlsx"), fixed = TRUE)
  updateTextInput(session, "final_stats_name", value = final_stats_name)
  
  cat(file = stderr(), "function check_comp_names....end", "\n")
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
  
  total_samples <- ncol(stats_data_N) + ncol(stats_data_D) + ncol(stats_data_SPQC)
  new_row <- c(comp_number, toString(factorsN), toString(factorsD), comp_name, ncol(stats_data_N), ncol(stats_data_D), ncol(stats_data_SPQC), 
               total_samples, toString(samples_N), toString(samples_D), toString(samples_SPQC), stats_out_name)
  stats_comp <- rbind(stats_comp, new_row)
  col_names <- c("Comp", "FactorsN", "FactorsD", "Name", "N", "D", "SPQC", "Total", "N_loc", "D_loc", "SPQC_loc", "Table_Name")
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

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
  RSQLite::dbDisconnect(conn)
  
  for (comp_number in 1:nrow(stats_comp)) {
    bg_stat_calc <- callr::r_bg(func = stat_calc_bg, args = list(params, comp_number, stats_comp), stderr = stringr::str_c(params$error_path, "//stat_calc.txt"), supervise = TRUE)
    bg_stat_calc$wait()
    print_stderr("stat_calc.txt")
  }
  
  cat(file = stderr(), "function stat_calc....end", "\n\n")
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
    df <- add_full_imputed(df, params)
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
    
    #rollup
    df <- peptide_refilter_rollup(df_filter_list, params, df_design)
    
  }
  
  cat(file = stderr(), "function stat_calc_bg....2", "\n")
  stats_out_name <- stringr::str_c(stats_comp$Table_Name[comp_number], "_final")
  RSQLite::dbWriteTable(conn, stats_out_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "function stat_calc_bg....end", "\n")
}










cat(file = stderr(), "Shiny_MVA.R", "\n")

#----------------------------------------------------------------------------------------- 
#create data frame for comparisons
check_comp_names <- function(session, input, output){
  cat(file = stderr(), "function check_comp_names....", "\n")
  
  table_name <- str_c("protein_", input$stats_norm_type)
  params$stat_norm <<- input$stats_norm_type
  params$comp_spqc <<- toString(input$comp_spqc)
  params$comp_number <<- input$comp_number
  
  #create df to store comparision info
  stats_comp <- data.frame(matrix(ncol = 6, nrow = 0))

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "stats_comp", stats_comp, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  for (comp_number in 1:input$comp_number) {
    factorsN <- input[[stringr::str_c('comp_',comp_number,'N')]]
    factorsD <- input[[stringr::str_c('comp_',comp_number,'D')]]
    bg_stat_groups <- callr::r_bg(func = check_comp_names_bg, args = list(params, table_name, comp_number, factorsN, factorsD), stderr = stringr::str_c(params$error_path, "//stat_groups.txt"), supervise = TRUE)
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
  param_save_to_database()
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
    
  # set comp group names
  comp_N <- paste(unique(unlist(stringr::str_split(factorsN, "_"))), collapse = "_")
  comp_D <- paste(unique(unlist(stringr::str_split(factorsD, "_"))), collapse = "_")
  comp_name <- stringr::str_c(comp_N, "_v_", comp_D)
    
  new_row <- c(comp_number, toString(factorsN), toString(factorsD), comp_name, ncol(stats_data_N), ncol(stats_data_D))
  stats_comp <- rbind(stats_comp, new_row)
  col_names <- c("Comp", "FactorsN", "FactorsD", "Name", "N", "D")
  names(stats_comp) <- col_names
  
  RSQLite::dbWriteTable(conn, "stats_comp", stats_comp, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "set_check_comp_names_bg....end", "\n")
}

#---------------------------------------------------------------------------------------------------------------------------
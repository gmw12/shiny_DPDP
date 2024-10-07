cat(file = stderr(), "Shiny_Norm.R", "\n")
#--------------------------------------------------------------------

norm_filter <- function() {
  cat(file = stderr(), "Function - norm_filter...", "\n")
  showModal(modalDialog("Setting normalization parameters...", footer = NULL))
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "norm filter precursor...", "\n")
    bg_normfilter <- callr::r_bg(func = norm_filter_bg, args = list("precursor_filter", "precursor_normdata", params), stderr = str_c(params$error_path, "//error_normfilter.txt"), supervise = TRUE)
    bg_normfilter$wait()
    print_stderr("error_normfilter.txt")
    
    bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_normdata", "Precursor_NormData", params$qc_path, params), stderr = str_c(params$error_path, "//error_normdatabarplot.txt"), supervise = TRUE)
    bg_bar$wait()
    print_stderr("error_normdatabarplot.txt")
  } 

  cat(file = stderr(), "Function - norm_filter...end", "\n\n")
  removeModal()
}


#--------------------------------------------------------------------------------------
norm_filter_bg <- function(table_name, new_table_name, params) {
  cat(file = stderr(), "Function - norm_filter_bg...", "\n")
  source('Shiny_Norm_Functions.R')
  
  start <- Sys.time()
    
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)

  df <- norm_filter_exclude_include(df, params)
  
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), stringr::str_c("norm_filter completed in ", Sys.time() - start), "\n")
  
}


#--------------------------------------------------------------------------------------
norm_apply <- function(session, input, output){
  cat(file = stderr(), "Function - norm_apply...", "\n")
  showModal(modalDialog("Normalizing data...", footer = NULL))
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "normalizing precursor data...", "\n")
    table_data <- "precursor_filter"
    table_norm_data <- "precursor_normdata"
    info_columns <- params$info_col_precursor
    new_table_name <- str_c("precursor_norm_", norm_type[1])
  } 
  
  #check if first normalization, if so then add impute only
  if (length(as.list(strsplit(params$norm_type, ",")[[1]])) == 2) {
    impute_table_name <- str_c("precursor_norm_", norm_type[2])
    impute_table_name <- stringr::str_replace_all(impute_table_name, " ", "")
    cat(file = stderr(), str_c("first pass adding impute only table... ", impute_table_name), "\n")
    
    bg_normapply_impute <- callr::r_bg(func = norm_apply_bg, args = list(table_data, table_norm_data, impute_table_name, info_columns, params, norm_type[2]), stderr = str_c(params$error_path, "//error_normapplyimpute.txt"), supervise = TRUE)
    bg_normapply_impute$wait()
    print_stderr("error_normapplyimpute.txt")
    cat(file = stderr(), "first pass adding impute only table...end", "\n")
  }
  
  
  bg_normapply <- callr::r_bg(func = norm_apply_bg, args = list(table_data, table_norm_data, new_table_name, info_columns, params, norm_type[1]), stderr = str_c(params$error_path, "//error_normapply.txt"), supervise = TRUE)
  bg_normapply$wait()
  print_stderr("error_normapply.txt")
  
  bg_normbar <- callr::r_bg(func = bar_plot, args = list(new_table_name, str_c("Precursor_",  norm_type[1]), params$qc_path, params), stderr = str_c(params$error_path, "//error_normbarplot.txt"), supervise = TRUE)
  bg_normbar$wait()
  print_stderr("error_normbarplot.txt")
  
  cat(file = stderr(), "Function - norm_apply...end", "\n\n")
  removeModal()
}


#--------------------------------------------------------------------------------------
norm_apply_bg <- function(table_data, table_norm_data, new_table_name, info_columns, params, norm_type) {
  cat(file = stderr(), stringr::str_c("Function - norm_apply_bg...", norm_type), "\n")
  
  source('Shiny_Norm_Functions.R')
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  data_to_norm <- RSQLite::dbReadTable(conn, table_data)
  norm_data <- RSQLite::dbReadTable(conn, table_norm_data)
  
  norm_type <- stringr::str_replace_all(norm_type, " ", "")
  
  if (norm_type == "sl") {
    df <- sl_normalize(norm_data, data_to_norm, info_columns)
  }else if (norm_type == "tmm") {
    df <- tmm_normalize(norm_data, data_to_norm, info_columns)
  }else if (norm_type == "sltmm") {
    df <- sl_normalize(norm_data, data_to_norm, info_columns)
  # }else if (norm_type == "protein") {
  #   df <- protein_normalize(data_to_norm, info_columns, params)
  }else if (norm_type == "ai") {
    df <- ai_normalize(data_to_norm, info_columns)
  }else if (norm_type == "ti") {
    df <- ti_normalize(data_to_norm, info_columns)
  }else if (norm_type == "mi") {
    df <- mi_normalize(data_to_norm, info_columns)
  }else if (norm_type == "mti") {
    df <- mti_normalize(data_to_norm, info_columns)
  }else if (norm_type == "vsn") {
    df <- vsn_normalize(data_to_norm, info_columns)
  }else if (norm_type == "loess") {
    df <- loess_normalize(data_to_norm, info_columns)
  }else if (norm_type == "lr") {
    df <- lr_normalize(data_to_norm, info_columns)
  } else if (norm_type == "quantile") {
    df <- quantile_normalize(data_to_norm, info_columns)
  }else if (norm_type == "directlfq") {
    df <- directlfq_normalize(data_to_norm, info_columns)
  }else if (norm_type == "impute") {
    df <- data_to_norm
  }
  
  cat(file = stderr(), stringr::str_c("write table... ", new_table_name), "\n")
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}








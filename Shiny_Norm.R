cat(file = stderr(), "Shiny_Norm.R", "\n")


norm_filter <- function() {
  cat(file = stderr(), "Function - norm_filter...", "\n")
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "norm filter precursor...", "\n")
    bg_normfilter <- callr::r_bg(func = norm_filter_bg, args = list("precursor_filter", "precursor_normdata", params), stderr = "error_normfilter.txt", supervise = TRUE)
    bg_normfilter$wait()
    cat(file = stderr(), readLines("error_normfilter.txt"), "\n")
    
    bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_normdata", "Precursor_NormData", params$qc_path, params), stderr = "error_normdatabarplot.txt", supervise = TRUE)
    bg_bar$wait()
    cat(file = stderr(), readLines("error_normdatabarplot.txt"), "\n")
  } 

  
}


#--------------------------------------------------------------------------------------
norm_filter_bg <- function(table_name, new_table_name, params) {
  cat(file = stderr(), "Function - norm_filter_bg...", "\n")
  
  start <- Sys.time()
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)

  #if include checked then only keep selected data
  if (params$norm_include) {
    cat(file = stderr(), stringr::str_c("Norm include filter = ", params$include_norm_grep), "\n")
    df <- df[grepl(params$include_norm_grep, df$Description, ignore.case = TRUE),]
  }
  
  #if exclude checked then remove selection from data
  if (params$norm_exclude) {
    cat(file = stderr(), stringr::str_c("Norm exclude filter = ", params$exclude_norm_grep), "\n")
    df <- df[!grepl(params$exclude_norm_grep, df$Description, ignore.case = TRUE),]
  }

  # if ptm norm checked then only use selected data
  if (params$norm_ptm) {
    df <- df[grep(params$norm_ptm_grep, df$Sequence, ignore.case = TRUE),]
  }
    
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), stringr::str_c("norm_filter completed in ", Sys.time() - start), "\n")
  
}











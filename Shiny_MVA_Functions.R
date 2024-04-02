cat(file = stderr(), "Shiny_MVA_Functions.R", "\n")

#-------------------------------------------------------------------------------
stat_create_comp_df <- function(df, factorsN, factorsD, params, df_design) {
  cat(file = stderr(), "Function - stat_create_comp_df...", "\n")
  
  #reduce dataframe to data only
  df_info <- df[1:(ncol(df) - params$sample_number)]
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df_N <- df
  df_D <- df
  df_SPQC <- df
  
  # find sample position numbers
  samples_N <- which(df_design$Group %in% as.list(factorsN))
  samples_D <- which(df_design$Group %in% as.list(factorsD))
  cat(file = stderr(), stringr::str_c("samples_N = ", samples_N), "\n")
  cat(file = stderr(), stringr::str_c("samples_D = ", samples_D), "\n")
  
  # find SPQC numbers
  samples_SPQC <- which(df_design$Group %in% as.list(params$comp_spqc))
  cat(file = stderr(), stringr::str_c("samples_SPQC = ", samples_SPQC), "\n")
  
  # reduce dataframe to samples of interest
  df_N <- df_N[c(samples_N)]
  df_D <- df_D[c(samples_D)]
  cat(file = stderr(), stringr::str_c("df_N = ", ncol(df_N)), "\n")
  cat(file = stderr(), stringr::str_c("df_D = ", ncol(df_D)), "\n")
  
  # reduce dataframe to SPQC
  df_SPQC <- df_SPQC[c(samples_SPQC)]
  cat(file = stderr(), stringr::str_c("df_SPQC = ", ncol(df_SPQC)), "\n")
  
  #combine info and samples to dataframe for stats later
  df_out <- cbind(df_info, df_N, df_D, df_SPQC)
  
  return(df_out)
}


#----------------------------------------------------------------------------------

peptide_refilter <- function(df, params) {
  cat(file = stderr(), "Function - peptide_refilter...", "\n")

  #remove 100% imputed precursors/peptides for this specific comparision
  
  
  if (params$peptide_missing_filter) {
    
  }
 
   
  if (params$peptide_cv_filter) {
    
  }
  
  
  

  cat(file = stderr(), "Function - peptide_refilter...end", "\n")
}

#-------------------------------------------------------------------------------

create_imputed_df <- function(params) {
  cat(file = stderr(), "function create_imputed_df....", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "precursor_filter")
  
  info_columns <- ncol(df) - params$sample_number
  
  df_protein_info <- df |> dplyr::select(contains(c("Accession", "Description", "Genes")))
  df <- df[(info_columns + 1):ncol(df)]
  
  df[df > 0] <- 1
  df[is.na(df)] <- 0
  
  df_protein <- cbind(df_protein_info, df)
  df_protein <- df_protein |> dplyr::group_by(Accession, Description, Genes) |> dplyr::summarise_all(list(sum))
  df_protein <- data.frame(dplyr::ungroup(df_protein))
  df_protein <- df_protein[4:ncol(df_protein)]
  
  RSQLite::dbWriteTable(conn, "precursor_missing", df, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "protein_missing", df_protein, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "function create_imputed_df....end", "\n")
}

#-------------------------------------------------------------------------------
reduce_imputed_df <- function(df) {
  
  df[df == 0] <- "-"
  df <- df |> dplyr::mutate_all(as.character)
  
  while (ncol(df) > 1) {
    df[,1] <- stringr::str_c(df[,1], ".", df[,2])
    df[,2] <- NULL
  }
  colnames(df) <- "Detected_Imputed"
  return(df)
} 

#-------------------------------------------------------------------------------
add_full_imputed_df <- function(df, params) {
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_missing <- RSQLite::dbReadTable(conn, "precursor_missing")
  RSQLite::dbDisconnect(conn)
  
  df_missing <-  reduce_imputed_df(df_missing)
  
  df <- tibble::add_column(df, df_missing, .after = (ncol(df) - params$sample_number + 1))
  
  return(df)
}


#-------------------------------------------------------------------------------
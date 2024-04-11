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
  df_out <- list(df_info, df_N, df_D, df_SPQC)
  
  cat(file = stderr(), "Function - stat_create_comp_df...end", "\n")
  return(df_out)
}

#-------------------------------------------------------------------------------
stat_create_comp_missing_df <- function(df, factorsN, factorsD, params, df_design) {
  cat(file = stderr(), "Function - stat_create_comp_missing_df...", "\n")
  
  #reduce dataframe to data only
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
  df_out <- list(df_N, df_D, df_SPQC)
  
  cat(file = stderr(), "Function - stat_create_comp_missing_df...end", "\n")
  return(df_out)
}

#----------------------------------------------------------------------------------

peptide_refilter <- function(df_list, df_missing_list, params) {
  cat(file = stderr(), "Function - peptide_refilter...", "\n")
  
  source('Shiny_MVA_Functions.R')
  source('Shiny_Stats_Functions.R')
  
  #unpack lists to df's
  df_info <- df_list[[1]]
  df_N <- df_list[[2]]
  df_D <- df_list[[3]]
  df_SPQC <- df_list[[4]]
  
  df <- cbind(df_info, df_N, df_D, df_SPQC)
  
  df_N_missing <- df_missing_list[[1]]
  df_D_missing <- df_missing_list[[2]]
  df_SPQC_missing <- df_missing_list[[3]]
  
  df_missing <- cbind(df_N_missing, df_D_missing, df_SPQC_missing)
  
  #find 100% imputed precursors/peptides for this specific comparision
  all_missing <- which((rowSums(df_N_missing) + rowSums(df_D_missing)) == 0)
  cat(file = stderr(), stringr::str_c("Peptide Filter, all missing ", length(all_missing)), "\n")
  missing_filter <- c()
  cv_filter <- c()
  
  if (params$peptide_missing_filter) {
    missing_filter <- which(missing_factor_gw(df_N_missing, df_D_missing) < params$peptide_missing_factor)
    cat(file = stderr(), stringr::str_c("Peptide Filter, missing filter ", length(missing_filter)), "\n")
  }
 
  if (params$peptide_cv_filter) {
    cv_filter <- which((pmin(percentCV_gw(df_N), percentCV_gw(df_D))) > (params$peptide_cv_factor * 100))   
    cat(file = stderr(), stringr::str_c("Peptide Filter, cv filter ", length(cv_filter)), "\n")
  }
  
  
  #reduce dataframes
  filtered_rows <- unique(sort(c(all_missing, missing_filter, cv_filter)))
  cat(file = stderr(), stringr::str_c("Fitering ", length(filtered_rows), " rows"), "\n")
  df <- df[-filtered_rows,]
  df_missing <- df_missing[-filtered_rows,]
  
  df_missing <- reduce_imputed_df(df_missing)
  sample_cols <- ncol(df_N) + ncol(df_D) + ncol(df_SPQC)
  df <- tibble::add_column(df, df_missing, .after = (ncol(df) - sample_cols + 1))

  return(df)
  
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


#missing factor ---------------------------------
missing_factor_gw <- function(x, y) {
  x <- x |> dplyr::mutate_all(as.numeric)
  y <- y |> dplyr::mutate_all(as.numeric)
  mf_x <- rowSums(x) / ncol(x)
  mf_y <- rowSums(y) / ncol(y)
  df_mf <- data.frame(cbind(mf_x, mf_y), stringsAsFactors = FALSE)
  df_mf$max <-
    apply(
      df_mf,
      1,
      FUN = function(x) {
        max(x, na.rm = TRUE)
      }
    )
  return(signif(df_mf$max, digits = 3))
}



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
  
  # df_missing <- reduce_imputed_df(df_missing)
  # sample_cols <- ncol(df_N) + ncol(df_D) + ncol(df_SPQC)
  # df <- tibble::add_column(df, df_missing, .after = (ncol(df) - sample_cols))

  return(list(df, df_missing))
  
  cat(file = stderr(), "Function - peptide_refilter...end", "\n")
}

#-------------------------------------------------------------------------------
peptide_refilter_rollup <- function(df_filter_list, params, df_design) {
  cat(file = stderr(), "Function - peptide_refilter_rollup...", "\n")
  
  source('Shiny_Rollup_Functions.R')
  source('Shiny_MVA_Functions.R')
  
  #unpack lists to df's
  df <- df_filter_list[[1]]
  df_missing <- df_filter_list[[2]]
  
  #count samples
  sample_count <- ncol(df_missing)
  
  #need to add back info columns to missing for rollup
  df_missing <- cbind(df[,1:(ncol(df) - sample_count)], df_missing)
  df_missing <- df_missing |> dplyr::select(contains(c("Accession", "Description", "Genes", df_design$ID))) |> 
    dplyr::mutate(Precursors = 1, .after = Genes)
  
  #rollup data and missing
  df <- rollup_selector(df, params, df_design)
  df_missing <- rollup_sum(df_missing)
  
  #add missing column to data
  df_missing <- df_missing[, (ncol(df_missing) - sample_count + 1):ncol(df_missing)]
  df_missing_summary <- reduce_imputed_df(df_missing)
  df <- tibble::add_column(df, df_missing_summary, .after = (ncol(df) - params$sample_number))
  
  cat(file = stderr(), "Function - peptide_refilter_rollup...end", "\n")  
  return(list(df, df_missing))
}


#-------------------------------------------------------------------------------

  

stat_add <- function(df, df_missing, params, comp_number, stats_comp, df_design) {
  cat(file = stderr(), "Function - stat_add...", "\n")
  
  source("Shiny_Misc_Functions.R")
  source("Shiny_Stats_Functions.R")
  
  sample_count <- as.numeric(stats_comp$Total[comp_number])
  df_samples <- df[,(ncol(df) - sample_count + 1):ncol(df)]
  
  #fix sample names
  cat(file = stderr(), "stat_add...1", "\n")
  sample_names <- c(df_design$Header2[str_to_numlist(stats_comp$N_loc[comp_number])],
                    df_design$Header2[str_to_numlist(stats_comp$D_loc[comp_number])],
                    df_design$Header2[str_to_numlist(stats_comp$SPQC_loc[comp_number])])
  colnames(df)[(ncol(df) - sample_count + 1):ncol(df)] <- sample_names
  
  cat(file = stderr(), "stat_add...2", "\n")
  df_N <- df_samples[, str_to_numlist(stats_comp$N_loc[comp_number])]
  df_D <- df_samples[, str_to_numlist(stats_comp$D_loc[comp_number])]
  df_SPQC <- df_samples[, str_to_numlist(stats_comp$SPQC_loc[comp_number])]
  
  df_N_missing <- df_missing[, str_to_numlist(stats_comp$N_loc[comp_number])]
  df_D_missing <- df_missing[, str_to_numlist(stats_comp$D_loc[comp_number])]

  cat(file = stderr(), "stat_add...3", "\n")
  df[[stringr::str_c(stats_comp$FactorsN[comp_number], "_CV")]] <- percentCV_gw(df_N)
  df[[stringr::str_c(stats_comp$FactorsD[comp_number], "_CV")]] <- percentCV_gw(df_D)
  df[["SPQC_CV"]] <- percentCV_gw(df_SPQC)
  
  cat(file = stderr(), "stat_add...4", "\n")
  df[[stringr::str_c(stats_comp$Name[comp_number], "_FC")]] <- foldchange_gw(df_N, df_D, params)
  df[[stringr::str_c(stats_comp$Name[comp_number], "_FC2")]] <- foldchange_decimal_gw(df_N, df_D, params)
  df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] <- pvalue_gw(df_N, df_D, params)
  
  cat(file = stderr(), "stat_add...5", "\n")
  if (params$checkbox_adjpval) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_adjpval")]] <- p.adjust(df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] , method = input$padjust_options) 
  }
  
  if (params$checkbox_cohensd) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_cohensd")]] <-  cohend_gw(df_N, df_D, as.logical(input$checkbox_cohensd))
  }
  
  if (params$checkbox_limmapvalue) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_limma")]] <- limma_gw(df_N, df_D)
  }
  
  cat(file = stderr(), "stat_add...5", "\n")
  
  df[[stringr::str_c(stats_comp$Name[comp_number], "_mf")]] <- missing_factor_gw(df_N_missing, df_D_missing)
  
  cat(file = stderr(), "Function - stat_add...end", "\n")  

  return(df)  
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
  cat(file = stderr(), "function reduce_imputed_df...", "\n")
  
  df[df == 0] <- "-"
  df <- df |> dplyr::mutate_all(as.character)
  
  while (ncol(df) > 1) {
    df[,1] <- stringr::str_c(df[,1], ".", df[,2])
    df[,2] <- NULL
  }
  colnames(df) <- "Detected_Imputed"
  
  cat(file = stderr(), "function reduce_imputed_df....end", "\n")
  return(df)
} 

#-------------------------------------------------------------------------------
add_imputed_df <- function(df, params, stats_comp, comp_number, table_name) {
  cat(file = stderr(), "function add_imputed_df...", "\n")
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_missing <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  #select samples in comparison
  df_missing <- df_missing[, c(str_to_numlist(stats_comp$N_loc[comp_number]), str_to_numlist(stats_comp$D_loc[comp_number]), str_to_numlist(stats_comp$SPQC_loc[comp_number]))]
  
  df_missing <-  reduce_imputed_df(df_missing)
  
  df <- tibble::add_column(df, df_missing, .after = (ncol(df) - params$sample_number))
  cat(file = stderr(), "function add_imputed_df...end", "\n")
  return(list(df, df_missing))
}


#-------------------------------------------------------------------------------





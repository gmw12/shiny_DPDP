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
  df <- tibble::add_column(df, df_missing_summary, .after = (ncol(df) - sample_count))
  
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
  
  df_N <- df_samples |> dplyr::select(starts_with(stats_comp$FactorsN[comp_number]))
  df_D <- df_samples |> dplyr::select(starts_with(stats_comp$FactorsD[comp_number]))
  df_SPQC <- df_samples |> dplyr::select(starts_with(params$comp_spqc))
  
  df_N_missing <- df_missing |> dplyr::select(starts_with(stats_comp$FactorsN[comp_number]))
  df_D_missing <- df_missing |> dplyr::select(starts_with(stats_comp$FactorsD[comp_number]))

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
    df[[stringr::str_c(stats_comp$Name[comp_number], "_adjpval")]] <- p.adjust(df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] , method = params$padjust_options) 
  }
  
  if (params$checkbox_cohensd) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_cohensd")]] <-  cohend_gw(df_N, df_D, as.logical(input$checkbox_cohensd))
  }
  
  if (params$checkbox_limmapvalue) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_limma")]] <- limma_gw(df_N, df_D)
  }
  
  cat(file = stderr(), "stat_add...5", "\n")
  
  df[[stringr::str_c(stats_comp$Name[comp_number], "_mf")]] <- missing_factor_gw(df_N_missing, df_D_missing)
  
  # Final filters
  #missing filter, always on...
  filtered_mf <- which(df[[stringr::str_c(stats_comp$Name[comp_number], "_mf")]] < params$missing_factor)  
  df <- df[-(filtered_mf),]
  
  if(params$stats_spqc_cv_filter) {
    filtered_spqc <- which(df[[stringr::str_c(params$comp_spqc, "_CV")]] > params$stats_spqc_cv_filter_factor)  
    df <- df[-(filtered_spqc),]
    }
  
  if(params$stats_comp_cv_filter) {
    filtered_onegroup <- which(df[[stringr::str_c(stats_comp$FactorsN[comp_number], "_CV")]] > params$stats_comp_cv_filter_factor & 
                                 df[[stringr::str_c(stats_comp$FactorsD[comp_number], "_CV")]] > params$stats_comp_cv_filter_factor)  
    df <- df[-(filtered_onegroup),]
    }
  
  if(params$stats_peptide_minimum) {
    filtered_min_peptide <- which(df$Precursors < params$stats_peptide_minimum_factor)
    df <- df[-(filtered_min_peptide),]
    }
  
  #label up/down
  df$Stats <- ""
  if(params$checkbox_filter_adjpval){
    filtered_up <- which(df[[stringr::str_c(stats_comp$Name[comp_number], "_FC")]] >= params$foldchange_cutoff & 
                           df[[stringr::str_c(stats_comp$Name[comp_number], "_adjpval")]] <= params$pvalue_cutoff) 
    filtered_down <- which(df[[stringr::str_c(stats_comp$Name[comp_number], "_FC")]] <= -params$foldchange_cutoff & 
                             df[[stringr::str_c(stats_comp$Name[comp_number], "_adjpval")]] <= params$pvalue_cutoff) 
  }else{
    filtered_up <- which(df[[stringr::str_c(stats_comp$Name[comp_number], "_FC")]] >= params$foldchange_cutoff & 
                           df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] <= params$pvalue_cutoff) 
    filtered_down <- which(df[[stringr::str_c(stats_comp$Name[comp_number], "_FC")]] <= -params$foldchange_cutoff & 
                           df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] <= params$pvalue_cutoff) 
  }
  
  df$Stats[filtered_up] <- "Up"
  df$Stats[filtered_down] <- "Down"
  
  df <- sort_by(df, df$Stats)
  df <- df[order(df$Stats, -df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]], decreasing = TRUE), ]
  
  cat(file = stderr(), "Function - stat_add...end", "\n\n")  

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


# data table filter ------------------------------------------------------

stats_data_table_filter <- function(df, sample_number, start_sample_col, input_stats_add_filters, input_stats_data_topn,
                                    input_stats_data_accession, input_stats_data_description) {
  cat(file = stderr(), "Function stats_data_table_filter...", "\n")
  
  if (dplyr::is.grouped_df(df)) {df <- ungroup(df)}
  
  if (input_stats_add_filters) {
    df <- df |> dplyr::filter(df$Stats == "Up" | df$Stats == "Down")
  }
  
  cat(file = stderr(), "stats_data_table_filter... 1", "\n")
  
  if (input_stats_data_topn != 0 ) {
    df$sum <- rowSums(df[start_sample_col:(start_sample_col + sample_number -1)])
    df <- df[order(-df$sum),]                      
    df <- df[1:input_stats_data_topn,]
    df$sum <- NULL
  }
  
  cat(file = stderr(), "stats_data_table_filter... 2", "\n")
  
  if (input_stats_data_accession != "0" ) {
    df <- df[grep(as.character(input_stats_data_accession), df$Accession), ]
  }
  
  if (input_stats_data_description != "0") {
    df <- df[grep(as.character(input_stats_data_description), df$Description), ]
  }
  
  df_colnames <- colnames(df)
  df_colnames <- gsub("_v_", " v ", df_colnames)
  df_colnames <- gsub("_FC", " FC", df_colnames)
  df_colnames <- gsub("_CV", " CV", df_colnames)
  df_colnames <- gsub("_MF", " MF", df_colnames)
  df_colnames <- gsub("_pval", " pval", df_colnames)
  df_colnames <- gsub("_limmapval", " Limma pval", df_colnames)
  df_colnames <- gsub("_cohensd", " CohensD", df_colnames)
  df_colnames <- gsub("_adjpval", " adjpval", df_colnames)
  df_colnames <- gsub("_", ".", df_colnames)
  colnames(df) <-  df_colnames
  
  cat(file = stderr(), "Function stats_data_table_filter...end", "\n")
  return(df)
}

# one protein data ------------------------------------------------------

oneprotein_data_backup <- function(df, input_stats_oneprotein_plot_comp, input_stats_oneprotein_accession, 
                                   input_stats_oneprotein_plot_spqc, input_stats_use_zscore, df_design, comp_number,
                                   sample_number, start_sample_col, start_sample_col_peptide, spqc_number) {
 
  cat(file = stderr(), "Function oneprotein_data..." , "\n")

  #add spqc to plots
  cat(file = stderr(), "One Protein stats and plots...3" , "\n")
  if (input_stats_oneprotein_plot_spqc) {
    df <- df[(dpmsr_set$y$info_columns_final + 1):(dpmsr_set$y$info_columns_final + sample_number + dpmsr_set$y$stats$comp_spqc_number)]
  }else{
    df <- df[(dpmsr_set$y$info_columns_final + 1):(dpmsr_set$y$info_columns_final + sample_number)]
  }  
  
  

  
  cat(file = stderr(), "Function oneprotein_data...end" , "\n")
}


# one protein data ------------------------------------------------------

oneprotein_data_backup <- function(df, input_stats_oneprotein_plot_comp, input_stats_oneprotein_accession, 
                            input_stats_oneprotein_plot_spqc, input_stats_use_zscore) {
  cat(file = stderr(), "Function oneprotein_data..." , "\n")
  
  if (params$data_output == "Peptide") {
    df_peptide <- dpmsr_set$data$stats[[comp_string]]
    df_peptide <- df_peptide[1:(dpmsr_set$y$info_columns_final + sample_number + dpmsr_set$y$stats$comp_spqc_number)]
  }else{
    df_peptide <- dpmsr_set$data$stats$peptide[[comp_string]]
  }
  
  df_peptide <- subset(df_peptide, Accession %in% as.character(comp_accession)  )
  peptide_info_columns <- ncol(df_peptide) - sample_number - dpmsr_set$y$stats$comp_spqc_number
  
  #add spqc to plots
  cat(file = stderr(), "One Protein stats and plots...3" , "\n")
  if (input_stats_oneprotein_plot_spqc) {
    df <- df[(dpmsr_set$y$info_columns_final + 1):(dpmsr_set$y$info_columns_final + sample_number + dpmsr_set$y$stats$comp_spqc_number)]
  }else{
    df <- df[(dpmsr_set$y$info_columns_final + 1):(dpmsr_set$y$info_columns_final + sample_number)]
  }
  
  comp_rows <- c(dpmsr_set$y$stats$groups$sample_numbers_N[comp_number],dpmsr_set$y$stats$groups$sample_numbers_D[comp_number] )
  #add spqc to plots
  if (input_stats_oneprotein_plot_spqc) {
    comp_rows <- c(comp_rows, dpmsr_set$y$stats$comp_spqc_sample_numbers)
  }
  
  #test_df_peptide <<- df_peptide
  
  cat(file = stderr(), "One Protein stats and plots...4" , "\n")
  comp_rows <- unlist(comp_rows)
  namex <- dpmsr_set$design$Label[comp_rows]
  color_list <- dpmsr_set$design$colorlist[comp_rows]
  groupx <- dpmsr_set$design$Group[comp_rows]
  
  cat(file = stderr(), "One Protein stats and plots...5" , "\n")
  if (dpmsr_set$x$final_data_output != "Peptide") {
    colnames(df_peptide)[(peptide_info_columns + 1):ncol(df_peptide)] <- 
      c(dpmsr_set$design$Header1[unlist(dpmsr_set$y$stats$groups$sample_numbers_N[comp_number])],
        dpmsr_set$design$Header1[unlist(dpmsr_set$y$stats$groups$sample_numbers_D[comp_number])],
        dpmsr_set$design$Header1[unlist(dpmsr_set$y$stats$comp_spqc_sample_numbers)] )
  } 
  
  #sort peptides by intensity, keep highest abundant peptide
  cat(file = stderr(), "One Protein stats and plots...6" , "\n")
  df_peptide$sum <- rowSums(df_peptide[(peptide_info_columns + 1):(peptide_info_columns + sample_number + 1)])
  df_peptide <- df_peptide[order(df_peptide$sum), ]
  df_peptide$sum <- NULL
  df_peptide <- df_peptide[!duplicated(df_peptide$Sequence),]
  
  #test_df_peptide <<- df_peptide
  
  if (input_stats_use_zscore) {df_peptide <- peptide_zscore(df_peptide, peptide_info_columns)}
  
  cat(file = stderr(), "One Protein stats and plots...end" , "\n")
  
  df_list <- list("df" = df, "df_peptide" = df_peptide, "namex" = namex, "color_list" = color_list, "comp_string" = comp_string, "peptide_info_columns" = peptide_info_columns)
  #test_df_list_one_protein <<- df_list
  return(df_list)
  cat(file = stderr(), "Function oneprotein_data...end" , "\n")
}



# one peptide data ------------------------------------------------------
onepeptide_data <- function(session, input, output) {
  
  cat(file = stderr(), "One Peptide stats and plots..." , "\n")
  
  comp_string <- input$stats_onepeptide_plot_comp
  comp_number <- which(dpmsr_set$y$stats$groups$comp_name == comp_string)
  
  cat(file = stderr(), "One Peptide stats and plots...1" , "\n")
  df <- dpmsr_set$data$stats[[comp_string]]
  df <- df[grep(as.character(input$stats_onepeptide_accession), df$Accession), ]
  sample_number <- dpmsr_set$y$stats$groups$N_count[comp_number] + dpmsr_set$y$stats$groups$D_count[comp_number]
  df_peptide <- df
  info_columns <- dpmsr_set$y$info_columns_final
  df_peptide_stats <- df_peptide[(info_columns + sample_number + dpmsr_set$y$stats$comp_spqc_number + 1):ncol(df_peptide)]
  
  #continue filtering of df, used for first plot of a single peptide
  cat(file = stderr(), "One Peptide stats and plots...2" , "\n")
  df <- subset(df, Sequence %in% as.character(input$stats_onepeptide_sequence)  )
  grep_mod <- stringr::str_replace_all(input$stats_onepeptide_modification, "\\[", "\\\\[")
  grep_mod <- stringr::str_replace_all(grep_mod, "\\]", "\\\\]")
  grep_mod <- stringr::str_replace_all(grep_mod,"\\(", "\\\\(")
  grep_mod <- stringr::str_replace_all(grep_mod,"\\)", "\\\\)")
  df <- df %>% filter(stringr::str_detect(Modifications, grep_mod) )
  
  #add spqc to plots
  cat(file = stderr(), "One Peptide stats and plots...3" , "\n")
  if (input$stats_onepeptide_plot_spqc) {
    df_peptide <- df_peptide[1:(info_columns + sample_number + dpmsr_set$y$stats$comp_spqc_number)]
    df <- df[(info_columns + 1):(info_columns + sample_number + dpmsr_set$y$stats$comp_spqc_number)]
    comp_rows <- c(dpmsr_set$y$stats$groups$sample_numbers_N[comp_number],dpmsr_set$y$stats$groups$sample_numbers_D[comp_number], 
                   dpmsr_set$y$stats$comp_spqc_sample_numbers )
  }else{
    df_peptide <- df_peptide[1:(info_columns + sample_number)]
    df <- df[(info_columns + 1):(info_columns + sample_number)]
    comp_rows <- c(dpmsr_set$y$stats$groups$sample_numbers_N[comp_number],dpmsr_set$y$stats$groups$sample_numbers_D[comp_number] )
  }
  
  cat(file = stderr(), "One Peptide stats and plots...4" , "\n")
  comp_rows <- unlist(comp_rows)
  namex <- dpmsr_set$design$Label[comp_rows]
  color_list <- dpmsr_set$design$colorlist[comp_rows]
  groupx <- dpmsr_set$design$Group[comp_rows]
  
  
  if (input$stats_onepeptide_use_zscore) {df_peptide <- peptide_zscore(df_peptide, info_columns)}
  
  df_list <- list("df" = df, "df_peptide" = df_peptide, "df_peptide_stats" = df_peptide_stats, "info_columns" = info_columns, "namex" = namex, "color_list" = color_list, "comp_string" = comp_string)
  #test_df_list <<- df_list
  
  cat(file = stderr(), "One Peptide stats and plots...end" , "\n")
  return(df_list)
}




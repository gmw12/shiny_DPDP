cat(file = stderr(), "Shiny_MVA_Functions.R", "\n")

#-------------------------------------------------------------------------------
stat_create_comp_df <- function(df, stats_comp, comp_number, params, df_design) {
  cat(file = stderr(), "Function - stat_create_comp_df...", "\n")
  source('Shiny_Misc_Functions.R')
  
  #reduce dataframe to data only
  df_info <- df[1:(ncol(df) - params$sample_number)]
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df_N <- df
  df_D <- df
  df_SPQC <- df
  
  # reduce dataframe to samples of interest
  n_loc <- str_to_numlist(stats_comp$N_loc[comp_number])
  d_loc <- str_to_numlist(stats_comp$D_loc[comp_number])
  df_N <- df[, n_loc]
  df_D <- df[, d_loc]
  cat(file = stderr(), stringr::str_c("samples_N = ", length(n_loc)), "\n")
  cat(file = stderr(), stringr::str_c("samples_D = ", length(d_loc)), "\n")

  spqc_loc <- str_to_numlist(stats_comp$SPQC_loc[comp_number])
  df_SPQC <- df[, spqc_loc]
  cat(file = stderr(), stringr::str_c("samples_SPQC = ", length(spqc_loc)), "\n")
  
  #combine into list for next step
  df_list <- list(df_info, df_N, df_D, df_SPQC)
  
  cat(file = stderr(), "Function - stat_create_comp_df...end", "\n")
  return(df_list)
}

#-------------------------------------------------------------------------------
stat_create_comp_missing_df <- function(df, stats_comp, comp_number, params, df_design) {
  cat(file = stderr(), "Function - stat_create_comp_missing_df...", "\n")
  
  #reduce dataframe to data only
  df_N <- df
  df_D <- df
  df_SPQC <- df
  
  # reduce dataframe to samples of interest
  n_loc <- str_to_numlist(stats_comp$N_loc[comp_number])
  d_loc <- str_to_numlist(stats_comp$D_loc[comp_number])
  df_N <- df[, n_loc]
  df_D <- df[, d_loc]
  cat(file = stderr(), stringr::str_c("samples_N = ", length(n_loc)), "\n")
  cat(file = stderr(), stringr::str_c("samples_D = ", length(d_loc)), "\n")
  
  spqc_loc <- str_to_numlist(stats_comp$SPQC_loc[comp_number])
  df_SPQC <- df[, spqc_loc]
  cat(file = stderr(), stringr::str_c("samples_SPQC = ", length(spqc_loc)), "\n")
  
  #combine info and samples to dataframe for stats later
  df_out <- list(df_N, df_D, df_SPQC)
  
  cat(file = stderr(), "Function - stat_create_comp_missing_df...end", "\n")
  return(df_out)
}

#----------------------------------------------------------------------------------

precursor_refilter <- function(df_list, df_missing_list, params) {
  cat(file = stderr(), "Function - precursor_refilter...", "\n")
  
  source('Shiny_MVA_Functions.R')
  source('Shiny_Stats_Functions.R')
  
  #unpack lists to df's
  df_info <- df_list[[1]]
  df_N <- df_list[[2]]
  df_D <- df_list[[3]]
  df_N_missing <- df_missing_list[[1]]
  df_D_missing <- df_missing_list[[2]]
  
  if (params$comp_spqc != ""){
    df_SPQC <- df_list[[4]]
    df_SPQC_missing <- df_missing_list[[3]]
    df <- cbind(df_info, df_N, df_D, df_SPQC)
    df_missing <- cbind(df_N_missing, df_D_missing, df_SPQC_missing)
  }else {
    df <- cbind(df_info, df_N, df_D)
    df_missing <- cbind(df_N_missing, df_D_missing)
  }
  
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
  
  if (length(filtered_rows) >0) {
    cat(file = stderr(), stringr::str_c("Fitering ", length(filtered_rows), " rows"), "\n")
    df <- df[-filtered_rows,]
    df_missing <- df_missing[-filtered_rows,]
  }
  # df_missing <- reduce_imputed_df(df_missing)
  # sample_cols <- ncol(df_N) + ncol(df_D) + ncol(df_SPQC)
  # df <- tibble::add_column(df, df_missing, .after = (ncol(df) - sample_cols))

  return(list(df, df_missing))
  
  cat(file = stderr(), "Function - precursor_refilter...end", "\n")
}

#-------------------------------------------------------------------------------
precursor_refilter_rollup <- function(df_filter_list, df_design, params) {
  cat(file = stderr(), "Function - precursor_refilter_rollup...", "\n")
  
  source('Shiny_Rollup.R')
  source('Shiny_Rollup_Functions.R')
  source('Shiny_MVA_Functions.R')
  
  #save(df_filter_list, file="y1"); save(df_design, file="y2"); 
  #  load(file="y1"); load(file="y2"); 
  
  
  #unpack lists to df's
  df <- df_filter_list[[1]]
  df_missing <- df_filter_list[[2]]
  
  #count samples
  sample_count <- ncol(df_missing)
  info_columns <- ncol(df) - sample_count
  
  if (params$data_output == "Protein") {
    #need to add back info columns to missing for rollup
    df_missing_info <- df |> dplyr::select(contains(c("Accession", "Description", "Name", "Genes"))) |> 
      dplyr::mutate(Precursors = 1, .after = Genes)
    
    df_missing <- cbind(df_missing_info, df_missing)
    
    #rollup data and missing
    df <- rollup_selector(df, df_design, params, sample_count)
    df_missing <- rollup_sum(df_missing)
    
    #resort because maxlfq sorts data, while summing does not...
    df <- df[order(df$Accession), ]
    df_missing <- df_missing[order(df_missing$Accession), ]
    row.names(df) <- NULL
    row.names(df_missing) <- NULL
    
    #add missing column to data
    df_missing <- df_missing[, (ncol(df_missing) - sample_count + 1):ncol(df_missing)]
    df_missing_summary <- reduce_imputed_df(df_missing)
    df <- tibble::add_column(df, df_missing_summary, .after = (ncol(df) - sample_count))
    
  }else{
    df_list <- collapse_precursor_ptm_raw(df, sample_count, info_columns, stats=FALSE, add_miss=TRUE, df_missing, params)
    df <- df_list[[1]]
    df_missing <- df_list[[2]]
  }

  cat(file = stderr(), "Function - precursor_refilter_rollup...end", "\n")  
  return(list(df, df_missing))
}


#-------------------------------------------------------------------------------
stat_add <- function(df, df_missing, params, comp_number, stats_comp, df_design) {
  cat(file = stderr(), "Function - stat_add...", "\n")
  
  #save(df, file="z1"); save(df_missing, file="z2"); save(comp_number, file="z3"); save(stats_comp, file="z4"); save(df_design, file="z5");
  #load(file="z1");  load(file="z2");load(file="z3"); load(file="z4"); load(file="z5")
  
  source("Shiny_Misc_Functions.R")
  source("Shiny_Stats_Functions.R")
  
  if (stats_comp$SPQC[comp_number] != 0) {
    sample_count <- as.numeric(stats_comp$N[comp_number]) + as.numeric(stats_comp$D[comp_number]) + as.numeric(stats_comp$SPQC[comp_number])
    df_samples <- df[,(ncol(df) - sample_count + 1):ncol(df)]
  }else {
    sample_count <- as.numeric(stats_comp$N[comp_number]) + as.numeric(stats_comp$D[comp_number])
    df_samples <- df[,(ncol(df) - sample_count + 1):ncol(df)] 
  }
  
  cat(file = stderr(), "stat_add...1", "\n")
  #fix sample names
  sample_names <- c(df_design$Header2[str_to_numlist(stats_comp$N_loc[comp_number])], 
                    df_design$Header2[str_to_numlist(stats_comp$D_loc[comp_number])])
  
  if (stats_comp$SPQC[comp_number] != 0) {
    sample_names <- c(sample_names, df_design$Header2[str_to_numlist(stats_comp$SPQC_loc[comp_number])])
  }
  
  colnames(df)[(ncol(df) - sample_count + 1):ncol(df)] <- sample_names
  colnames(df_samples) <- sample_names
  
  cat(file = stderr(), "stat_add...2", "\n")
  
  n_start <- 1
  n_end <- as.numeric(stats_comp$N[comp_number])
  
  d_start <- n_end + 1
  d_end <- d_start + as.numeric(stats_comp$D[comp_number]) -1
  
  df_N <- df_samples[,n_start:n_end]
  df_D <- df_samples[,d_start:d_end]
  
  if (stats_comp$SPQC[comp_number] != 0) {
    spqc_start <- d_end + 1
    spqc_end <- spqc_start + as.numeric(stats_comp$SPQC[comp_number]) - 1
    df_SPQC <- df_samples[,spqc_start:spqc_end]
  }
  
  
  if (params$raw_data_format != "protein"){
    df_N_missing <- df_missing |> dplyr::select(starts_with(stats_comp$FactorsN[comp_number]))
    df_D_missing <- df_missing |> dplyr::select(starts_with(stats_comp$FactorsD[comp_number]))
  }
  
  
  cat(file = stderr(), "stat_add...3", "\n")
  df[[stringr::str_c(stats_comp$FactorsN[comp_number], "_CV")]] <- percentCV_gw(df_N)
  df[[stringr::str_c(stats_comp$FactorsD[comp_number], "_CV")]] <- percentCV_gw(df_D)
  if (stats_comp$SPQC[comp_number] != 0) {
    df[["SPQC_CV"]] <- percentCV_gw(df_SPQC)
  }
  
  cat(file = stderr(), "stat_add...4", "\n")
  df[[stringr::str_c(stats_comp$Name[comp_number], "_FC")]] <- foldchange_gw(df_N, df_D, params)
  df[[stringr::str_c(stats_comp$Name[comp_number], "_FC2")]] <- foldchange_decimal_gw(df_N, df_D, params)
  df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] <- pvalue_gw(df_N, df_D, params)
  
  cat(file = stderr(), "stat_add...5", "\n")
  if (params$checkbox_adjpval) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_adjpval")]] <- p.adjust(df[[stringr::str_c(stats_comp$Name[comp_number], "_pval")]] , method = params$padjust_options) 
  }
  
  if (params$checkbox_cohensd) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_cohensd")]] <-  cohend_gw(df_N, df_D, as.logical(params$checkbox_cohensd_hedges))
  }

  if (params$checkbox_limmapvalue) {
    df[[stringr::str_c(stats_comp$Name[comp_number], "_limma")]] <- limma_gw(df_N, df_D)
  }
  
  cat(file = stderr(), "stat_add...6", "\n")
  if (params$raw_data_format != "protein"){
    df[[stringr::str_c(stats_comp$Name[comp_number], "_mf")]] <- missing_factor_gw(df_N_missing, df_D_missing)
    
    # Final filters
    #missing filter, always on...
    filtered_mf <- which(df[[stringr::str_c(stats_comp$Name[comp_number], "_mf")]] < params$missing_factor)  
    if(length(filtered_mf) > 0 ){ df <- df[-(filtered_mf),] }
  }
  
  if(params$stats_spqc_cv_filter & stats_comp$SPQC[comp_number] != 0) {
    filtered_spqc <- which(df[[stringr::str_c(params$comp_spqc, "_CV")]] > params$stats_spqc_cv_filter_factor)  
    if(length(filtered_spqc) > 0 ){ df <- df[-(filtered_spqc),] }
  }
  
  cat(file = stderr(), "stat_add...7", "\n")
  if(params$stats_comp_cv_filter) {
    filtered_onegroup <- which(df[[stringr::str_c(stats_comp$FactorsN[comp_number], "_CV")]] > params$stats_comp_cv_filter_factor & 
                                 df[[stringr::str_c(stats_comp$FactorsD[comp_number], "_CV")]] > params$stats_comp_cv_filter_factor)  
    if(length(filtered_onegroup) > 0 ){ df <- df[-(filtered_onegroup),]}
  }
  
  if(params$stats_peptide_minimum) {
    filtered_min_peptide <- which(df$Precursors < params$stats_peptide_minimum_factor)
    if(length(filtered_min_peptide) > 0 ){ df <- df[-(filtered_min_peptide),] }
  }
  
  cat(file = stderr(), "stat_add...8", "\n")
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
  
  cat(file = stderr(), "stat_add...9", "\n")
  df$Stats[filtered_up] <- "Up"
  df$Stats[filtered_down] <- "Down"
  
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
  
  cat(file = stderr(), "function create_imputed_df....1", "\n")
  df_protein_info <- df |> dplyr::select(contains(c("Accession", "Description","Name", "Genes")))
  df <- df[(info_columns + 1):ncol(df)]
  
  df[df > 0] <- 1
  df[is.na(df)] <- 0
  
  cat(file = stderr(), "function create_imputed_df....2", "\n")
  df_protein <- cbind(df_protein_info, df)
  df_protein <- df_protein |> dplyr::group_by(Accession, Description, Name, Genes) |> dplyr::summarise_all(list(sum))
  df_protein <- data.frame(dplyr::ungroup(df_protein))
  df_protein <- df_protein[5:ncol(df_protein)]
  
  cat(file = stderr(), "function create_imputed_df....3", "\n")
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
  source("Shiny_Misc_Functions.R")
  source("Shiny_File.R")
  
  df_missing <- read_table_try(table_name, params)
  
  #select samples in comparison
  df_missing <- df_missing[, c(str_to_numlist(stats_comp$N_loc[comp_number]), str_to_numlist(stats_comp$D_loc[comp_number]), str_to_numlist(stats_comp$SPQC_loc[comp_number]))]
  
  df_missing <-  reduce_imputed_df(df_missing)
  
  df <- tibble::add_column(df, df_missing, .after = (ncol(df) - params$sample_number))
  cat(file = stderr(), "function add_imputed_df...end", "\n")
  return(list(df, df_missing))
}


# data table filter ------------------------------------------------------

stats_data_table_filter <- function(df, sample_number, start_sample_col, input_stats_add_filters, input_stats_search_field,
                                    input_stats_data_description) {
  cat(file = stderr(), "Function stats_data_table_filter...", "\n")
  
  if (dplyr::is.grouped_df(df)) {df <- ungroup(df)}
  
  if (input_stats_add_filters) {
    df <- df |> dplyr::filter(df$Stats == "Up" | df$Stats == "Down")
  }
  
  cat(file = stderr(), "stats_data_table_filter... 1", "\n")
  
  if (input_stats_search_field == "topn" & input_stats_data_description != "" ) {
    df$sum <- rowSums(df[start_sample_col:(start_sample_col + sample_number -1)])
    df <- df[order(-df$sum),]                      
    df <- df[1:as.numeric(input_stats_data_description),]
    df$sum <- NULL
  }
  
  cat(file = stderr(), "stats_data_table_filter... 2", "\n")
  
  if (input_stats_search_field == "accession" & input_stats_data_description != "" ) {
    df <- df[grep(as.character(input_stats_data_description), df$Accession), ]
  }
  
  if (input_stats_search_field == "description" & input_stats_data_description != "" ) {
    df <- df[grep(as.character(input_stats_data_description), df$Description), ]
  }
  
  if (input_stats_search_field == "genes" & input_stats_data_description != "" ) {
    df <- df[grep(as.character(input_stats_data_description), df$Genes), ]
  }
  
  if (input_stats_search_field == "name" & input_stats_data_description != "" ) {
    df <- df[grep(as.character(input_stats_data_description), df$Name), ]
  }
  
  cat(file = stderr(), "stats_data_table_filter... 3", "\n")
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

oneprotein_data <- function(df, df_peptide, input_stats_oneprotein_plot_comp, input_stats_oneprotein_accession, 
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
  df <- df |> filter(stringr::str_detect(Modifications, grep_mod) )
  
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


#------------------------------------------------------------------------------------------------------------------------------------
peptide_position_lookup <- function(df_peptide, params)  {
  cat(file = stderr(), "function peptide_position_lookup...", "\n")
  source('Shiny_File.R')
  
  #save(df_peptide, file="zzpeptideposlookup") #    load(file="zzpeptideposlookup")

  if (params$data_source == "PD" & params$raw_data_format =="precursor") {
    # create peptide lookup table
    peptide_pos_lookup <- df_peptide |> dplyr::select("Accession", "Sequence")
    peptide_raw <- read_table_try("peptide_raw", params)
    peptide_pos_lookup <- merge(peptide_raw, peptide_pos_lookup, by =(c("Accession", "Sequence")) )

    cat(file = stderr(), "peptide_position_lookup...1", "\n")
    peptide_pos_lookup$Position <- gsub("\\]; \\[", "xxx",  peptide_pos_lookup$Position)
    s <- strsplit(peptide_pos_lookup$Position, split = "; ")
    peptide_pos_lookup <- data.frame(Sequence = rep(peptide_pos_lookup$Sequence, sapply(s, length)), Position = unlist(s))
    peptide_pos_lookup$Count <- stringr::str_count(peptide_pos_lookup$Position, "xxx")
    peptide_pos_lookup <- peptide_pos_lookup |> tidyr::separate(Position, c("Accession", "Start_Stop"), sep = " ")
    peptide_pos_lookup$Start_Stop <- gsub("\\[", "",  peptide_pos_lookup$Start_Stop)
    peptide_pos_lookup$Start_Stop <- gsub("\\]", "",  peptide_pos_lookup$Start_Stop)
    peptide_pos_lookup$Start_Stop <- gsub("xxx", ", ",  peptide_pos_lookup$Start_Stop)
    peptide_pos_lookup$AS <- stringr::str_c(peptide_pos_lookup$Accession, " ", peptide_pos_lookup$Sequence)
    
    cat(file = stderr(), "peptide_position_lookup...2", "\n")
    s <- strsplit(peptide_pos_lookup$Start_Stop, split = ", ")
    peptide_pos_lookup <- data.frame(AS = rep(peptide_pos_lookup$AS, sapply(s, length)), Position = unlist(s))
    peptide_pos_lookup <- peptide_pos_lookup |> tidyr::separate(AS, c("Accession", "Sequence"), sep = " ")
    peptide_pos_lookup <- peptide_pos_lookup |> tidyr::separate(Position, c("Start", "Stop"), sep = "-")
  }
  
  if (params$data_source == "SP") {
    # create peptide lookup table
    if (params$data_output == "Protein") {
      peptide_pos_lookup <- df_peptide |> dplyr::select(Accession, Sequence, PeptidePosition)
      peptide_pos_lookup$Sequence <- gsub("_", "", peptide_pos_lookup$Sequence)
      peptide_pos_lookup$Sequence <- gsub("\\[.*?\\]", "", peptide_pos_lookup$Sequence)
    }else{
      peptide_pos_lookup <- df_peptide |> dplyr::select(Accession, Sequence, PeptidePosition)
      peptide_pos_lookup$Sequence <- gsub("_", "", peptide_pos_lookup$Sequence)
      #peptide_pos_lookup$Sequence <- gsub("\\[.*?\\]", "", peptide_pos_lookup$Sequence)
    }

    cat(file = stderr(), "peptide_position_lookup...3", "\n")
    colnames(peptide_pos_lookup) <- c("Accession", "Sequence", "Start")
    peptide_pos_lookup$Start <- gsub("(.*),.*", "\\1", peptide_pos_lookup$Start)
    peptide_pos_lookup$Start <- gsub("(.*);.*", "\\1", peptide_pos_lookup$Start)
    peptide_pos_lookup <- peptide_pos_lookup |> dplyr::distinct()
    peptide_pos_lookup$Stop <- nchar(peptide_pos_lookup$Sequence) + as.numeric(peptide_pos_lookup$Start)
    
  }    
  
  peptide_pos_lookup$dup <- stringr::str_c(peptide_pos_lookup$Accession, peptide_pos_lookup$Sequence, peptide_pos_lookup$Start, peptide_pos_lookup$Stop)
  peptide_pos_lookup <- peptide_pos_lookup[!duplicated(peptide_pos_lookup$dup),]
  peptide_pos_lookup$dup <- NULL
  
  cat(file = stderr(), "function peptide_position_lookup...end", "\n")
  return(peptide_pos_lookup)
  
}

# peptide zscore ------------------------------------------------------
peptide_zscore <- function(df_peptide, start_sample_col_peptide) {
  cat(file = stderr(), "Function peptide_zscore...", "\n")
  #  df_peptide <- df_no_stats; start_sample_col_peptide <- start_sample_col

  info_df <- df_peptide[1:(start_sample_col_peptide - 1)]
  df_data <- df_peptide[(start_sample_col_peptide):ncol(df_peptide)]
  col_names <- colnames(df_data)
  #df_data <- log(df_data, 2)
  df_zscore <- t(data.frame(apply(df_data, 1, scale)))
  colnames(df_zscore) <- col_names
  df_out <- data.frame(cbind(info_df, df_zscore), stringsAsFactors = FALSE)
  
  cat(file = stderr(), "Function peptide_zscore...end", "\n")
  return(df_out)
}
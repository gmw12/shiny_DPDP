cat(file = stderr(), "Shiny_Rollup.R", "\n")
#--------------------------------------------------------------------

rollup_apply <- function(session, input, output, db_path){ 
  cat(file = stderr(), "Function - rollup_apply...", "\n")
  showModal(modalDialog("Apply Rollup...", footer = NULL))
  
  cat(file = stderr(), stringr::str_c("rollup_method = ", get_param('rollup_method', db_path)), "\n")
  
  arg_list <- list(db_path)
  bg_rollup_apply <- callr::r_bg(func = rollup_apply_bg, args = arg_list, stderr = stringr::str_c(get_param('error_path', db_path), "//error_rollup_bg.txt"), supervise = TRUE)
  bg_rollup_apply$wait()
  print_stderr("error_rollup_bg.txt", db_path)

  removeModal()
}

#--------------------------------------------------------------------

rollup_apply_bg <- function(db_path) {
  cat(file = stderr(), "Function - rollup_apply_bg...", "\n")
  
  source("Shiny_Rollup_Functions.R") 
  source("Shiny_File.R")
  
  params <- get_params(db_path)
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  df_design <- RSQLite::dbReadTable(conn, "design")
  
  for (norm in norm_type) {
    norm <- stringr::str_replace_all(norm, " ", "")
    table_name <- stringr::str_c("precursor_impute_", norm)
    df <- RSQLite::dbReadTable(conn, table_name)

    if(params$data_output == "Protein") {
      cat(file = stderr(), "Output type is protein", "\n")
      table_name_out <- stringr::str_c("protein_", norm)
      table_name_out_peptide <- stringr::str_c("peptide_", norm)
      #rollup precursor/peptides
      protein_df <- rollup_selector(df, df_design, params, params$sample_number)
      #rollup precursor to peptide
      peptide_df <- rollup_sum_peptide(df, df_design, comp_number=0, stats_comp = 0, params)
      RSQLite::dbWriteTable(conn, table_name_out, protein_df, overwrite = TRUE)
      RSQLite::dbWriteTable(conn, table_name_out_peptide, peptide_df, overwrite = TRUE)
    }
    
    if(params$data_output == "Peptide" & params$ptm) {
      cat(file = stderr(), "Output type is peptide/ptm", "\n")
      #rollup precursor ptm
      source('Shiny_Rollup.R')
      table_name_out_peptide_ptm <- stringr::str_c("peptide_impute_", norm)
      df_missing <- read_table_try("precursor_missing", db_path)
      peptide_df_list <- collapse_precursor_ptm_raw(df, params$sample_number, info_columns = 0, stats = FALSE, add_miss = TRUE, df_missing, params, db_path)
      peptide_df <- peptide_df_list[[1]]
      RSQLite::dbWriteTable(conn, table_name_out_peptide_ptm, peptide_df, overwrite = TRUE)
    }
    
    if(params$data_output == "Peptide" & !params$ptm) {
      cat(file = stderr(), "Output type is peptide", "\n")
      #rollup precursor ptm
      source('Shiny_Rollup.R')
      table_name_out_peptide <- stringr::str_c("peptide_impute_", norm)
      df_missing <- read_table_try("precursor_missing", db_path)
      peptide_df_list <- collapse_precursor_raw2(df, params$sample_number, info_columns = 0, stats = FALSE, add_miss = TRUE, df_missing, params, db_path)
      peptide_df <- peptide_df_list[[1]]
      RSQLite::dbWriteTable(conn, table_name_out_peptide, peptide_df, overwrite = TRUE)
    }
    
  }
  
  RSQLite::dbDisconnect(conn)
}

#---------------------------------------------------------------------------------------------------------

stat_prep_rollup <- function(session, input, output){ 
  
  cat(file = stderr(), "stat_prep_rollup function...", "\n")
  showModal(modalDialog("Stat prep...", footer = NULL)) 
  stat_prep()
  removeModal()
  cat(file = stderr(), "define final info columns", "\n")
  set_final_info_columns()
  cat(file = stderr(), "qc_apply", "\n")
  showModal(modalDialog("QC stats...", footer = NULL)) 
  qc_apply()
  removeModal()
  cat(file = stderr(), "qc - create_plots", "\n")
  showModal(modalDialog("QC plots...", footer = NULL)) 
  create_qc_plots()
  cat(file = stderr(), "qc - render_plots", "\n")
  qc_render(session, input, output)
  update_widget_post_processing(session, input, output)
  cat(file = stderr(), "qc - save_final_nostats", "\n")
  save_final_nostats()
  removeModal()
}

#--- collapse precursor to peptide-------------------------------------------------------------
collapse_precursor_raw <- function(precursor_data, info_columns = 0, stats = FALSE, params) {
  cat(file = stderr(), "function collapse_precursor_raw...", "\n")
  
  #drop columns 
  precursor_data$PrecursorId <- NULL
  precursor_data$Detected_Imputed <- NULL
  
  if (info_columns == 0) {
    info_columns <- ncol(precursor_data) - params$sample_number
  }
  
  if (stats == TRUE) {
    info_columns = info_columns - 2
  }

  #save sample column number to reset info columns below
  sample_columns <- ncol(precursor_data) - info_columns
  
  columns = names(precursor_data)[1:info_columns]
  
  peptide_data <- precursor_data |>
    dplyr::group_by_at(dplyr::vars(all_of(columns))) |>
    dplyr::summarise_all(list(sum))
  
  peptide_data <- data.frame(dplyr::ungroup(peptide_data))
  
  cat(file = stderr(), "function collapse_precursor_raw...", "\n")
  return(peptide_data)
  
}


#--- collapse precursor to peptide-------------------------------------------------------------
collapse_precursor_raw2 <- function(precursor_data, sample_columns, info_columns, stats, add_miss, df_missing, params, db_path) {
  cat(file = stderr(), "function collapse_precursor_raw...", "\n")
  
  # precursor_data=df; sample_columns=params$sample_number; info_columns = 0; stats = FALSE; add_miss = TRUE;
  # precursor_data=df; stats = FALSE; add_miss = TRUE; sample_columns=sample_count
  
  #drop columns 
  precursor_data$PrecursorId <- NULL
  precursor_data$Detected_Imputed <- NULL
  
  if (info_columns == 0) {
    info_columns <- ncol(precursor_data) - params$sample_number
  }
  
  if (stats == TRUE) {
    info_columns = info_columns - 2
  }
  
  info_columns <- ncol(precursor_data) - sample_columns
  
  columns = names(precursor_data)[1:info_columns]
  
  precursor_data <- tibble::add_column(precursor_data, "Precursors"=1, .after=info_columns)

  peptide_data <- precursor_data |>
    dplyr::group_by_at(dplyr::vars(all_of(columns))) |>
    dplyr::summarise_all(list(sum))
  
  peptide_data <- data.frame(dplyr::ungroup(peptide_data))
  
  
  if (add_miss) {
    cat(file = stderr(), "adding imputed column...", "\n")
    source('Shiny_MVA_Functions.R')
    if(!is.null(df_missing)) {df_missing <- cbind(precursor_data[1:info_columns], df_missing)}
    df_missing_peptide <- df_missing |>
      dplyr::group_by_at(dplyr::vars(all_of(columns))) |> dplyr::summarise_all(list(sum))
    df_missing_peptide <- data.frame(dplyr::ungroup(df_missing_peptide))
    df_missing_peptide <- df_missing_peptide[(info_columns+1):ncol(df_missing_peptide)]
    imputed <- reduce_imputed_df(df_missing_peptide) 
    peptide_data <- tibble::add_column(peptide_data, imputed$Detected_Imputed, .after=(info_columns+1))
    colnames(peptide_data)[info_columns+2] <- "Detected_Imputed"
  }
  
  cat(file = stderr(), "function collapse_precursor_ptm_raw...end", "\n")
  
  if (add_miss) {
    return(list(peptide_data, df_missing_peptide))
  } else {
    return(list(peptide_data, data.frame()))
  }
  
}

#--- collapse precursor to peptide-------------------------------------------------------------
collapse_precursor_ptm_raw <- function(precursor_data, sample_columns, info_columns, stats, add_miss, df_missing, params, db_path) {
  cat(file = stderr(), "function collapse_precursor_ptm_raw...", "\n")
  
  #.   precursor_data <- df; sample_columns <- params$sample_number; info_columns = 0
  # save(precursor_data, file="z1");save(info_columns, file="z2");save(stats, file="z3");save(add_miss, file="z4");save(df_missing, file="z5");save(sample_columns, file="z6")
  #  load(file="z1");load(file="z2");load(file="z3");load(file="z4");load(file="z5");  load(file="z6")
  
  localized_data <- precursor_data |> dplyr::select("Local", "Local2")

  #drop columns 
  col_count <- ncol(precursor_data)
  precursor_data$PrecursorId <- NULL
  precursor_data$Detected_Imputed <- NULL
  precursor_data$ProteinPTMLocations <- NULL
  precursor_data$Local <- NULL
  precursor_data$Local2 <- NULL
  col_count <- col_count - ncol(precursor_data)
  
  if (info_columns == 0) {
    info_columns <- ncol(precursor_data) - sample_columns
  } else {
    info_columns <- info_columns - col_count
  }
  
  if (stats == TRUE) {
    info_columns = info_columns - 2
  }
  
  columns = names(precursor_data)[1:info_columns]
  
  #setting info columns the same so that all rollups group the same way
  localized_data <- cbind(precursor_data$Sequence, localized_data)
  colnames(localized_data) <- c("Sequence", "Local", "Local2")
  precursor_count_df <- precursor_data[,1:info_columns]
  
  if(!is.null(df_missing)) {df_missing <- cbind(precursor_data[1:info_columns], df_missing)}
  
  #rollup data and ungroup
  peptide_data <- precursor_data |>
    dplyr::group_by_at(dplyr::vars(all_of(columns))) |> dplyr::summarise_all(list(sum))
  peptide_data <- data.frame(dplyr::ungroup(peptide_data))
  
  local_rollup_df <- rollup_local(localized_data, params)
  
  local_rollup_df <- local_rollup_df[match(peptide_data$Sequence, local_rollup_df$Sequence),]
  
  peptide_data <- tibble::add_column(peptide_data, "Local2"=local_rollup_df$Local2, .after="Sequence")
  peptide_data <- tibble::add_column(peptide_data, "Local"=local_rollup_df$Local, .after="Sequence")
  
  precursor_count_df$Precusors <- 1
  precursor_count_df <- precursor_count_df |>
    dplyr::group_by_at(dplyr::vars(all_of(columns))) |> dplyr::summarise_all(list(sum))
  Precursors <- precursor_count_df$Precusors
  
  peptide_data <- tibble::add_column(peptide_data, Precursors, .after="Sequence")
    
  if (add_miss) {
    cat(file = stderr(), "adding imputed column...", "\n")
    source('Shiny_MVA_Functions.R')
    df_missing_peptide <- df_missing |>
      dplyr::group_by_at(dplyr::vars(all_of(columns))) |> dplyr::summarise_all(list(sum))
    df_missing_peptide <- data.frame(dplyr::ungroup(df_missing_peptide))
    df_missing_peptide <- df_missing_peptide[(info_columns+1):ncol(df_missing_peptide)]
    imputed <- reduce_imputed_df(df_missing_peptide) 
    peptide_data <- tibble::add_column(peptide_data, imputed$Detected_Imputed, .after=(info_columns+2))
    colnames(peptide_data)[info_columns+3] <- "Detected_Imputed"
  }
  
  cat(file = stderr(), "function collapse_precursor_ptm_raw...end", "\n")
  
  if (add_miss) {
    return(list(peptide_data, df_missing_peptide))
  } else {
    return(list(peptide_data, data.frame()))
  }
}

#------------------------------------------------------------
rollup_local <- function(localized_data, params) {
  cat(file = stderr(), "Function rollup_local...", "\n")
  
  #save(localized_data, file="zz1")
  # load(file="zz1")
  
  str_to_numlist <- function(str_in) {
    num_out <- strsplit(str_in, ",") |> unlist() |> as.numeric()
    return(num_out)
  }
  
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  local_unique <- data.frame(unique(localized_data$Sequence))
  local_unique$Local <- ""
  local_unique$Local2 <- ""
  colnames(local_unique) <- c("Sequence", "Local", "Local2")
  
  #for (i in (1:nrow(local_unique))) {
  parallel_result <- foreach(i = 1:nrow(local_unique), .combine = rbind) %dopar% { 
    if(grepl("Phospho", local_unique$Sequence[i])) {
      
      test_df <- localized_data[localized_data$Sequence == local_unique$Sequence[i],]
      
      if(nrow(test_df) > 1) {
        first_value <- TRUE
        for (r in (1:nrow(test_df))) {
          if (first_value) { 
            temp1 <- str_to_numlist(test_df$Local[r])
            if (!is.na(temp1[[1]])) {
              first_value <- FALSE
            }
          }else {
            temp2 <- str_to_numlist(test_df$Local[r])
            if (!is.na(temp2[[1]])) {
              temp1 <- pmax(temp1, temp2)
            }
          }
        }
      }else {
        temp1 <- str_to_numlist(test_df$Local[1])
      }
      
      if (max(temp1) >= params$ptm_local) {
        if (min(temp1) >= params$ptm_local) {
          local2 <- "Y"
        } else {
          local2 <- "P"
        }
      }else {
        local2 <- "N"
      }
      
      #local_unique$Local[i] <- list(temp1) 
      #local_unique$Local2[i] <- local2
      
    }else {
      temp1 <- ""
      local2 <- ""
    }
    
    #local_unique$Local[i] <- list(temp1) 
    #local_unique$Local2[i] <- local2
    list(temp1, local2)
  }
  
  stopCluster(cl) 
  
  parallel_result <- data.frame(parallel_result)
  row.names(parallel_result) <- NULL
  parallel_result <- cbind(local_unique$Sequence, parallel_result)
  colnames(parallel_result) <- c("Sequence", "Local", "Local2")
  
  numlist_to_string <- function(x) {
    return(toString(paste(unlist(x$Local) |> as.character() |> paste(collapse = ","))))
  }
  
  numlist_to_string2 <- function(x) {
    return(toString(paste(unlist(x$Local2) |> as.character() |> paste(collapse = ","))))
  }
  
  parallel_result$Local <- apply(parallel_result, 1, numlist_to_string)
  parallel_result$Local2 <- apply(parallel_result, 1, numlist_to_string2)
  
  cat(file = stderr(), "Function rollup_local...end", "\n")
  return(parallel_result)
}















































#--- collapse peptide to protein-------------------------------------------------------------
collapse_peptide <- function(peptide_data, info_columns=0, stats=FALSE, impute=FALSE){
  
  cat(file = stderr(), "starting collapse_peptide...", "\n")
  
  if (info_columns == 0) {
    info_columns <- ncol(peptide_data) - dpmsr_set$y$sample_number
  }
  
  #save sample column number to reset info columns below
  sample_columns <- ncol(peptide_data) - info_columns
  
  peptide_data <- collapse_peptide_setup(peptide_data, info_columns)
  #reset info_columns after function call (will reduce info columns to 3 or 4)
  info_columns <- ncol(peptide_data) - sample_columns
  
  if (!impute) {
    if (dpmsr_set$x$rollup_method == "Sum") {
      protein_data <- rollup_sum(peptide_data, info_columns)
    }else if (dpmsr_set$x$rollup_method == "Median") {
      protein_data <- rollup_median(peptide_data, info_columns)
    }else if (dpmsr_set$x$rollup_method == "Median_Polish") {
      protein_data <- rollup_median_polish(peptide_data, info_columns)
    }else if (dpmsr_set$x$rollup_method == "Mean") {
      protein_data <- rollup_mean(peptide_data, info_columns)
    }else if (dpmsr_set$x$rollup_method == "IQ_MaxLFQ") {
      protein_data <- rollup_maxlfq(peptide_data, info_columns)
    }else if (dpmsr_set$x$rollup_method == "TopN") {
      protein_data <- rollup_topn(peptide_data, info_columns)
    }else if (dpmsr_set$x$rollup_method == "DirectLFQ") {
      protein_data <- rollup_directlfq(peptide_data, info_columns)
    }
  }else{
    protein_data <- rollup_sum(peptide_data, info_columns)
  }
  
  protein_data <- data.frame(ungroup(protein_data))
  
  #add imputed column info
  if (!stats) {
    cat(file = stderr(), "collapse peptide/precursor... 4", "\n")
    if ((dpmsr_set$x$raw_data_input == "Protein_Peptide" || dpmsr_set$x$raw_data_input == "Peptide") 
        && dpmsr_set$x$final_data_output == "Protein" && !as.logical(dpmsr_set$x$tmt_spqc_norm)   )
    {
      cat(file = stderr(), "collapse peptide to protein... 4.1", "\n")
      protein_data <- add_column(protein_data, dpmsr_set$data$protein_missing, .after = "Peptides")
      #dpmsr_set$y$info_columns_final <<- ncol(protein_data)-dpmsr_set$y$sample_number
      dpmsr_set$y$info_columns_final <<- ncol(protein_data) - sample_columns
      names(protein_data)[grep("Peptides", colnames(protein_data)) + 1] <- "Detected_Imputed"
    }
    
    if (dpmsr_set$x$raw_data_input == "Precursor" && dpmsr_set$x$final_data_output == "Protein" && !as.logical(dpmsr_set$x$tmt_spqc_norm))
    {
      cat(file = stderr(), "collapse to protein... 4.2", "\n")
      protein_data <- add_column(protein_data, dpmsr_set$data$protein_missing, .after = "Peptides")
      #dpmsr_set$y$info_columns_final <<- ncol(protein_data)-dpmsr_set$y$sample_number
      dpmsr_set$y$info_columns_final <<- ncol(protein_data) - sample_columns
      names(protein_data)[grep("Peptides", colnames(protein_data)) + 1] <- "Detected_Imputed"
    }
    
  }
  
  cat(file = stderr(), "finished collapse_peptide...", "\n")
  return(protein_data) 
  
  
}


#-------------------------------------------------------------------------------------------

collapse_peptide_setup <- function(peptide_data, info_columns, directlfq=FALSE){
  
  #test_peptide_data <<- peptide_data; test_info_columns <<- info_columns; 
  #peptide_data <- test_peptide_data; info_columns <- test_info_columns
  
  cat(file = stderr(), "starting collapse_peptide_setup...", "\n")
  
  peptide_annotate <- peptide_data[1:info_columns]
  peptide_data <- peptide_data[(info_columns + 1):ncol(peptide_data)]
  peptide_data[is.na(peptide_data)] <- 0
  
  # Issue with previous version of dpmsr_set file not have unique peptide information
  if ("Unique" %in% colnames(peptide_annotate))
  {
    cat(file = stderr(), "Unique column found in peptide data...", "\n")
    peptide_annotate <- peptide_annotate[, c("Accession", "Description", "Genes", "Unique")]
  }else{
    cat(file = stderr(), "Unique column NOT found in peptide data...", "\n")
    peptide_annotate <- peptide_annotate[, c("Accession", "Description", "Genes")]
  }
  
  
  #count number of peptides for each protein
  peptide_annotate$Peptides <- 1
  peptide_annotate$Peptides <- as.numeric(peptide_annotate$Peptides)
  
  
  # Issue with previous version of dpmsr_set file not have unique peptide information
  if ("Unique" %in% colnames(peptide_annotate))
  {
    #count number of unique peptides for each protein
    peptide_annotate$Unique[peptide_annotate$Unique == "Unique"] <- 1
    peptide_annotate$Unique[peptide_annotate$Unique != 1] <- 0
    peptide_annotate$Unique <- as.numeric(peptide_annotate$Unique)
    peptide_annotate <- peptide_annotate[, c("Accession", "Description", "Genes", "Peptides", "Unique")]
  }else{
    peptide_annotate <- peptide_annotate[, c("Accession", "Description", "Genes", "Peptides")]
  }
  
  #combine data
  peptide_data <- cbind(peptide_annotate, peptide_data)
  
  cat(file = stderr(), "finished collapse_peptide_setup...", "\n")
  return(peptide_data)
} 
















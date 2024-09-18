cat(file = stderr(), "load Shiny_Filter.R", "\n")

#-------------------------------------------------------------------------------------------#----------------------------------------------------------------------------------------
filter_data <- function(session, input, output){
  cat(file = stderr(), "Function - filter_data...", "\n")
  showModal(modalDialog("Applying data filters...", footer = NULL))

  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "preprocess filter precursor...", "\n")
    bg_filter <- callr::r_bg(func = filter_data_bg, args = list("precursor_noise", "precursor_filter", params), stderr = str_c(params$error_path, "//error_filter.txt"), supervise = TRUE)
    bg_filter$wait()
    print_stderr("error_filter.txt")
  } 
  
  cat(file = stderr(), "Function - filter_data...end", "\n\n")
  removeModal()
}

#----------------------------------------------------------------------------------------
# create column to flag and delete rows with no data or only one data value
# require row to contain at least one group that has at least 2 values
filter_data_bg <- function(table_name, new_table_name, params){
  cat(file = stderr(), "Function - filter_data_bg...", "\n")
  source("Shiny_Filter.R")
  
  start <- Sys.time()
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  sample_groups <- RSQLite::dbReadTable(conn, "sample_groups")


  info_columns <- ncol(df) - params$sample_number
  total_columns <- ncol(df)
  df[df == 0] <- NA
  
  # Step 1 remove peptides/proteins below minimum count requirement overall 
  cat(file = stderr(), "step 1, remove below minimum...", "\n")
  minimum_filter <- function(df, info_columns, params) {
    df$measured_count <- apply(df[, (info_columns + 1):ncol(df)], 1, function(x) sum(!is.na(x)))
    df <- subset(df, df$measured_count >= params$filter_min_measured_all)
    df <- df[,1:total_columns]
    return(df)
  }
  df <- minimum_filter(df, info_columns, params)
  
  # Step 2 remove peptides/proteins below minimum count requirement in groups 
  cat(file = stderr(), "step 2, remove below minimum group requirement...", "\n")
  filter_min_group <- function(df, sample_groups, info_columns, params) {
    for (i in 1:params$group_number) {
      df$na_count <- apply(df[, (sample_groups$start[i] + info_columns):(sample_groups$end[i] + info_columns)], 1, function(x) sum(!is.na(x)))
      df$na_count <- df$na_count / sample_groups$Count[i]
      colnames(df)[colnames(df) == "na_count"] <- sample_groups$Group[i]
    }
    df$max <- apply(df[, (total_columns + 1):(ncol(df))], 1, function(x) max(x))
    df <- subset(df, df$max >= params$filter_x_percent_value)
    df <- df[1:total_columns]
    return(df)
  }
  
  if (params$filter_x_percent) {
    filter_min_group(df, sample_groups, info_columns, params)
  }
  
  #Step 3 optional misaligned filter
  cat(file = stderr(), "step 3, misaligned filter...", "\n")
  misaligned_count <- 0
  misaligned_rows_total <- NULL
  
  if (params$checkbox_misaligned) {
    cat(file = stderr(), "setting misaligned to NA...", "\n")
    
    #function to find misalignments
    test_alignment <- function(x) {
      misaligned <- FALSE
      missing <- sum(is.na(x))/length(x) * 100
      if (missing > params$misaligned_cutoff && missing < 100) {
        if (mean(x, na.rm = TRUE) >= params$intensity_cutoff) {
          misaligned <- TRUE
        }
      }
      return(misaligned)
    }
    
    for (i in 1:nrow(sample_groups)) {
      temp_df <- df[,(sample_groups$start[i] + info_columns):(sample_groups$end[i] + info_columns)] 
      test <- apply(temp_df, 1, test_alignment )
      misaligned_rows <- which(test == TRUE)
      misaligned_rows_total <- c(misaligned_rows_total, misaligned_rows)
      misaligned_count = misaligned_count + length(misaligned_rows)
      temp_df[misaligned_rows, ] <- NA
      df[,(sample_groups$start[i] + info_columns):(sample_groups$end[i] + info_columns)] <- temp_df
    } 
    
    cat(file = stderr(), stringr::str_c("Misaligned rows --> ", misaligned_count), "\n")
        
    misaligned_rows_total <- unique(misaligned_rows_total)
    if (params$misaligned_target == "dataset") {
      cat(file = stderr(), stringr::str_c("Misaligned dataset rows --> ", length(misaligned_rows_total), "\n"))
      df <- df[-misaligned_rows_total,]
      }
    
  }
  
  
  # Step 4, reapply first 2 filters in case misalignment filter creates new canidates 
  cat(file = stderr(), "step 1,2 repeat, remove below minimum group requirement...", "\n")
  df <- minimum_filter(df, info_columns, params)
  if (params$filter_x_percent) {
    filter_min_group(df, sample_groups, info_columns, params)
  }
  
  #Step 5 optional filter by cv of specific sample group
  cat(file = stderr(), "step 4, cv minimum...", "\n")
  if (params$filter_cv) {
    cat(file = stderr(), stringr::str_c("filter by cv"), "\n")

    #Percent CV ---------------------------------
    percentCV <- function(x) {
      ave <- rowMeans(x)
      n <- ncol(x)
      sd <- apply(x[1:n], 1, sd)
      cv <- (100 * sd / ave)
      return(signif(cv, digits = 3))
    }
    
    start <- info_columns + sample_groups$start[sample_groups$Group == params$filter_cv_group]
    end <- info_columns + sample_groups$end[sample_groups$Group == params$filter_cv_group]
    df$filterCV <- percentCV(df[start:end])
    df <- subset(df, df$filterCV < params$filter_cv_value)
    df <- df[-ncol(df)]
  
  }
  
  
  # Step 5, precursor quality filter 
  cat(file = stderr(), "step 5 - Precursor Quality Filter...", "\n")
  if (params$precursor_quality) {
    df <- precursor_quality(df, params)
  }
  
  cat(file = stderr(), "step 6 - remove_duplicates...", "\n")
  
  if (params$data_source == "PD") {
    df$Modifications[is.na(df$Modifications)] <- ""
    df$dup <- stringr::str_c(df$Sequence, "_", df$Modifications)
    df <- dplyr::distinct(df, dup, .keep_all = TRUE)
    df$dup <- NULL
  }
  
  if (params$data_source == "SP") {
    df <- dplyr::distinct(df, PrecursorId, .keep_all = TRUE)
  }
  
  cat(file = stderr(), "step 6 - create missing.values table for impute page...", "\n")
  df_samples <- df[(info_columns + 1):ncol(df)]
  missing.values <- df_samples |>
    tidyr::gather(key = "key", value = "val") |>
    dplyr::mutate(is.missing = is.na(val)) |>
    dplyr::group_by(key, is.missing) |>
    dplyr::summarise(num.missing = dplyr::n()) |>
    dplyr::filter(is.missing == T) |>
    dplyr::select(-is.missing) |>
    dplyr::arrange(desc(num.missing)) 
  RSQLite::dbWriteTable(conn, "missing_values", missing.values, overwrite = TRUE)
  
  cat(file = stderr(), "step 7 - create missing.values2 table for impute page plots...", "\n")
  missing.values2 <- df_samples |>
    tidyr::gather(key = "key", value = "val") |>
    dplyr::mutate(isna = is.na(val)) |>
    dplyr::group_by(key) |>
    dplyr::mutate(total = dplyr::n()) |>
    dplyr::group_by(key, total, isna) |>
    dplyr::summarise(num.isna = dplyr::n()) |>
    dplyr::mutate(pct = num.isna / total * 100)
  
  RSQLite::dbWriteTable(conn, "missing_values_plots", missing.values2, overwrite = TRUE)
  
  cat(file = stderr(), "step 8 - write data to db...", "\n")
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), stringr::str_c("filter_data completed in ", Sys.time() - start), "\n")
  return()
}

#----------------------------------------------------------------------------------

remove_duplicates <- function(data_in){
  cat(file = stderr(), "remove_duplicates...", "\n")
  
  if (dpmsr_set$x$data_source == "PD") {
    data_in$Modifications[is.na(data_in$Modifications)] <- ""
    data_in$dup <- str_c(data_in$Sequence, "_", data_in$Modifications)
    data_out <- distinct(data_in, dup, .keep_all = TRUE)
    data_out$dup <- NULL
  }
  
  if (dpmsr_set$x$data_source == "SP") {
    ##test_data_in <<- data_in
    data_out <- distinct(data_in, PrecursorId, .keep_all = TRUE)
  }
  
  return(data_out)
}
  
  #----------------------------------------------------------------------------------
  
  precursor_quality <- function(df, params){
    cat(file = stderr(), "Function precusor_quality...", "\n")
    
    protein_list <- unique(df$Accession)
    
    require(foreach)
    require(doParallel)
    cores <- detectCores()
    cl <- makeCluster(cores - 2)
    registerDoParallel(cl)
    
    bad_df <- foreach(protein = protein_list, .combine = rbind) %dopar% {
      df2 <- df[df$Accession==protein,]
      
      bad_list <- NULL
      bad_result <- NULL
      bad_average <- NULL
      if (nrow(df2) >= 3) {
        df3 <- df2[,(ncol(df2)-params$sample_number+1):ncol(df2)]
        row.names(df3) <- df2$PrecursorId
        
        df3$average <- rowMeans(df3, na.rm = TRUE)
        df3 <- df3[order(df3$average, decreasing=TRUE),]
        prec_average <- unlist(df3$average)
        df3$average <- NULL
        prec_rows <- min(2, nrow(df3) - 2) 
        for (i in (1:nrow(df3))) {
          if (prec_average[i] < params$precursor_quality_intensity) { break }
          prec_sums <- colSums(df3, na.rm = TRUE)
          prec_ratio <- unlist(df3[1,]) / prec_sums 
          test <- sd(prec_ratio, na.rm = TRUE)/mean(prec_ratio, na.rm = TRUE) * 100
          if (test > params$precursor_quality_sd) {
            bad_list <- c(bad_list, row.names(df3)[1])
            bad_result <- c(bad_result, test)
            bad_average <- c(bad_average, prec_average[i])
          }
          df3 <- df3[-1,]
        }
      }
      
      temp_df <- data.frame(cbind(bad_list, bad_result, bad_average))
      temp_df

    }
    stopCluster(cl) 
    
    df_final <- df[!(df$PrecursorId %in% bad_df$bad_list),]
    
    cat(file = stderr(), "Function precusor_quality...end", "\n")
    return(df_final)
  } 
  
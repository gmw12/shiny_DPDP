cat(file = stderr(), "load Shiny_Filter.R", "\n")

#-------------------------------------------------------------------------------------------#----------------------------------------------------------------------------------------
filter_data <- function(session, input, output){
  cat(file = stderr(), "Function - filter_data...", "\n")

  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "preprocess filter precursor...", "\n")
    bg_filter <- callr::r_bg(func = filter_data_bg, args = list("precursor_noise", "precursor_filter", params), stderr = str_c(params$error_path, "//error_filter.txt"), supervise = TRUE)
    bg_filter$wait()
    print_stderr("error_filter.txt")
  } 
  
}

#----------------------------------------------------------------------------------------
# create column to flag and delete rows with no data or only one data value
# require row to contain at least one group that has at least 2 values
filter_data_bg <- function(table_name, new_table_name, params){
  cat(file = stderr(), "Function - filter_data_bg...", "\n")
  
  start <- Sys.time()
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  sample_groups <- RSQLite::dbReadTable(conn, "sample_groups")


  info_columns <- ncol(df) - params$sample_number
  total_columns <- ncol(df)
  df[df == 0] <- NA
  
  #Step 1 optional misaligned filter
  cat(file = stderr(), "step 1, misaligned filter...", "\n")
  
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
    
    misaligned_count <- 0
    for (i in 1:nrow(sample_groups)) {
      temp_df <- df[,(sample_groups$start[i] + info_columns):(sample_groups$end[i] + info_columns)] 
      test <- apply(temp_df, 1, test_alignment )
      misaligned_rows <- which(test == TRUE)
      misaligned_count = misaligned_count + length(misaligned_rows)
      temp_df[misaligned_rows, ] <- NA
      df[,(sample_groups$start[i] + info_columns):(sample_groups$end[i] + info_columns)] <- temp_df
    } 
    
    cat(file = stderr(), stringr::str_c("Misaligned rows --> ", misaligned_count), "\n")
  }
  
  
  # Step 2 remove peptides/proteins below minimum count requirement overall 
  cat(file = stderr(), "step 2, remove below minimum...", "\n")
  df$measured_count <- apply(df[, (info_columns + 1):ncol(df)], 1, function(x) sum(!is.na(x)))
  df <- subset(df, df$measured_count >= params$filter_min_measured_all)
  df <- df[,1:total_columns]
  
  # Step 3 remove peptides/proteins below minimum count requirement in groups 
  cat(file = stderr(), "step 3, remove below minimum group requirement...", "\n")
  if (params$filter_x_percent) {
    for (i in 1:params$group_number) {
      df$na_count <- apply(df[, (sample_groups$start[i] + info_columns):(sample_groups$end[i] + info_columns)], 1, function(x) sum(!is.na(x)))
      df$na_count <- df$na_count / sample_groups$Count[i]
      colnames(df)[colnames(df) == "na_count"] <- sample_groups$Group[i]
    }
    df$max <- apply(df[, (total_columns + 1):(ncol(df))], 1, function(x) max(x))
    df <- subset(df, df$max >= params$filter_x_percent_value)
    df <- df[1:total_columns]
  }
  
  #Step 4 optional filter by cv of specific sample group
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
  
  cat(file = stderr(), "step 5 - remove_duplicates...", "\n")
  
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
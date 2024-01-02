cat(file = stderr(), "load Shiny_Filter.R", "\n")

#-------------------------------------------------------------------------------------------#----------------------------------------------------------------------------------------
filter_data <- function(session, input, output){
  cat(file = stderr(), "Function - filter_data...", "\n")

  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "preprocess filter precursor...", "\n")
    bg_filter <- callr::r_bg(func = filter_data_bg, args = list("precursor_start", "precursor_filter", params), stderr = "error_filter.txt", supervise = TRUE)
    bg_filter$wait()
    cat(file = stderr(), readLines("error_filter.txt"), "\n")
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

  # Step 1 remove peptides/proteins below minumum count requirement overall 
  cat(file = stderr(), "step 1, remove below minimum...", "\n")
  info_columns <- ncol(df) - params$sample_number
  total_columns <- ncol(df)
  df[df == 0] <- NA
  df$measured_count <- apply(df[, (info_columns + 1):ncol(df)], 1, function(x) sum(!is.na(x)))
  df <- subset(df, df$measured_count >= params$filter_min_measured_all)
  df <- df[,1:total_columns]
  
  # Step 2 remove peptides/proteins below minumum count requirement in groups 
  cat(file = stderr(), "step 2, remove below minimum group requirement...", "\n")
  if (params$filter_x_percent) {
    for (i in 1:params$group_number) {
      df$na_count <- apply(df[, (sample_groups$start[i] + info_columns):
                                (sample_groups$end[i] + info_columns)], 1, function(x) sum(!is.na(x)))
      df$na_count <- df$na_count / sample_groups$Count[i]
      colnames(df)[colnames(df) == "na_count"] <- sample_groups$Group[i]
    }
    df$max <- apply(df[, (total_columns + 1):(ncol(df))], 1, function(x) max(x))
    df <- subset(df, df$max >= params$filter_x_percent_value)
    df <- df[1:total_columns]
  }
  
  #Step 3 optional filter by cv of specific sample group
  cat(file = stderr(), "step 3, cv minimum...", "\n")
  if (params$filter_cv) {
    cat(file = stderr(), str_c("filter by cv"), "\n")

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
  

  cat(file = stderr(), "step 4 - remove_duplicates...", "\n")
  
  if (params$data_source == "PD") {
    df$Modifications[is.na(df$Modifications)] <- ""
    df$dup <- str_c(df$Sequence, "_", df$Modifications)
    df <- dplry::distinct(df, dup, .keep_all = TRUE)
    df$dup <- NULL
  }
  
  if (params$data_source == "SP") {
    df <- dplyr::distinct(df, PrecursorId, .keep_all = TRUE)
  }
  
  cat(file = stderr(), "step 5 - write data to db...", "\n")
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
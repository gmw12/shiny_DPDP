cat(file = stderr(), "Shiny_Impute_Functions.R", "\n")

#--------------------------------------------------------------------------------
# imputation of missing data
impute_duke <- function(df, df_random, df_groups, params) {
  cat(file = stderr(), "Function - impute_duke...", "\n")
  
  df_impute <- df[1:(ncol(df) - params$sample_number)]
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- log(df,2)
  
  for (i in 1:nrow(df_groups)) {
    # calculate stats for each sample group
    df_temp <- data.frame(df[c(df_groups$start[i]:df_groups$end[i])])
    df_temp_random <- data.frame(df_random[c(df_groups$start[i]:df_groups$end[i])])
    
    # adding if statement incase there are non quant groups (1 sample)
    if (ncol(df_temp) > 1) {
      df_temp$max_missing <- df_groups$Count[i]*((100 - as.numeric(params$missing_cutoff))/100)
      df_temp$missings <- rowSums(is.na(df_temp[1:df_groups$Count[i]]))
      df_temp$average <- apply(df_temp[1:df_groups$Count[i]], 1, FUN = function(x) {mean(x, na.rm = TRUE)})
      
      #get table for intensity, sd, average 
      df_impute_bin_name <- stringr::str_c("impute_bin_", df_groups$Group[i])
      conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
      df_impute_bin <- RSQLite::dbReadTable(conn, df_impute_bin_name)
      RSQLite::dbDisconnect(conn)
      
      # if the number of missing values <= minimum then will impute based on normal dist of measured values
      find_rows <- which(df_temp$missings > 0 & df_temp$missings <= df_temp$max_missing)
      for (j in find_rows) {
        findsd <- df_impute_bin |> dplyr::filter(df_temp$average[j] >= min2, df_temp$average[j] <= max)
        for (k in 1:df_groups$Count[i]) {
          if (is.na(df_temp[j,k])) {
            df_temp[j,k] = df_temp$average[j] + (df_temp_random[j,k] * findsd$sd[1])
          }
        }
      }
    }
    df_temp <- df_temp[1:df_groups$Count[i]]
    df_temp <- data.frame(2^df_temp)
    df_impute <- cbind(df_impute, df_temp[1:df_groups$Count[i]])
  }
  
   
    cat(file = stderr(), "Function - impute_duke...end", "\n")
    
  return(df_impute)
}

#--------------------------------------------------------------------------------
# imputation of missing data
impute_bottomx <- function(df, df_random, params){
  cat(file = stderr(), "apply_bottomx function...", "\n")
  
  df_impute <- df[1:(ncol(df) - params$sample_number)]
  
  #Use all data for distribution or only ptm
  if (as.logical(params$impute_ptm)) {
    if ("Modifications" %in% colnames(df)) {
      df_bottomx_data <- df[grep(params$impute_ptm_grep, df$Modifications),]
    } else {
      df_bottomx_data <- df[grep(params$impute_ptm_grep, df$Sequence),]
    }  
  } else {
    df_bottomx_data <- df
  }
  
  df_bottomx_data <- df_bottomx_data[(ncol(df_bottomx_data) - params$sample_number + 1):ncol(df_bottomx_data)] 
  df_bottomx_data <- log(df_bottomx_data,2)
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)] 
  df <- log(df,2)
  
  #calc 100 bins for Bottom X%
  data_dist <- as.vector(t(df_bottomx_data))
  data_dist <- data_dist[!is.na(data_dist)]
  data_dist <- data_dist[data_dist > 0]
  data_dist <- data.frame(data_dist)
  data_dist$bin <- dplyr::ntile(data_dist, 100)  
  bottomx_min <- min(data_dist[data_dist$bin == 1,]$data_dist)
  bottomx_max <- max(data_dist[data_dist$bin == as.numeric(params$bottom_x),]$data_dist)
  
  test <- apply(is.na(df), 2, which)
  test <- test[lapply(test,length) > 0]
  
  for (n in names(test)) {
    for (r in (test[[n]])) {
      c <- which(colnames(df) == n )
      # uses stored random numbers from -1 to 1
      df[r,c] <- bottomx_min + (abs(as.numeric(df_random[r,c])) * (bottomx_max - bottomx_min))
    }
  }
  
  df <- data.frame(2^df)
  df_impute <- cbind(df_impute, df)
  
  cat(file = stderr(), "Function - impute_bottomx...end", "\n")
  
  return(df_impute)
}


#--------------------------------------------------------------------------------
# Local least squares imputation (lls)
impute_lls <- function(data_in){
  column_names <- colnames(data_in)
  data_in[data_in == 0] <- NA
  tdata_in <- as.data.frame(t(data_in))
  tdata_in_impute <- llsImpute(tdata_in, k = 150, allVariables = TRUE)
  data_out <- tdata_in_impute@completeObs
  data_out <- as.data.frame(t(data_out))
  colnames(data_out) <- column_names
  return(data_out)
}

#--------------------------------------------------------------------------------
# Local least squares imputation (lls)
impute_knn <- function(data_in){
  require(impute)
  knndata <- log2(data_in)
  knndata[knndata == -Inf] = NA
  knndata[knndata == 0] <- NA
  knndata <- data.matrix(knndata)
  knnimpute <- impute.knn(knndata, k = 10)
  data_out <- knnimpute$data
  return(data_out)
}


#--------------------------------------------------------------------------------
impute_average_group <- function(data_in){
  data_in[data_in == 0] <- NA
  for (i in 1:dpmsr_set$y$group_number) 
  {
    assign(dpmsr_set$y$sample_groups$Group[i], data.frame(data_in[c(dpmsr_set$y$sample_groups$start[i]:dpmsr_set$y$sample_groups$end[i])]))
    df <- get(dpmsr_set$y$sample_groups$Group[i])
    df$sum <- rowSums(df, na.rm = TRUE)
    df$rep <- dpmsr_set$y$sample_groups$Count[i]  
    df$min <- dpmsr_set$y$sample_groups$Count[i]/2
    df$missings <- rowSums(is.na(df[1:dpmsr_set$y$sample_groups$Count[i]]))
    df$average <- df$sum / (df$rep - df$missings)
    
    for (j in 1:nrow(df)) {
      for (k in 1:dpmsr_set$y$sample_groups$Count[i]) {
        if (is.na(df[j,k]) ) { df[j,k] <- df$average[j]}
      }
    }
    assign(dpmsr_set$y$sample_groups$Group[i], df[1:dpmsr_set$y$sample_groups$Count[i]])
  }
  data_out <- get(dpmsr_set$y$sample_groups$Group[1])
  for (i in 2:dpmsr_set$y$group_number)  {data_out <- cbind(data_out, get(dpmsr_set$y$sample_groups$Group[i]))}
  return(data_out)
}

#--------------------------------------------------------------------------------
impute_average_global <- function(data_in) {
  data_in[data_in == 0] <- NA
  df <- data_in
  df$average <- apply(df, 1, FUN = function(x) {mean(x[x > 0], na.rm = TRUE)})
  for (j in 1:nrow(df)) {
    for (k in 1:dpmsr_set$y$sample_number) {
      if (is.na(df[j,k]) ) { df[j,k] <- df$average[j]}
    }
  }
  
  data_out <- df[1:dpmsr_set$y$sample_number]
  return(data_out)
}



#--------------------------------------------------------------------------------

impute_minimum <- function(df){
  df[df == 0] <- NA
  df$minimum <- apply(df, 1, FUN = function(x) {min(x[x > 0], na.rm = TRUE)})
  for (j in 1:nrow(df)) {
    for (k in 1:dpmsr_set$y$sample_number) {
      if (is.na(df[j,k])) {df[j,k] = df$minimum[j]}
    }
  }
  return(df[1:dpmsr_set$y$sample_number])
}

#--------------------------------------------------------------------------------
# Imputing missing values using the EM algorithm proposed in section 5.4.1 of Schafer (1997).
impute_mle <- function(df){
  require(imp4p)
  df[df == 0] <- NA
  df <- log2(df)
  df_mle <- impute.mle(df, dpmsr_set$y$group_factor) 
  df_mle <- data.frame(df_mle)
  return(df_mle)
}

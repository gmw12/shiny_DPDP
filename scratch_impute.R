
conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
df_temp_random <- RSQLite::dbReadTable(conn, "test_df_temp_random")
df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
df_temp <- RSQLite::dbReadTable(conn, "test_df_temp")
df_impute_bin <- RSQLite::dbReadTable(conn, "test_df_impute_bin")
table_name <- stringr::str_c("precursor_norm_", "sltmm")
RSQLite::dbDisconnect(conn)
i=1

# if the number of missing values <= minimum then will impute based on normal dist of measured values
start_time <- Sys.time()
find_rows <- which((df_temp$missings > 0 & df_temp$missings <= df_temp$max_missing), arr.ind = TRUE)
for (j in find_rows) {
  findsd <- df_impute_bin |> dplyr::filter(df_temp$average[j] >= min2, df_temp$average[j] <= max)
  for (k in 1:df_groups$Count[i]) {
    if (is.na(df_temp[j,k])) {
      df_temp[j,k] = df_temp$average[j] + (df_temp_random[j,k] * findsd$sd[1])
    }
  }
}

end_time <- Sys.time() - start_time
end_time
#---------------------------
conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
df_temp_random <- RSQLite::dbReadTable(conn, "test_df_temp_random")
df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
df_temp <- RSQLite::dbReadTable(conn, "test_df_temp")
df_impute_bin <- RSQLite::dbReadTable(conn, "test_df_impute_bin")
table_name <- stringr::str_c("precursor_norm_", "sltmm")
RSQLite::dbDisconnect(conn)
i=1

# if the number of missing values <= minimum then will impute based on normal dist of measured values
start_time <- Sys.time()
find_rows <- which((df_temp$missings > 0 & df_temp$missings <= df_temp$max_missing))
find_na <- data.frame(which(is.na(df_temp[,1:df_groups$Count[i]]), arr.ind = TRUE))
find_final <- dplyr::filter(find_na, row %in% find_rows)

for (j in 1:nrow(find_final)) {
  r <- find_final$row[j]
  c <- find_final$col[j]
  findsd <- df_impute_bin |> dplyr::filter(df_temp$average[r] >= min2, df_temp$average[r] <= max)
  df_temp[r,c] = df_temp$average[r] + (df_temp_random[r,c] * findsd$sd[1])
}

end_time <- Sys.time() - start_time
end_time


#---------------------------
conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
df_temp_random <- RSQLite::dbReadTable(conn, "test_df_temp_random")
df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
df_temp <- RSQLite::dbReadTable(conn, "test_df_temp")
df_impute_bin <- RSQLite::dbReadTable(conn, "test_df_impute_bin")
table_name <- stringr::str_c("precursor_norm_", "sltmm")
RSQLite::dbDisconnect(conn)
i=1

# if the number of missing values <= minimum then will impute based on normal dist of measured values
start_time <- Sys.time()
find_rows <- which((df_temp$missings > 0 & df_temp$missings <= df_temp$max_missing))
find_na <- data.frame(which(is.na(df_temp[,1:df_groups$Count[i]]), arr.ind = TRUE))
find_final <- dplyr::filter(find_na, row %in% find_rows)

require(foreach)
require(doParallel)
cores <- detectCores()
cl <- makeCluster(cores/2)
registerDoParallel(cl)

testme <- foreach(j = 1:nrow(find_final), .combine = c) %dopar% {
  r <- find_final$row[j]
  c <- find_final$col[j]
  findsd <- df_impute_bin |> dplyr::filter(df_temp$average[r] >= min2, df_temp$average[r] <= max)
  #df_temp[r,c] <- 
  df_temp$average[r] + (df_temp_random[r,c] * findsd$sd[1])
}
stopCluster(cl)

end_time <- Sys.time() - start_time
end_time



#-------
start_time <- Sys.time()
df_find <- which(df_temp$missings > 0 & df_temp$missings <= df_temp$max_missing)



for (j in find_rows) {
  findsd <- df_impute_bin |> dplyr::filter(df_temp$average[j] >= min2, df_temp$average[j] <= max)
  for (k in 1:df_groups$Count[i]) {
    if (is.na(df_temp[j,k])) {
      df_temp[j,k] = df_temp$average[j] + (df_temp_random[j,k] * findsd$sd[1])
    }
  }
}

end_time <- Sys.time() - start_time
end_time
























start_time <- Sys.time()

proc_count <- 10
df_proc <- data.frame(matrix(ncol = 5, nrow = proc_count))
colnames(df_proc) <- c("proc", "start_row", "end_row", "df_row_start", "df_row_end")
df_proc$proc <- seq(1:proc_count)
step <- floor(length(find_rows)/proc_count)

df_proc$start_row[1] <- 1
df_proc$end_row[1] <- step
df_proc$df_row_start[1] <- 1
df_proc$df_row_end[1] <- find_rows[df_proc$end_row[1]]
for (p in 2:proc_count) {
  df_proc$start_row[p] <- df_proc$start_row[p - 1] + step
  df_proc$end_row[p] <- df_proc$end_row[p - 1] + step
  df_proc$df_row_start[p] <- df_proc$df_row_end[p - 1] + 1 
  df_proc$df_row_end[p] <- find_rows[df_proc$end_row[p]]
}
df_proc$end_row[proc_count] <- length(find_rows)
df_proc$df_row_end[proc_count] <- nrow(df_temp)


faster_bg <- function(find_rows, df_impute_bin, df_temp, df_groups) {
  for (j in find_rows) {
    findsd <- df_impute_bin |> dplyr::filter(df_temp$average[j] >= min2, df_temp$average[j] <= max)
    for (k in 1:df_groups$Count[i]) {
      if (is.na(df_temp[j,k])) {
        df_temp[j,k] = df_temp$average[j] + (df_temp_random[j,k] * findsd$sd[1])
      }
    }
  }
}


for (j in find_rows) {
  findsd <- df_impute_bin |> dplyr::filter(df_temp$average[j] >= min2, df_temp$average[j] <= max)
  for (k in 1:df_groups$Count[i]) {
    if (is.na(df_temp[j,k])) {
      df_temp[j,k] = df_temp$average[j] + (df_temp_random[j,k] * findsd$sd[1])
    }
  }
}



test_df <- df_temp[df_proc$df_row_start[2]:df_proc$df_row_end[2],]


test_df[1,1]
#------------------------------------------------------------------------------
# imputation of missing data
impute_duke <- function(df, df_random, df_groups, params) {

  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df_random <- RSQLite::dbReadTable(conn, "random")
  df_groups <- RSQLite::dbReadTable(conn, "sample_groups")
  table_name <- stringr::str_c("precursor_norm_", "sltmm")
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "Function - impute_duke...", "\n")
  
  df_impute <- df[1:(ncol(df) - params$sample_number)]
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- log(df,2)
  
  require(foreach)
  require(doParallel)
  cores = detectCores()
  cl <- makeCluster(cores/2)
  registerDoParallel(cl)
  
  testcluster <- foreach(i = 1:nrow(df_groups)) {
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













list1 = find_final$row
list2 = find_final$col
find_final$result <- testme

list3 = list(find_final$result)

df_temp[list1, list2] <- list3

testme <- df_temp

start <- Sys.time()
col_max <- max(find_final$col)
for (i in 1:col_max) {
    swap_row <- find_final$row[find_final$col == i]
    swap_result <- find_final$result[find_final$col == i]
    testme[swap_row, i] <- swap_result
}
end <- Sys.time() - start
end

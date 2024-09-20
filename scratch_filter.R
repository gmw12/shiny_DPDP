df <- read_table_try('precursor_filter', params)

protein_list <- unique(df$Accession)
bad_df <- data.frame()
percentsd_cutoff = 50
intensity_cutoff = 500

start <- Sys.time()

require(foreach)
require(doParallel)
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

df_final <- foreach(protein = protein_list, .combine = rbind) %dopar% {
#for (protein in protein_list){
  
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
      if (prec_average[i] < intensity_cutoff) { break }
      prec_sums <- colSums(df3, na.rm = TRUE)
      prec_ratio <- unlist(df3[1,]) / prec_sums 
      test <- sd(prec_ratio, na.rm = TRUE)/mean(prec_ratio, na.rm = TRUE) * 100
      if (test > percentsd_cutoff) {
        bad_list <- c(bad_list, row.names(df3)[1])
        bad_result <- c(bad_result, test)
        bad_average <- c(bad_average, prec_average[i])
        }
      df3 <- df3[-1,]
    }
  }
  
  temp_df <- data.frame(cbind(bad_list, bad_result, bad_average))
  temp_df
  #bad_df <- rbind(bad_df, temp_df)
}
stopCluster(cl) 
end <- Sys.time() - start
end

df_final <- df[!(df$PrecursorId %in% bad_df$bad_list),]
nrow(df) - nrow(df_final)
length(protein_list) - length(unique(df_final$Accession))


file <- "/mnt/h_black2/11135/20260107_130119_11135_Feng_SEER_AB_010726_Report.tsv"
df_raw <- data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)

df_raw <- data.frame(df_raw)

data_column_total <- 26

df_info <- df_raw[, 1:(ncol(df_raw)-data_column_total)]
df_data <- df_raw[, (ncol(df_raw)-data_column_total+1):ncol(df_raw)]

df_A <- df_raw[, grepl("_A_|EG.ModifiedSequence", names(df_raw))]
df_B <- df_raw[, grepl("_B_|EG.ModifiedSequence", names(df_raw))]


# set all values where df_A = "filtered" to 0
df_A[df_A == "Filtered"] <- 0
df_B[df_B == "Filtered"] <- 0

df_A[df_A == "NaN"] <- 0
df_B[df_B == "NaN"] <- 0

#set all NA values to 0
df_A[is.na(df_A)] <- 0
df_B[is.na(df_B)] <- 0


#in df_A set all columns except EG.ModifiedSequence to numeric
for (col in names(df_A)) {
  if (col != "EG.ModifiedSequence") {
    df_A[[col]] <- as.numeric(df_A[[col]])
  }
}

for (col in names(df_B)) {
  if (col != "EG.ModifiedSequence") {
    df_B[[col]] <- as.numeric(df_B[[col]])
  }
}



#create missing dataframes
df_A_missing <- df_A
df_A_missing[df_A_missing > 0] <- 1
df_B_missing <- df_B
df_B_missing[df_B_missing > 0] <- 1

library(dplyr)
df_A_summed <- df_A %>%
  group_by(EG.ModifiedSequence) %>%
  summarise(across(everything(), sum))

df_B_summed <- df_B %>%
  group_by(EG.ModifiedSequence) %>%
  summarise(across(everything(), sum))

df_A_missing_summed <- df_A_missing %>%
  group_by(EG.ModifiedSequence) %>%
  summarise(across(everything(), sum))

df_B_missing_summed <- df_B_missing %>%
  group_by(EG.ModifiedSequence) %>%
  summarise(across(everything(), sum))

df_A_summed_mean <-rowMeans(df_A_summed[2:ncol(df_A_summed)])
df_B_summed_mean <-rowMeans(df_B_summed[2:ncol(df_B_summed)])

df_A_summed_sum <-rowSums(df_A_summed[2:ncol(df_A_summed)])
df_B_summed_sum <-rowSums(df_B_summed[2:ncol(df_B_summed)])

df_A_missing_summed_sum <-rowSums(df_A_missing_summed[2:ncol(df_A_missing_summed)])
df_B_missing_summed_sum <-rowSums(df_B_missing_summed[2:ncol(df_B_missing_summed)])

df_final <- data.frame(
  EG.ModifiedSequence = df_A_summed$EG.ModifiedSequence,
  A_Mean = df_A_summed_mean,
  B_Mean = df_B_summed_mean,
  A_Sum = df_A_summed_sum,
  B_Sum = df_B_summed_sum,
  A_Missing = df_A_missing_summed_sum,
  B_Missing = df_B_missing_summed_sum
)

df_final$score <- 0

#if df_final$A_Mean > df_final$B_Mean then add 1 to score
df_final$score <- df_final$score + ifelse(df_final$A_Mean > df_final$B_Mean, 1, 0)
#if df_final$A_Sum > df_final$B_Sum then add 1 to score
df_final$score <- df_final$score + ifelse(df_final$A_Sum > df_final$B_Sum, 1, 0)
#if df_final$A_Missing < df_final$B_Missing then add 1 to score
df_final$score <- df_final$score + ifelse(df_final$A_Missing < df_final$B_Missing, 1, 0)

A_peptides <- df_final$EG.ModifiedSequence[df_final$score >= 2]
B_peptides <- df_final$EG.ModifiedSequence[df_final$score <= 1]

length(A_peptides) + length(B_peptides)
nrow(df_final)

#subset of df_raw where df_raw$EG.ModifiedSequence is in A_peptides
df_A_final <- df_raw[df_raw$EG.ModifiedSequence %in% A_peptides, ]
df_B_final <- df_raw[df_raw$EG.ModifiedSequence %in% B_peptides, ]
nrow(df_A_final) + nrow(df_B_final)
nrow(df_raw)

df_A_final <- df_A_final[, !grepl("_B_", names(df_A_final))]
df_B_final <- df_B_final[, !grepl("_A_", names(df_B_final))]

#edit column names by removing "_A" or "_B"
names(df_A_final) <- gsub("_A_", "_", names(df_A_final))
names(df_B_final) <- gsub("_B_", "_", names(df_B_final))

#edit column names by removing all before "ID"
names(df_A_final) <- sub(".*(ID\\d+_\\d+_)", "\\1", names(df_A_final))
names(df_B_final) <- sub(".*(ID\\d+_\\d+_)", "\\1", names(df_B_final))

sort_SEER_columns <- function(df) {
  nms <- names(df)
  
  # Extract the two numeric keys from names like "...ID234343_01_..."
  m <- regexec("ID(\\d+)_([0-9]+)_", nms)
  rm <- regmatches(nms, m)
  
  id_num  <- rep(NA_integer_, length(nms))
  sub_num <- rep(NA_integer_, length(nms))
  
  ok <- lengths(rm) == 3
  id_num[ok]  <- as.integer(vapply(rm[ok], `[[`, "", 2))
  sub_num[ok] <- as.integer(vapply(rm[ok], `[[`, "", 3))
  
  # "Data columns" = those where we successfully extracted both numbers
  data_idx <- ok
  info_idx <- !data_idx
  
  # Order data columns by the extracted numbers (and name as a stable tie-breaker)
  ord <- order(id_num[data_idx], sub_num[data_idx], nms[data_idx])
  
  df_sorted <- df[, c(which(info_idx), which(data_idx)[ord]), drop = FALSE]
  
  
  return(df_sorted)
}

df_A_final <- sort_SEER_columns(df_A_final)
df_B_final <- sort_SEER_columns(df_B_final)


df_final_combined <- rbind(df_A_final, df_B_final)

output_file <- gsub(".tsv", "_filtered_SEER.tsv", file)
data.table::fwrite(df_final_combined, file = output_file, sep = "\t", quote = FALSE, row.names = FALSE)
print(paste0("Filtered data written to ", output_file))



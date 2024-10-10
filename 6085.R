library(readxl)
file_name <- "/mnt/h_black1/00000/6085_5.xlsx"

df <- read_excel(file_name)
df$Modifications <- gsub(");", "),", df$Modifications)

df$split <- stringr::str_split(df$Modifications, pattern = ";")
df$mod_phos <- ""
df$phos_count <- ""
df$mod_list <- ""

for (i in (1:nrow(df)) ) {
  test <- unlist(df$split[i])
  for (j in (1:mod_count)) {
    if (grepl("Phospho", test[j])) {
      df$mod_phos[i] <- test[j]
      df$phos_count[i] <- substr(test[j], 1, 1)
      }
    }
  }

df$mod_list <- gsub("^.*\\[", "", df$mod_phos)
df$mod_list <- gsub("\\]", "", df$mod_list)
df$mod_list <- gsub("\\(.*?\\)", "", df$mod_list)
df$mod_list <- gsub("[STY]/[STY]/[STY]", "999", df$mod_list)
df$mod_list <- gsub("[STY]/[STY]", "999", df$mod_list)
df$mod_list <- gsub("[STY]", "", df$mod_list)



df$ModSeq <- df$Sequence
phos_string <- "[Phospho (STY)]"

for (i in (1:nrow(df)) ) {
  sites <- df$mod_list[i]
  sites <- unlist(stringr::str_split(sites, pattern = ", "))
  sites <- as.numeric(sites)
  sites <- sort(sites, decreasing = TRUE)
  if (length(sites >0)) {
  for (site in sites) {
    if (site < 999) {
      str1 <- substr(df$ModSeq[i], 1, site)
      str2 <- substr(df$ModSeq[i], site+1, nchar(df$ModSeq[i]))
      df$ModSeq[i] <- paste(str1, phos_string, str2, sep = "")
    }
   }
  }
    df$ModSeq[i] <- paste("_", df$ModSeq[i], "_", sep = "")
}


wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "newformat")
openxlsx::writeData(wb, sheet=1, df)  
openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
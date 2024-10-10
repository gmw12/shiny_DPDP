file1 = "/mnt/h_black2/10651/20241009_104707_10651_GluC_ZrIMAC_100924_Report_GW.tsv"
file2 = "/mnt/h_black2/10651/20241009_115311_10651_GluC_TiO2_100924_Report_GW.tsv"
file3 = "/mnt/h_black2/10651/20241009_132403_10651_Trypsin_ZrIMAC_100924_Report_GW.tsv"
file4 = "/mnt/h_black2/10651/20241009_145150_10651_Trypsin_TiO2_10092_Report_GW.tsv"

desc1 = "GluC_ZrIMAC"
desc2 = "GluC_TiO2"
desc3 = "Trypsin_ZrIMAC"
desc4 = "Trypsin_TiO2"

file = file1
desc = desc1

#--------------------------------------------------------------------------------------------------

df <- data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)

phos_which <- which(grepl("Phospho", df$EG.ModifiedSequence))
df_phos <- df[phos_which,]
df_non <- df[-phos_which,]

df_phos_prob <- df_phos |> dplyr::select(contains('PTMProbabilities [Phospho')) 

#--
reduce_prob <- function(df){
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  df[df=="Filtered"] <- ""
  
  parallel_result <- foreach(c = colnames(df), .combine = cbind) %dopar% {
    #for (c in colnames(df)){
    test <- df[[c]]
    find_rows <- which(stringr::str_detect(test, ";"))
    for (r in find_rows) {
      if (grepl(";", test[r])) {
        test[r] <- max(strsplit(test[r], ";") |> unlist() |> as.numeric())
      }
    }
    test
  }
  stopCluster(cl) 
  
  parallel_result <- data.frame(parallel_result)
  
  max_result  <- apply(parallel_result, 1, FUN = max, na.rm = TRUE)
  
  #df <- tibble::add_column(df, max_result, .after="EG.PrecursorID")
  
  return(max_result)  
}


Localized <- as.numeric(reduce_prob(df_phos_prob))
df_phos <- tibble::add_column(df_phos, Localized, .after="EG.PrecursorId")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, desc)
openxlsx::writeData(wb, sheet=1, df_phos)
file_name <- stringr::str_c("/mnt/h_black2/10651/", desc, ".xlsx")
openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)




#------------------------------------------------------------------------------------

file_nolocal = "/mnt/h_black2/10651/20241009_132403_10651_Trypsin_ZrIMAC_100924_Report_nolocal.tsv"
file_local = "/mnt/h_black2/10651/20241009_132403_10651_Trypsin_ZrIMAC_100924_Report_local.tsv"

df_nolocal <- data.table::fread(file = file_nolocal, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)
df_local <- data.table::fread(file = file_local, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)

phos_which <- which(grepl("Phospho", df_nolocal$EG.ModifiedSequence))
df_nolocal_phos <- df_nolocal[phos_which,]
phos_which <- which(grepl("Phospho", df_local$EG.ModifiedSequence))
df_local_phos <- df_local[phos_which,]


df_diff <- setdiff(df_nolocal_phos, df_local_phos)



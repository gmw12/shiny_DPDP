<<<<<<< HEAD


#--------------------------------------------------------------------------------------------------
notlocal_file <- "/Users/gregwaitt/data/nolocal_PTManlaysis_3modlocal.tsv"
df <- data.table::fread(file = notlocal_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)

phos_which <- which(grepl("Phospho", df$EG.ModifiedSequence))
df_phos <- df[phos_which,]
df_non <- df[-phos_which,]

df_phos_prob <- df_phos |> dplyr::select(contains('PTMProbabilities [Phospho')) 
df_non_prob <- df_non |> dplyr::select(contains('PTMProbabilities [Oxidation'), contains('PTMProbabilities [Carbamidomethyl')) 

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

#--
extract_mods <- function(x){
  temp <- str_replace(x, pattern = "^.*?\\[", replacement = "") 
  temp <- str_replace(temp, pattern = "\\](?:.(?!\\]))+$", replacement = "") 
  temp <- gsub("\\].*?\\[", ", ", temp)
  temp <- gsub("_.*?_", "", temp)
  return(temp)
}

#--
remove_mods <- function(x){
  temp <- gsub("\\[.*?\\]", "", x)
  return(temp)
}
#--

Localized <- as.numeric(reduce_prob(df_phos_prob))
df_phos <- tibble::add_column(df_phos, Localized, .after="EG.PrecursorId")

PrecursorId_nomod <- remove_mods(df_phos$EG.PrecursorId)
ModifiedSequence_nomod <- remove_mods(df_phos$EG.ModifiedSequence)
mods_only <- extract_mods(df_phos$EG.ModifiedSequence)

new_PrecursorID <- paste(PrecursorId_nomod, mods_only, sep = "[")
new_PrecursorID <- paste(new_PrecursorID, "]", sep = "")

new_ModifiedSeqence <- paste(ModifiedSequence_nomod, mods_only, sep = "[")
new_ModifiedSeqence <- paste(new_ModifiedSeqence, "]", sep = "")

to_rename <- which(df_phos$Localized < 0.75)

df_phos$EG.PrecursorId[to_rename] <- new_PrecursorID[to_rename]
df_phos$EG.ModifiedSequence[to_rename] <- new_ModifiedSeqence[to_rename]


Localized <- as.numeric(reduce_prob(df_non_prob))
df_non <- tibble::add_column(df_non, Localized, .after="EG.PrecursorId")
to_remove <- which(df_non$Localized < 0.75)
df_non <- df_non[-to_remove,]
df_non$Localized <- ""

df_final <- rbind(df_phos, df_non)

df_final_test1 <- df_final |> dplyr::select(contains('Accessions'), contains('ModifiedSequence'), contains("Localized")) 
df_final_test2 <- df_final |> dplyr::select(contains('Accessions'), contains('PrecursorId'), contains("Localized")) 

df_final_peptide1 <- df_final_test1 |> dplyr::group_by(PG.ProteinAccessions, EG.ModifiedSequence) |> dplyr::summarise_all(list(max))
df_final_peptide2 <- df_final_test2 |> dplyr::group_by(PG.ProteinAccessions, EG.PrecursorId) |> dplyr::summarise_all(list(max))

























=======
>>>>>>> f4d49cd458d43aa4ce75106a7ffa40fb16581cd1
local_file <- "/Users/gregwaitt/data/local_PTManlaysis_3modlocal.tsv"
df_local <- data.table::fread(file = local_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")

notlocal_file <- "/Users/gregwaitt/data/nolocal_PTManlaysis_3modlocal.tsv"
df_notlocal <- data.table::fread(file = notlocal_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
df <- df_notlocal |> dplyr::select(contains('PTMProbabilities')) 

find_ptm <- data.frame(which(grepl(";", df), arr.ind = TRUE))

test <- df[[9,2]]
num_out <- max(strsplit(test, ";") |> unlist() |> as.numeric())

test <- df[,1]
test <- unlist(test)
find_rows <- which(grepl(";", test), arr.ind = TRUE)
find_rows <- which(stringr::str_detect(test, ";"))

start <- Sys.time()
count = 0
for (c in colnames(df)) {
  test <- df[[c]]
  find_rows <- which(stringr::str_detect(test, ";"))
  for (r in find_rows) {
    if (grepl(";", test[r])) {
      test[r] <- max(strsplit(test[r], ";") |> unlist() |> as.numeric())
      count <- count + 1
    }
  }
  df[[c]] <- test
}
cat(file = stderr(), stringr::str_c("time = ", Sys.time() - start), "\n")
#------------------

test_df <- data.frame(df_local$EG.ModifiedSequence)
colnames(test_df) <- c("ModSeq")
test_df$mods <- stringr::str_match_all(test_df$ModSeq, "\\[\\s*(.*?)\\s*\\]")
#str_replace(x, pattern = ".*-(.)-.*", replacement = "\\1") 

test_df$ModSeq[33]
test_df$mods <- str_replace(test_df$ModSeq, pattern = "^.*?\\[", replacement = "") 
test_df$mods[33]
#.+(\\.+)$
test_df$mods2 <- str_replace(test_df$mods, pattern = "\\](?:.(?!\\]))+$", replacement = "") 
test_df$mods2[33]

test_df$mods3 <- gsub("\\].*?\\[", ", ", test_df$mods2)
test_df$mods3[33]

str_replace(test_df$mod2[33], pattern = "\\](.*)\\[", replacement = ", ")
stringr::str_locate_all(test_df$ModSeq[33], "\\[")
stringr::str_locate_all(test_df$ModSeq[33], "\\]")

gsub("\\].*?\\[", ", ", test_df$mods2[33])


test <- data.frame(df_local$EG.PrecursorId)
test$nomod <- gsub("\\[.*?\\]", "", test$df_local.EG.PrecursorId)


#-----------------------------
#local_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_local.tsv"
local_file <- "/mnt/h_black2/x10546/local_PTManlaysis_3modlocal.tsv"
df_local <- data.table::fread(file = local_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
local_phos <- which(grepl("Phospho", df_local$EG.ModifiedSequence))
df_local_phos <- df_local[local_phos,]
df_local_non_phos <- df_local[-local_phos,]


#notlocal_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_notlocal.tsv"
notlocal_file <- "/mnt/h_black2/x10546/nolocal_PTManlaysis_3modlocal.tsv"
df_notlocal <- data.table::fread(file = notlocal_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
notlocal_phos <- which(grepl("Phospho", df_notlocal$EG.ModifiedSequence))
df_notlocal_phos <- df_notlocal[notlocal_phos,]
df_notlocal_non_phos <- df_notlocal[-notlocal_phos,]


#find values not in both df's
df_unique <- subset(df_notlocal_non_phos, !(EG.PrecursorId %in% df_local_non_phos$EG.PrecursorId))
nrow(df_notlocal_non_phos)-nrow(df_local_non_phos)






df_notlocal_non_phos$nomod <- gsub("\\[.*?\\]", "", df_notlocal_non_phos$EG.PrecursorId)
temp <- str_replace(df_notlocal_non_phos$EG.ModifiedSequence, pattern = "^.*?\\[", replacement = "") 
temp <- str_replace(temp, pattern = "\\](?:.(?!\\]))+$", replacement = "") 
temp <- gsub("\\].*?\\[", ", ", temp)
df_notlocal_non_phos$mods <- gsub("_.*?_", "", temp)

df_notlocal_non_phos$sort <- paste(df_notlocal_non_phos$nomod, df_notlocal_non_phos$mods, sep = "-")

df_qvalue <- df_notlocal_non_phos |> dplyr::select(contains('Qvalue')) 
df_qvalue[df_qvalue== "Filtered"] <- ""
df_qvalue <- mutate_all(df_qvalue, function(x) as.numeric(as.character(x)))
df_qvalue$min <- apply(df_qvalue, 1, FUN = min, na.rm = TRUE)

df_notlocal_non_phos$minQvalue <- df_qvalue$min
df_notlocal_non_phos <- df_notlocal_non_phos[order(df_notlocal_non_phos$sort)]
df_notlocal_non_phos$duplicates <- duplicated(df_notlocal_non_phos$sort)
test_dup <- which(df_notlocal_non_phos$duplicates == "TRUE")




df <- df_notlocal |> dplyr::select(contains('PTMProbabilities')) 


require(foreach)
require(doParallel)
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

start <- Sys.time()
parallel_result <- foreach(c = colnames(df), .combine = cbind) %dopar% {
  test <- df[[c]]
  find_rows <- which(stringr::str_detect(test, ";"))
  for (r in find_rows) {
    if (grepl(";", test[r])) {
      test[r] <- max(strsplit(test[r], ";") |> unlist() |> as.numeric())
    }
  }
  test
}
cat(file = stderr(), stringr::str_c("time = ", Sys.time() - start), "\n")
stopCluster(cl) 

parallel_result <- data.frame(parallel_result)

colnames(parallel_result) <-  colnames(df)
df2 <- parallel_result
df2[df2=="Filtered"] <- ""
df2$max_local <- apply(df2, 1, max)
df2$phos <- grepl("Phospho", df_notlocal$EG.ModifiedSequence)
df2$precursorID <- df_notlocal$EG.PrecursorId

df2$duplicates <- duplicated(df2$precursorID)

test_dup <- which(df2$duplicates == "TRUE")

to_delete <- which(df2$phos == "TRUE" & df2$max_local < 0.75)
df_notlocal2 <- df_notlocal[-to_delete]

#-----


df_test <- df_notlocal[df_notlocal$EG.PrecursorId %in% unlist(df_local$EG.PrecursorId)]

testme <- df_local[grepl("AVDDIP", df_local$EG.PrecursorId)]


df_notlocal$inlocal <- 0

nrow(df_notlocal) - nrow(df_local)

list1 <- (df_notlocal$EG.PrecursorId)
list2 <- (df_local$EG.PrecursorId)
list3 <- list2[!(list2 %in% list1)]
length(list1)
length(list2)
length(list3)

testp <- list3[3]
test_df1 <- df_local[df_local$EG.PrecursorId == testp]
test_df2 <- df_notlocal[df_notlocal$EG.PrecursorId == testp]















#--------------------------------------------------------------------------------------------------
notlocal_file <- "/mnt/h_black2/x10546/nolocal_PTManlaysis_3modlocal.tsv"
df <- data.table::fread(file = notlocal_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
phos_which <- which(grepl("Phospho", df$EG.ModifiedSequence))
df_phos <- df[phos_which,]
df_non <- df[-phos_which,]

df_phos_prob <- df_phos |> dplyr::select(contains('PTMProbabilities [Phospho')) 
df_non_prob <- df_non |> dplyr::select(contains('PTMProbabilities [Oxidation'), contains('PTMProbabilities [Carbamidomethyl')) 

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

#--
extract_mods <- function(x){
  temp <- str_replace(x, pattern = "^.*?\\[", replacement = "") 
  temp <- str_replace(temp, pattern = "\\](?:.(?!\\]))+$", replacement = "") 
  temp <- gsub("\\].*?\\[", ", ", temp)
  temp <- gsub("_.*?_", "", temp)
return(temp)
}

#--
remove_mods <- function(x){
  temp <- gsub("\\[.*?\\]", "", x)
  return(temp)
}
#--

Localized <- as.numeric(reduce_prob(df_phos_prob))
df_phos <- tibble::add_column(df_phos, Localized, .after="EG.PrecursorId")

PrecursorId_nomod <- remove_mods(df_phos$EG.PrecursorId)
ModifiedSequence_nomod <- remove_mods(df_phos$EG.ModifiedSequence)
mods_only <- extract_mods(df_phos$EG.ModifiedSequence)

new_PrecursorID <- paste(PrecursorId_nomod, mods_only, sep = "[")
new_PrecursorID <- paste(new_PrecursorID, "]", sep = "")

new_ModifiedSeqence <- paste(ModifiedSequence_nomod, mods_only, sep = "[")
new_ModifiedSeqence <- paste(new_ModifiedSeqence, "]", sep = "")

to_rename <- which(df_phos$Localized < 0.75)

df_phos$EG.PrecursorId[to_rename] <- new_PrecursorID[to_rename]
df_phos$EG.ModifiedSequence[to_rename] <- new_ModifiedSeqence[to_rename]


Localized <- as.numeric(reduce_prob(df_non_prob))
df_non <- tibble::add_column(df_non, Localized, .after="EG.PrecursorId")
to_remove <- which(df_non$Localized < 0.75)
df_non <- df_non[-to_remove,]
df_non$Localized <- ""

df_final <- rbind(df_phos, df_non)

df_final_test1 <- df_final |> dplyr::select(contains('Accessions'), contains('ModifiedSequence'), contains("Localized")) 
df_final_test2 <- df_final |> dplyr::select(contains('Accessions'), contains('PrecursorId'), contains("Localized")) 

df_final_peptide1 <- df_final_test1 |> dplyr::group_by(PG.ProteinAccessions, EG.ModifiedSequence) |> dplyr::summarise_all(list(max))
df_final_peptide2 <- df_final_test2 |> dplyr::group_by(PG.ProteinAccessions, EG.PrecursorId) |> dplyr::summarise_all(list(max))












df_phos_sort <- data.frame(df_phos$EG.PrecursorId)
colnames(df_phos_sort) <- "PrecursorID"
df_phos_sort$NoMod <- remove_mods(df_phos)
df_phos_sort$Mods <- extract_mods(df_phos)
df_phos_sort$Test <- paste(df_phos_sort$NoMod, df_phos_sort$Mods, sep = "[")
df_phos_sort$Test <- paste(df_phos_sort$Test, "]", sep = "")
df_phos_sort$Test2 <- paste(df_phos$PEP.StrippedSequence, df_phos_sort$Mods, sep = "[")
df_phos_sort$Test2 <- paste(df_phos_sort$Test2, "]", sep = "")
df_phos_sort$local <- as.numeric(reduce_prob(df_phos_prob))

df_phos_sort <- df_phos_sort[order(df_phos_sort$Test, -df_phos_sort$local),]
df_phos_sort$dup1 <- duplicated(df_phos_sort$Test)
df_phos_sort$dup2 <- duplicated(df_phos_sort$Test)

to_remove <- which(df_phos_sort$local < 0.75 & df_phos_sort$dup1==TRUE)
df_phos_sort$remove <- ""
df_phos_sort$remove[to_remove] <- "Remove"

df_phos_sort <- df_phos_sort[order(as.numeric(rownames(df_phos_sort))),]

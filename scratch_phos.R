#---------------------------------------------


local_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_local.tsv"
df_local <- data.table::fread(file = local_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")

notlocal_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_notlocal.tsv"
df_notlocal <- data.table::fread(file = notlocal_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
df_notlocal_p <- df_notlocal |> dplyr::select(contains('PTMProbabilities')) 

df_notlocal_iq <- df_notlocal |> dplyr::select(contains('EG.Qvalue')) 
df_notlocal_iq[df_notlocal_iq=="Filtered"] <- ""
df_notlocal_iq$min_iq <- apply(df_notlocal_iq, 1, min)

#---------
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

#------------------------------------
require(foreach)
require(doParallel)
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

start <- Sys.time()
parallel_result <- foreach(c = colnames(df_notlocal_p), .combine = cbind) %dopar% {
  test <- df_notlocal_p[[c]]
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

df_notlocal_p <- data.frame(parallel_result)
rm(parallel_result)
rm(cl)
#colnames(parallel_result) <-  colnames(df)

#---------------------
colnames(df_notlocal_p) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
df_notlocal_p[df_notlocal_p=="Filtered"] <- ""
df_notlocal_p$max_local <- apply(df_notlocal_p, 1, max)
df_notlocal_p$phos <- grepl("Phospho", df_notlocal$EG.ModifiedSequence)
df_notlocal_p$precursorID <- df_notlocal$EG.PrecursorId
df_notlocal_p$sequence <- df_notlocal$PEP.StrippedSequence
df_notlocal_p$nchar <- nchar(df_notlocal_p$precursorID) 
df_notlocal_p$precur <- substr(df_notlocal_p$precursorID, nchar(df_notlocal_p$precursorID)-1, nchar(df_notlocal_p$precursorID))
df_notlocal_p$unique <- paste(df_notlocal_p$sequence, df_notlocal_p$nchar, df_notlocal_p$precur)

df_notlocal_p$duplicates <- duplicated(df_notlocal_p$unique)
df_notlocal_p$min_iq = df_notlocal_iq$min_iq
rm(df_notlocal_iq)

#------------------------
#compare
local_notin_notlocal <- df_local[!df_local$EG.PrecursorId %in% df_notlocal$EG.PrecursorId,]
notlocal_notin_local <- df_notlocal[!df_notlocal$EG.PrecursorId %in% df_local$EG.PrecursorId,]

df_phos_local <- df_local[grepl("Phospho", df_local$EG.PrecursorId),]
df_phos_notlocal <- df_notlocal[grepl("Phospho", df_notlocal$EG.PrecursorId),]

phos_local_notin_notlocal <- df_phos_local[!df_phos_local$EG.PrecursorId %in% df_phos_notlocal$EG.PrecursorId,]
phos_notlocal_notin_local <- df_phos_notlocal[!df_phos_notlocal$EG.PrecursorId %in% df_phos_local$EG.PrecursorId,]


df_notphos_local <- df_local[!grepl("Phospho", df_local$EG.PrecursorId),]
df_notphos_notlocal <- df_notlocal[!grepl("Phospho", df_notlocal$EG.PrecursorId),]

notphos_local_notin_notlocal <- df_notphos_local[!df_notphos_local$EG.PrecursorId %in% df_notphos_notlocal$EG.PrecursorId,]
notphos_notlocal_notin_local <- df_notphos_notlocal[!df_notphos_notlocal$EG.PrecursorId %in% df_notphos_local$EG.PrecursorId,]

testseq <- "VFDSLNYLPNMLLGMEAAGSAGGVVEAAISYTGDVADPSR"
test_local <- df_notphos_local[df_notphos_local$PEP.StrippedSequence == testseq,]
test_notlocal <- df_notphos_notlocal[df_notphos_notlocal$PEP.StrippedSequence == testseq,]




to_delete <- which(df_notlocal_p$phos == "TRUE" & df_notlocal_p$max_local < 0.75)
df_notlocal2 <- df_notlocal[-to_delete]

df_test <- df_notlocal[df_notlocal$EG.PrecursorId %in% unlist(df_local$EG.PrecursorId)]

testme <- df_local[grepl("AVDDIP", df_local$EG.PrecursorId)]


df_notlocal$inlocal <- 0

nrow(df_notlocal) - nrow(df_local)

list1 <- (df_notlocal$EG.PrecursorId)
list2 <- (df_local$EG.PrecursorId)
list3 <- list2[!(list2 %in% list1)]
list4 <- list1[!(list1 %in% list2)]
length(list4) - length(list3)
nrow(df_notlocal) - nrow(df_local)

testp <- list4[1]
test_df2 <- df2[df2$precursorID == testp,]
test_df1 <- df_local[df_local$EG.PrecursorId == testp]
test_df2 <- df_notlocal[df_notlocal$EG.PrecursorId == testp]


#---------------compare non phos

local_phos <- grepl("Phospho", df_local$EG.ModifiedSequence)
notlocal_phos <- grepl("Phospho", df_notlocal$EG.ModifiedSequence)

df_local_nophos <- df_local[!local_phos,]
df_notlocal_nophos <- df2[!notlocal_phos,]

df_notlocal_nophos <- dplyr::arrange(df_notlocal_nophos, unique, as.numeric(min_iq))

df_notlocal_nophos$duplicates <- duplicated(df_notlocal_nophos$unique)
df_notlocal_nophos2 <- df_notlocal_nophos |> dplyr::distinct(unique, .keep_all=TRUE)
nrow(df_notlocal_nophos) - nrow(df_notlocal_nophos2)


list1 <- (df_notlocal_nophos$precursorID)
list2 <- (df_local_nophos$EG.PrecursorId)
list3 <- list2[!(list2 %in% list1)]

test_df3 <- df_local_nophos[df_local_nophos$EG.PrecursorId %in% list3]


#--------
start <- Sys.time()
df <- data.frame()
for (r in (1:nrow(df_notlocal_p))) {
  check <- 0
  for (c in (1:9)){
    if(df_notlocal_p[[r,c]] != ""){
      sites <- strsplit(unlist(df_notlocal_p[[r,c]]), ";") |> unlist() |> as.numeric()
      if (check == 0){
        temp_df <- data.frame(sites)
        check <- 1
      }else{
        temp_df <- cbind(temp_df, sites)
      }
    }
  }
  if (check == 0){
    sites <- ""
  }else{
    temp_df$max <- apply(temp_df, 1, max)
    sites <- paste(temp_df$max, collapse = ", " )
  }
  df <- rbind(df, sites)
}
cat(file = stderr(), stringr::str_c("time = ", Sys.time() - start), "\n")

#------------------------------------
require(foreach)
require(doParallel)
cores <- detectCores()
cl <- makeCluster(cores - 2)
registerDoParallel(cl)

start <- Sys.time()
df_notlocal_p[df_notlocal_p=="Filtered"] <- ""
parallel_result <- foreach(r = 1:nrow(df_notlocal_p), .combine = rbind) %dopar% {
  check <- 0
  for (c in (1:ncol(df_notlocal_p))){
    if(df_notlocal_p[[r,c]] != ""){
      sites <- strsplit(unlist(df_notlocal_p[[r,c]]), ";") |> unlist() |> as.numeric()
      if (check == 0){
        temp_df <- data.frame(sites)
        check <- 1
      }else{
        temp_df <- cbind(temp_df, sites)
      }
    }
  }
  if (check == 0){
    sites <- ""
  }else{
    temp_df$max <- apply(temp_df, 1, max)
    sites <- paste(temp_df$max, collapse = ", " )
  }
  sites
}
cat(file = stderr(), stringr::str_c("time = ", Sys.time() - start), "\n")
stopCluster(cl) 

df_notlocal_p$prob <- parallel_result
save(df_notlocal_p, file = "erase")
load("erase")

source("Shiny_Misc_Functions.R")
df_notlocal_p$max_local <- ""
for (r in (1:nrow(df_notlocal_p))) {
  df_notlocal_p$max_local[r] <- str_to_numlist_max(df_notlocal_p$prob[[r]])  
}

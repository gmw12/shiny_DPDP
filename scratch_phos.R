local_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_local.tsv"
df_local <- data.table::fread(file = local_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")

notlocal_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_notlocal.tsv"
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
test_df$mods <- stringr::str_match_all(test_df$df_local.EG.ModifiedSequence, "\\[\\s*(.*?)\\s*\\]")
test_df$mods <- stringr::str_match_all(test_df$df_local.EG.ModifiedSequence, "(?<=\\[).*(?=\\])")
test_df$mods2 <- gsub("\\[[^>]+\\]", "", test_df$mods)
#-----------------------------

local_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_local.tsv"
df_local <- data.table::fread(file = local_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
local_phos <- which(grepl("Phospho", df_local$EG.ModifiedSequence))
df_local_phos <- df_local[local_phos,]
df_local_non_phos <- df_local[-local_phos,]



notlocal_file <- "/Users/gregwaitt/data/20240914_164026_10546_Daichi_Phos_081224_Report_notlocal.tsv"
df_notlocal <- data.table::fread(file = notlocal_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
notlocal_phos <- which(grepl("Phospho", df_notlocal$EG.ModifiedSequence))
df_notlocal_phos <- df_notlocal[notlocal_phos,]
df_notlocal_non_phos <- df_notlocal[-notlocal_phos,]


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

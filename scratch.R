#clear memory
rm(list = ls())
gc()
.rs.restartR()

params$database_path <- stringr::str_c(getwd(),"/database/test.db")
database_path <- stringr::str_c("/Users/gregwaitt/Data/project_101224.db")

create_db(db_path)

create_db <- function(db_name) {
  conn <- dbConnect(RSQLite::SQLite(), db_name) 
  RSQLite::dbDisconnect(conn)
}

read_table <- function(table_name, db_path){
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  df <- dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

write_table <- function(table_name, df, db_path){
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

list_tables <- function(db_path){
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  table_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}

filter_db <- function(table_name, column_name, key_word, db_path) {
  conn <- dbConnect(RSQLite::SQLite(), db_path) 
  query <- stringr::str_c("SELECT * FROM ", table_name, " WHERE ", column_name, " LIKE '", accession,"'") 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}

accession = "A0A087WPF7"
df_peptide <- filter_db("peptide_sltmm", "Accession", accession, params)

list_tables(db_path)


db_path = stringr::str_c(getwd(), "/database/project_021125.db")
source('Shiny_File.R')

test <- read_table('params', db_path)

df_test1 <- read_table('precursor_impute_sl', db_path)
df_test2 <- read_table('peptide_impute_sl', db_path)
test_precursor_missing <- read_table('precursor_missing', db_path)
test_protein_missing <- read_table('protein_missing', db_path)

df_raw <- read_table('precursor_raw', db_path)
df_start <- read_table('precursor_start', db_path)
df_filter <- read_table('precursor_filter', db_path)
df_noise <- read_table_try('precursor_noise', db_path)
df_sltmm <- read_table_try('precursor_impute_sltmm', db_path)
df_noise <- read_table_try('precursor_noise', db_path)
df_sl <- read_table_try('precursor_impute_sl', db_path)

df_protein <- df_sl[df_sl$Accession=="TurboID",]
test2 <- colSums(df_protein[,8:ncol(df_protein)])

test2



df_motif <- read_table_try('MotifX_table', db_path)

phos_fasta <- read_table("phos_fasta", db_path)

mv <- read_table("missing_values", db_path)

df1 <- read_table("peptide_impute_sltmm", db_path)
df2 <- read_table("peptide_impute_sltmm_Lrrk2_v_Control_final", db_path)
df3 <- read_table("peptide_impute_sltmm", db_path)

testme2 <- read_table("precursor_missing", db_path)
df_design <- read_table("design", db_path)
test <- read_table("design", db_path)
design <- read_table("design", db_path)
test_sample_groups <- read_table("sample_groups", db_path)
test_stats_comp <- read_table("stats_comp", db_path)
test_design <- read_table("design", db_path)
test_protein_missing <- read_table("protein_missing", db_path)

stats_comp <- read_table("stats_comp", db_path)
sample_groups <- read_table("sample_groups", db_path)





df1 <- read_table("peptide_impute_sltmm_Lrrk2_v_Control_final", db_path)
df_norm_data <- read_table_try('precursor_normdata', db_path)
df <- read_table_try('precursor_noise', db_path)
df <- read_table('precursor_filter', db_path)
df <- read_table_try('precursor_noise', db_path)
df_raw <- read_table_try('precursor_raw', db_path)
df <- read_table_try('precursor_start', db_path)
df_start <- read_table_try('precursor_start', db_path)
df_norm_sltmm <- read_table_try('precursor_norm_sltmm', db_path)
df_norm_sltmm_protein <- read_table_try('protein_sltmm_final', db_path)
precursor_data <- read_table_try('precursor_impute_sltmm', db_path)
df <- read_table_try('precursor_impute_sltmm', db_path)
df1 <- read_table_try('peptide_impute_sltmm', db_path)
df1f <- read_table_try('peptide_impute_sltmm_final', db_path)
dfm <- read_table_try('precursor_missing', db_path)
test3 <- read_table('protein_sltmm', db_path)
test4 <- read_table('protein_sltmm_final', db_path)
df <- read_table(stats_comp$Final_Table_Name[1], db_path)
data_to_norm <- read_table_try('precursor_filter', db_path)
data_name <- 'protein_sltmm_Caskin1_Test_v_Caskin1_Ctrl_final'
accession <- 'Q9JMG2'
conn <- dbConnect(RSQLite::SQLite(), params$database_path) 

query <- stringr::str_c("SELECT * FROM ", data_name, " WHERE Accession LIKE '", accession,"'") 
query
rs <- dbGetQuery(conn, query)


RSQLite::dbDisconnect(conn)

  #------------
sample_cols <- c(colnames(df |> dplyr::select(contains("Normalized"))),
                 colnames(df |> dplyr::select(contains("Imputed"))) )

df_new <- df %>% mutate(across(6:(6+4), round, 0))

pval_cols <- which(stringr::str_detect(colnames(df), "pval"))
df <- df |> dplyr::mutate(dplyr::across(pval_cols, round, 7))

df <- df(pval_cols), round, 7))
df <- df |> mutate_at(pval_cols, funs(round(., 1)))

which(colnames(df) contains('Accession'))
which(str_detect(colnames(df), "SPQC"))

conn <- dbConnect(RSQLite::SQLite(), params$database_path)
dbListTables(conn)

df <- dbReadTable(conn, "params")
df <- dbReadTable(conn, "precursor_raw")
df <- dbReadTable(conn, "precursor_start")
df <- dbReadTable(conn, "precursor_filter")
df_design <- dbReadTable(conn, "design")
df <- dbReadTable(conn, "precursor_sltmm")
df_groups <- dbReadTable(conn, "sample_groups")
df_test <- dbReadTable(conn, "impute_bin_CA")
df <- dbReadTable(conn, "missing_values")
df2 <- dbReadTable(conn, "missing_values_plots")
df <- dbReadTable(conn, "precursor_impute_sl")
df <- dbReadTable(conn, "protein_sl")
df_test <- dbReadTable(conn, "protein_sl_CV")
df_test <- dbReadTable(conn, "summary_cv")
stats_comp_df <- dbReadTable(conn, "stats_comp")
df_missing <- dbReadTable(conn, "precursor_missing")
df_final <- dbReadTable(conn, "protein_sltmm_CA_v_DN_final")

testme <- dbReadTable(conn, "design")


RSQLite::dbDisconnect(conn)





nchar(stats_comp_df$Name[1])
str_split(stats_comp_df$Name[1])

df_samples <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
df2 <- df |>  mutate(across(!where(is.numeric), as.numeric))

test <- unique(df$EG.ModifiedSequence)
test <- unique(df$PG.ProteinAccessions)

test <- readLines("error.txt")
cat(file = stderr(), readLines("error_filter.txt"), "\n")


params$protein_norm_grep <- "A2A5R2|A2A690"

protein_norm_raw <- subset(df, Accession %in% params$protein_norm_grep)


df1 <- df[grepl(params$protein_norm_grep, df$Accession, ignore.case = TRUE),]




df <- df[(ncol(df)-params$sample_number+1):ncol(df)]

test_alignment <- function(x) {
  missing <- sum(is.na(x))/length(x) * 100
  misaligned_count <- 0
  if (missing > params$misaligned_cutoff){
    misaligned_count <- sum(x > params$intensity_cutoff, na.rm = TRUE)
  }
  return(misaligned_count)
}

count_misaligned <- list()
for (i in 1:nrow(df_groups)) {
  temp_df <- df[df_groups$start[i]:df_groups$end[i]] 
  temp_df$test <- apply(temp_df, 1, test_alignment )
  test <- sum(temp_df$test)
  print(i)
  print(test)
  count_misaligned <- c(count_misaligned, test)  
}



test = data.frame(matrix(runif(ncol(df)*nrow(df), min = -1, max = 1), ncol=ncol(df)))

#--------------------------------------------------------------------------------
box(title = "Parameters", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 4, height = 800,
    tags$h3("Set analysis parameters..."),
    br(),
    checkboxInput("primary_group", label = "Use only primary group for filter and impute", value = FALSE),
    br(),
    radioButtons("data_output", label = h3("Select Output Data Type"),
                 choices = list("Protein" = 1, "Peptide" = 2),
                 selected = 2),
    checkboxInput("norm_ptm", label = "Normalize on PTM?"),
    textInput("peptide_norm_grep", label = "Normalize PTM grep", value = "Enter value here"),
    checkboxInput("impute_ptm", label = "Impute Distribution based on PTM?"),
    textInput("peptide_impute_grep", label = "Impute PTM grep", value = "Enter value here"),
    selectInput("razor", label = h5("Peptides to Use"), 
                choices = list("Razor", "Unique", "Shared"), 
                selected = "Razor"),
    checkboxInput("tmt", label = "SPQC Normalized TMT sets"),
    checkboxInput("isoform", label = "Use Peptide Isoform?"),
)


test = readLines("error_setsamplegroups.txt")
cat(file = stderr(), readLines("error_setsamplegroups.txt"), "\n")
length(test)






#-----------------------------------------------------------------------
df_airquality  <- airquality
df_risk <- riskfactors
library(naniar)
vis_miss(airquality)

df_samples <- df[7:15]
vis_miss(df_samples)
gg_miss_upset(df_samples)
gg_miss_fct(x=df_samples)


missing.values <- df_samples %>%
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 


missing.values |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = key, y = num.missing), stat = 'identity') +
  ggplot2::labs(x = 'variable', y = "number of missing values", title = 'Number of missing values') +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot2::ggplot(df_samples) +
  ggplot2::geom_bar(ggplot2::aes(x = key, y = num.missing), stat = 'identity') +
  ggplot2::labs(x = 'variable', y = "number of missing values", title = 'Number of missing values') +
  ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot2::ggsave(stringr::str_c(params$qc_path, "missing_bar_plot.png"), width = 8, height = 8)

missing.values <- df_samples %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)

levels <-
  (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot


row.plot <- df_samples %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha = 0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Row Number", title = "Missing values in rows") +
  coord_flip()

row.plot

#--------------------------------------------------
df2 <- df |> dplyr::select(contains(c("Accession", "Description", "Gene", df_design$ID)))

norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])

for (norm in norm_type) {
  norm <- stringr::str_replace_all(norm, " ", "")
  cat(file = stderr(), stringr::str_c("norm_type = ", norm, ",   rollup_method = ", params$rollup_method), "\n")
}  
    

for (norm in norm_type) {
  norm <- stringr::str_replace_all(norm, " ", "")
  print(norm)
}     



#-------------------------------------------------
#inflection
library(inflection)

test <- df[,7:15]
test <- unlist(test, use.names = FALSE)
test <- test[test > 1]
test <- sort(test[!is.na(test)], decreasing = TRUE)
test <- log(test, 2)

testdf <- data.frame(test)
testdf$ID <- seq.int(nrow(testdf))

testdf <- testdf[1:(nrow(testdf)),]

x <- testdf$ID 
y <- testdf$test

cc = check_curve(x,y)
cc
cc$index

ipede = ede(x,y,cc$index)
ipede
ipede[3]

2^testdf$test[testdf$ID == floor(ipede[3])]

testdf2 <- testdf[ipede[3]:(nrow(testdf)),]

x <- testdf2$ID 
y <- testdf2$test

cc = check_curve(x,y)
cc
cc$index

ipede = ede(x,y,cc$index)
ipede
ipede[3]

inflection <- round(2^testdf$test[testdf$ID == floor(ipede[3])], digits = 0)

plotdf <- testdf[seq(1, nrow(testdf), 10),]

ggplot2::ggplot(plotdf, ggplot2::aes(x = ID, y = test)) +
  ggplot2::geom_point(ggplot2::aes(color = "blue")) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_vline(xintercept = ipede[3]) +
  ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", inflection ), x =
                  'Count', y = "Intensity") +
  ggplot2::theme(plot.title = element_text(hjust = 0.5))
  
  
ggplot2::ggsave(stringr::str_c(params$qc_path, "Inflection_Point.png"), width = 6, height = 8)


conn <- dbConnect(RSQLite::SQLite(), params$database_path)
tables_list <- dbListTables(conn)
RSQLite::dbDisconnect(conn)
"summary_cv" %in% tables_list




imgs_fct = function(){
  lapply(images, 
         function(x) {
           box(
             width = 12,
             status = "primary",
             renderImage({
               list(src = x, alt = "This is alternate text")}, deleteFile = F)
           )
         })
}

images <- c("test1", "test2")

imgs_fct = function(){
  lapply(counter, 
         function(x) {
           print(x)
         })
}

imgs_fct()

create_comp <- function(i) {
  fluidRow(
    i = 3,
    column(width = 2,
           tags$b(style = "color:blue; font-size:20px", 'Comparision ', i),
    ),
    column(width = 3,
           pickerInput(inputId = str_c("comp_", i, "N"), label = "Numerator",  choices = "None", 
                       options = list(`actions-box` = TRUE,size = 10,
                                      `selected-text-format` = "count > 7"),  multiple = TRUE)
    ),
    column(width = 3,
           pickerInput(inputId = str_c("comp_", i, "D"), label = "Denominator",  choices = "None", 
                       options = list(`actions-box` = TRUE,size = 10,
                                      `selected-text-format` = "count > 7"),  multiple = TRUE)
    ),
    column(width = 3,
           textInput(str_c("comp", i, "_name"), label = "Description", value = "")
    )
  )
}


create_imputed_df <- function(info_columns, df) {
  cat(file = stderr(), "function create_imputed_column....", "\n")
  
  df_protein_info <- df |> dplyr::select(contains(c("Accession", "Description", "Genes")))
  df <- df[(info_columns + 1):ncol(df)]
  
  df[df > 0] <- 1
  df[is.na(df)] <- 0
  
  df_protein <- cbind(df_protein_info, df)
  df_protein <- df_protein |> dplyr::group_by(Accession, Description, Genes) |> dplyr::summarise_all(list(sum))
  df_protein <- data.frame(dplyr::ungroup(df_protein))
  df_protein <- df_protein[4:ncol(df_protein)]

  reduce_df <- function(df) {
    
    df[df == 0] <- "-"
    df <- df |> dplyr::mutate_all(as.character)
    
    while (ncol(df) > 1) {
      df[,1] <- stringr::str_c(df[,1], ".", df[,2])
      df[,2] <- NULL
    }
    colnames(df) <- "Detected_Imputed"
    return(df)
  } 
  
  return(list(reduce_df(df), reduce_df(df_protein)))
}


#------------------------------------------------------

str_to_num <- function(df, str_list){
  
  col_select <- strsplit(unlist(str_list), ",")
  col_select <- as.numeric(unlist(col_select))
  
  return(df[,col_select])
}

test <- str_to_num(df_missing, stats_comp_df$N_loc[1])

test <- df_missing[stats_comp_df$N_loc[1]]

testme2 <- strsplit(unlist(stats_comp_df$N_loc[1]), ",")
testme3 <- as.numeric(unlist(testme2))

test <- df_missing[,testme3]

df_list = list(df_missing, df_missing2)
test <- df_list[[1]]

test1 <- df_missing[,1:3]
test2 <- df_missing[,4:6]
sumtest <- rowSums(test1) + rowSums(test2)
test3 <- 0
test3 <- which((rowSums(test1) + rowSums(test2)) == 0)
test4 <- missing_factor_gw(test1, test2)
test5 <- which(test4 < 0.3)

test1 <- df[,7:9]
test2 <- df[,10:12]
test6 <- percentCV_gw(test1)
test7 <- percentCV_gw(test2)
test8 <- pmin(percentCV_gw(test1), percentCV_gw(test2))
test9 <- which(test8 > 70)

test10 <- unique(sort(c(test9, test5)))


test11 <- df[-test9,]


t1 <- c(1,2,3)
t2 <- c(4,5,3)
t3 <- c(t1,t2)
t4 <- sort(t3)
t5 <- unique(t4)

t6 <- reduce_imputed_df(df_missing)


plotdf <- df2[seq(1, nrow(df2), 10),]

ggplot2::ggplot(plotdf, ggplot2::aes(x = ID, y = vec)) +
  ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_vline(xintercept = ipede[3]) +
  ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                  'Count', y = "Intensity") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
ggplot2::ggsave(stringr::str_c(params$qc_path, "Inflection_Point.png"), width = 8, height = 6)


ipese = ese(df2$ID, df2$vec, cc2$index, doparallel = TRUE)
ipede = bede(df$ID, df$ved, cc$index)
ipbede2 = bede(df2$ID, df2$vec, cc2$index)

test = nrow(df2)
df3 = df2[(test/2):(nrow(df2)),]
cc3 <- check_curve(df3$ID, df3$vec)

df2 <- df[ipede[3]:(nrow(df)),]

cc2 <- check_curve(df2$ID, df2$vec)
cat(file = stderr(), stringr::str_c("inflection, check_curve = ", cc2$ctype), "\n")
cat(file = stderr(), stringr::str_c("inflection, index = ", cc2$index), "\n")

ipede3 = ede(df3$ID, df3$vec, cc3$index)


test = read_table("precursor_start")
raw_peptide <- collapse_precursor_raw(test, info_columns = 0, stats = FALSE, params)
write_table("raw_peptide", raw_peptide)

test1 = read_table("protein_sltmm")
test2 = read_table("protein_sltmm_cv")

#combine df and cv data
test <- cbind(test1, test2[,2:ncol(test2)])
write_table("protein_sltmm_final", test)



library(rWikiPathways)
listPathways('Homo sapiens')
listOrganisms()
listPathwayUrls()

wp.gmt <- downloadPathwayArchive(date="20240910", organism="Mus musculus", format="gmt", destpath = params$string_path) ## download file
wp2gene <- clusterProfiler::read.gmt(str_c(params$string_path, wp.gmt))


p <- barplot(ggo, title = stringr::str_c("Go Profile"), drop=TRUE, showCategory=12, order=TRUE)
p
#---------------------------------------------

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
df2$phos <- grepl("Phospho", df_notlocal$EG.ModifiedSequence)
df2$precursorID <- df_notlocal$EG.PrecursorId

df2$duplicates <- duplicated(df2$precursorID)

df2$max_local <- apply(df2, 1, max)
to_delete <- which(df2$phos == "TRUE" & df2$max_local < 0.75)
df_notlocal2 <- df_notlocal[-to_delete]

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



zip(zipfile = 'testZip', files = str_c(getwd(), '/data'))
getwd()


#----------------------------------------------
Simple_Excel <- function(df, sheetname, filename) {
  cat(file=stderr(), stringr::str_c("Simple_Excel -> ", filename), "\n")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet=1, df)  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

name = str_c(params$data_path, "eraseme")
Simple_Excel(params, "stuff", file_name)
create_dir(name)


#----------------------------------------------------------------------------------------
create_dir <- function(name){
  cat(file = stderr(), "Function create_dir...", "\n")
  if (fs::is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists, deleting...", "\n")
    do.call(file.remove, list(list.files(name, full.names = TRUE)))
    dir_delete(name)
    dir_create(name)
  }else{
    dir_create(name)
  }
  name <- str_replace_all(name, "/", "//")
  name <- str_c(name, "//")
  cat(file = stderr(), str_c(name, " created...", "\n"))
  
  cat(file = stderr(), "Function create_dir...end", "\n\n")
  return(name)
}








df2 <- df[seq(1, nrow(df), 100),]
df2$ID <- seq.int(nrow(df2))
rownames(df2) <- NULL

df_row <- nrow(df2)
for (i in (1:100)) {
  m <- (df2$vec[df_row] - df2$vec[df_row-20]) / (df2$ID[df_row] - df2$ID[df_row - 20]) *1000
  df_row <- df_row - 10
  noise <- round(2^df2$vec[df_row], digits = 2)
  cat(file = stderr(), stringr::str_c("i <- ", i, "m<- ", m, "   noise <- ", noise), "\n")
  if (m > -4) {break}
}

params$noise_inflection <- noise

ggplot2::ggplot(df2, ggplot2::aes(x = ID, y = vec)) +
  ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_vline(xintercept = df_row) +
  ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                  'Count', y = "Intensity") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))









plotdf <- df[seq(1, nrow(df), 100),]

df2 <- df2[as.integer(nrow(df2)*.995):(nrow(df2)),]
#   df2 = df2[(nrow(df2)/):(nrow(df2)),]

cc2 <- check_curve(df2$ID, df2$vec)
cat(file = stderr(), stringr::str_c("inflection, check_curve = ", cc2$ctype), "\n")
cat(file = stderr(), stringr::str_c("inflection, index = ", cc2$index), "\n")

ipede2 = ede(df2$ID, df2$vec, cc2$index)
cat(file = stderr(), stringr::str_c("ipede2 = ", ipede2[3]), "\n")

params$noise_inflection <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)

ggplot2::ggplot(df3, ggplot2::aes(x = ID, y = vec)) +
  ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_vline(xintercept = ipede2[3]) +
  ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                  'Count', y = "Intensity") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

ggplot2::ggplot(plotdf, ggplot2::aes(x = ID, y = vec)) +
  ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_vline(xintercept = ipede2[3]) +
  ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                  'Count', y = "Intensity") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

loess()
erase <- loess(vec ~ ID, df)

# test inflection point, if above threshold need to calc again
test <- 0
if(!is.na(ipede2[3])) {
  test <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)
  cat(file = stderr(), stringr::str_c("noise test = ", test), "\n")
}

if (is.na(ipede2[3]) | test > 50) {
  for (i in (1:10)){
    df2 = df2[(nrow(df2)/2):(nrow(df2)),]
    cc2 <- check_curve(df2$ID, df2$vec)
    cat(file = stderr(), stringr::str_c("rerun ", i, "  inflection, check_curve = ", cc2$ctype), "\n")
    cat(file = stderr(), stringr::str_c("inflection, index = ", cc2$index), "\n")
    
    ipede2 = ede(df2$ID, df2$vec, cc2$index)
    cat(file = stderr(), stringr::str_c("ipede2 = ", ipede2[3]), "\n")
    if (!is.na(ipede2[3])) { 
      test <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)
      if (test < 50 ) {break} 
    } 
  }
}

cat(file = stderr(), stringr::str_c("ede inflection = ", ipede2[3]), "\n")
cat(file = stderr(), stringr::str_c("ede inflection value = ", 2^df2$vec[df2$ID == floor(ipede2[3])]), "\n")

params$noise_inflection <- round(2^df2$vec[df2$ID == floor(ipede2[3])], digits = 2)

# count data points below inflection 
params$noise_count = nrow(df) - floor(ipede2[3])
params$noise_total = nrow(df)
cat(file = stderr(), stringr::str_c("noise data points to be removed = ", params$noise_count, " out of ", nrow(df)), "\n")

plotdf <- df[seq(1, nrow(df), 10),]

RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
RSQLite::dbDisconnect(conn)

ggplot2::ggplot(plotdf, ggplot2::aes(x = ID, y = vec)) +
  ggplot2::geom_point(ggplot2::aes(colour = "blue") ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::geom_vline(xintercept = ipede2[3]) +
  ggplot2::labs(title = stringr::str_c("Dataset Values - Inflection = ", params$noise_inflection), x =
                  'Count', y = "Intensity") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))




df <- read_table_try('precursor_start', params)
df <- read_table('precursor_filter', params)
df <- read_table('precursor_normdata', params)

df_data <- df[,10:ncol(df)]
sum_all <- sum(df_data, na.rm = TRUE)

df_phos <- df[grepl(params$ptm_grep, df$Sequence, ignore.case = TRUE),]
df_phos_data <- df_phos[,10:ncol(df_phos)]
sum_phos <- sum(df_phos_data, na.rm = TRUE)

sum_phos/sum_all


t1 <- df_filter_list[[1]]
t2 <- df_filter_list[[2]]
t3 <- df_list[[3]]
t4 <- df_list[[4]]

list_tables(db_path)
df_raw <- read_table_try('precursor_raw', params)
df_start <- read_table_try('precursor_start', params)
df_noise <- read_table_try('precursor_noise', params)
df_filter <- read_table_try('precursor_filter', params)
df_norm <- read_table_try('precursor_norm_sltmm', params)

which(is.na(df_noise$Local))
which(is.na(df_noise$Local2))

which(is.na(df_filter$Local))
which(is.na(df_filter$Local2))

which(is.na(df_norm$Local))
which(is.na(df_norm$Local2))


dl1 <- df_list[[1]]
dl2 <- df_list[[2]]
dl3 <- df_list[[3]]
dl4 <- df_list[[4]]


testme <-  data.frame(foreground_Seqs_Filtered)
filename <- stringr::str_c(params$phos_path, "testme.xlsx")
filename2 <- stringr::str_c(params$phos_path, "testme.tsv")
Simple_Excel(testme, "data", filename)
write.table(testme, file=filename2, quote=FALSE, sep='\t', col.names = NA)





df <- read_table_try('precursor_start', params)
test_df <- df |> dplyr::select(contains(c('Accession', 'Genes', 'Local', 'Protein_PTM_Loc')))
test_df$Local2 <- NULL
test_df$Accession <- gsub(";.*$", "", test_df$Accession)
test_df$Genes <- gsub(";.*$", "", test_df$Genes)
test_df$Local <- gsub(";.*$", "", test_df$Local)
test_df$Protein_PTM_Loc <- gsub(";.*$", "", test_df$Protein_PTM_Loc)

find_mult <- which(grepl(",", df$Protein_PTM_Loc))
new_df <- test_df[-find_mult,]
new_df$Phos_ID <- paste(new_df$Accession, "_", new_df$Genes, "_", new_df$Protein_PTM_Loc, sep = "")

for(r in find_mult) {
  sites <- unlist(stringr::str_split(test_df$Protein_PTM_Loc[r], ","))
  site_local <- unlist(stringr::str_split(test_df$Local[r], ","))
  for (i in (1:length(sites))){
    new_df <- rbind(new_df, c(test_df$Accession[r], test_df$Genes[r], site_local[i], test_df$Protein_PTM_Loc[r], 
                              paste(test_df$Accession[r], "_", test_df$Genes[r], "_", sites[i], sep = "")))
  }
}

phos_unique_all <- unique(new_df$Phos_ID)
phos_unique_local <- unique(new_df[new_df$Local > 0.75,]$Phos_ID)

test <- params$design_path
test2 <- unlist(str_split(test, "/"))
test3 <- test2[nzchar(test2)]
test3[length(test3)]

fs::is_dir(params$design_path)


#-------------------------------------------------------------------------------------------
# count lines in all *.R files in the current directory

#-------------------------------------------------------------------------------------------
# Create a function to count lines in a file
count_lines <- function(file) {
  lines <- readLines(file)
  return(length(lines))
}
#-------------------------------------------------------------------------------------------
# Get a list of all *.R files in the current directory
#-------------------------------------------------------------------------------------------
r_files <- list.files(pattern = "\\.R$", full.names = TRUE)
#-------------------------------------------------------------------------------------------
# Use lapply to apply the function to each file and get the results
#-------------------------------------------------------------------------------------------
line_counts <- lapply(r_files, count_lines)
#-------------------------------------------------------------------------------------------
# Combine the results into a data frame
#-------------------------------------------------------------------------------------------
line_counts_df <- data.frame(
  File = r_files,
  Lines = unlist(line_counts)
)
#-------------------------------------------------------------------------------------------
# Print the data frame
#-------------------------------------------------------------------------------------------
print(line_counts_df)
print(sum(line_counts_df$Lines))
print(sum(line_counts_df$Lines)/35)



#-------------------------------------------------------------------------------------------
p = df$K10_v_Ki17778_pval
#sort p 
p = sort(p)
adjpval = p.adjust(p, method = "fdr")


#-------------------------------------------------------------------------------------------
t1=1
t2=2
t3=3

test1 <- function() {
  t1 <- 4
  t2 <- 5
  t3 <- 6
  test2()
}

test2 <- function() {
  t1 <<- 7
  t2 <<- 8
  t3 <<- 9
  test3()
}

test3 <- function() {
  t1 <<- 10
  t2 <<- 11
  t3 <<- 12
  t4 <- 13
  assign("t4", t4, envir = .GlobalEnv)
}

test1()
test2()
#-------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------t

testme <- 10

test <- function(testme){

  bg_test <- callr::r_bg(test_bg, args = list(testme), stderr = str_c("eraseme.txt"), supervise = TRUE)
  bg_test$wait()
  error_list = readLines(stringr::str_c('eraseme.txt'))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }

  return(bg_test$get_result())
}

test2 <- function(testme){
  testme <- testme + 2
  return(testme)
}


#----------------------------------------------------------------------------------------
test_bg <- function(testme){
  source("scratch.R")
  return(test2(testme))
}

result = test(testme)

#----------------------------------------------------------------------------------------
# Function to delete and recreate a directory
manage_directory <- function(dir_path) {
  # Convert to absolute path for safety
  dir_path <- normalizePath(dir_path, mustWork = FALSE)
  
  # Check if the directory exists
  if (dir.exists(dir_path)) {
    cat("Directory exists:", dir_path, "\n")
    cat("Removing directory and all its contents...\n")
    
    # Use unlink with recursive=TRUE to delete the directory and all its contents
    unlink(dir_path, recursive = TRUE)
    
    if (dir.exists(dir_path)) {
      stop("Failed to delete directory:", dir_path)
    } else {
      cat("Directory successfully deleted.\n")
    }
  } else {
    cat("Directory does not exist:", dir_path, "\n")
  }
  
  # Create the directory
  cat("Creating directory:", dir_path, "\n")
  dir.create(dir_path, recursive = TRUE)
  
  if (dir.exists(dir_path)) {
    cat("Directory successfully created.\n")
  } else {
    stop("Failed to create directory:", dir_path)
  }
  
  return(invisible(dir_path))
}
manage_directory(name)

# Example usage
# Replace with your directory path
# manage_directory("path/to/your/directory")


# Function to delete and recreate a directory using system commands
manage_directory <- function(dir_path) {
  # Convert to absolute path for safety
  dir_path <- normalizePath(dir_path, mustWork = FALSE)
  
  # Check if the directory exists
  if (dir.exists(dir_path)) {
    cat("Directory exists:", dir_path, "\n")
    cat("Removing directory and all its contents...\n")
    
    # Use system commands to delete the directory and all its contents
    if (.Platform$OS.type == "windows") {
      # Windows command
      system2("cmd", args = c("/c", paste0("rmdir /s /q \"", dir_path, "\"")), invisible = FALSE)
    } else {
      # Unix/Linux/Mac command
      system2("rm", args = c("-rf", shQuote(dir_path)), invisible = FALSE)
    }
    
    if (dir.exists(dir_path)) {
      stop("Failed to delete directory:", dir_path)
    } else {
      cat("Directory successfully deleted.\n")
    }
  } else {
    cat("Directory does not exist:", dir_path, "\n")
  }
  
  # Create the directory using system commands
  cat("Creating directory:", dir_path, "\n")
  
  if (.Platform$OS.type == "windows") {
    # Windows command
    system2("cmd", args = c("/c", paste0("mkdir \"", dir_path, "\"")), invisible = FALSE)
  } else {
    # Unix/Linux/Mac command
    system2("mkdir", args = c("-p", shQuote(dir_path)), invisible = FALSE)
  }
  
  if (dir.exists(dir_path)) {
    cat("Directory successfully created.\n")
  } else {
    stop("Failed to create directory:", dir_path)
  }
  
  return(invisible(dir_path))
}


#transpose test into a dataframe with column names: params, values
#----------------------------------------------------------------------------------------
# Function to transpose a list into a data frame with column names
transpose_list_to_df <- function(input_list) {
  # Convert the list to a data frame
  df <- as.data.frame(do.call(rbind, input_list), stringsAsFactors = FALSE)
  
  # Set the column names
  colnames(df) <- c("params", "values")
  
  return(df)
}



test3 <- data.frame(t(test))
test3$param <- rownames(test3)
rownames(test3) <- NULL
test3 <- test3[,c(2,1)]
colnames(test3) <- c("params", "values")


#check if file.txt exists in /database folder
file.exists(str_c(database_dir, "/string_db"))
database_dir

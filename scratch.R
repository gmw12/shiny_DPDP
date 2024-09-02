
read_table <- function(table_name){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  df <- dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

write_table <- function(table_name, df, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

list_tables <- function(table_name, params){
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  table_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}

filter_db <- function(table_name, column_name, key_word, params) {
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT * FROM ", table_name, " WHERE ", column_name, " LIKE '", accession,"'") 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}

test <- filter_db(data_name, "Accession", accession, params)

list_tables(params)


testme <- read_table("precursor_impute_sltmm", params)
testme2 <- read_table("temp_df_impute", params)
df_design <- read_table("design", params)
test_sample_groups <- read_table("sample_groups", params)
stats_comp <- read_table("stats_comp", params)
test_design <- read_table("design", params)
test_protein_missing <- read_table("protein_missing", params)

stats_comp <- read_table("stats_comp", params)
sample_groups <- read_table("sample_groups", params)

df <- read_table('precursor_impute_sltmm', params)
test3 <- read_table('protein_sltmm', params)
test4 <- read_table('protein_sltmm_final', params)
df <- read_table('protein_sltmm_Caskin1_Test_v_Caskin1_Ctrl_final', params)

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

df <- dbReadTable(conn, "parameters")
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

#-------------------------------------

# Server.R
shinyServer(function(input, output, session) {
  output$sampletable <- DT::renderDataTable({
    sampletable
  }, server = TRUE, selection = 'single')
  
  output$selectedrow <- DT::renderDataTable({
    selectedrowindex <<-
      input$sampletable_rows_selected[length(input$sampletable_rows_selected)]
    selectedrowindex <<- as.numeric(selectedrowindex)
    selectedrow <- (sampletable[selectedrowindex, ])
    selectedrow
  })
  
  output$plots <- renderPlot({
    variable <- sampletable[selectedrowindex, 1]
    #write your plot function
    
    
  })
  
  
})

#ui.R
shinyUI(navbarPage(
  "Single Row Selection",
  
  
  
  tabPanel(
    "Row selection example",
    sidebarLayout(
      sidebarPanel("Parameters"),
      mainPanel(
        DT::dataTableOutput("selectedrow"),
        DT::dataTableOutput("sampletable")
        
      )
    )
    
  )
  
))

# global.R

library(DT)
library(shiny)
selectedrowindex = 0



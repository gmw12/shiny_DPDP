
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

RSQLite::dbDisconnect(conn)

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

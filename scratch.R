
conn <- dbConnect(RSQLite::SQLite(), params$database_path)
dbListTables(conn)

df <- dbReadTable(conn, "parameters")
df <- dbReadTable(conn, "precursor_raw")
df <- dbReadTable(conn, "precursor_start")
df <- dbReadTable(conn, "precursor_filter")
df_design <- dbReadTable(conn, "design")
df_groups <- dbReadTable(conn, "sample_groups")

df <- dbReadTable(conn, "precursor_filter")




RSQLite::dbDisconnect(conn)

df <- df[(ncol(df) - params$sample_number+1):ncol(df)]
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

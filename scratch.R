
conn <- dbConnect(RSQLite::SQLite(), params$database_path)
dbListTables(conn)

df <- dbReadTable(conn, "parameters")


RSQLite::dbDisconnect(conn)

#--------------------------------------------------------------------------------
box(title = "Parameters", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 4, height = 800,
    tags$h3("Set analysis parameters..."),
    br(),
    checkboxInput("primary_group", label = "Use only primary group for filter and impute", value = FALSE),
    br(),
    radioButtons("data_output", label = h3("Select Output Data Type"),
                 choices = list("Protein" = 1, "Peptide" = 2),
                 selected = 2),
    checkboxInput("checkbox_norm_ptm", label = "Normalize on PTM?"),
    textInput("peptide_norm_grep", label = "Normalize PTM grep", value = "Enter value here"),
    checkboxInput("checkbox_impute_ptm", label = "Impute Distribution based on PTM?"),
    textInput("peptide_impute_grep", label = "Impute PTM grep", value = "Enter value here"),
    selectInput("razor", label = h5("Peptides to Use"), 
                choices = list("Razor", "Unique", "Shared"), 
                selected = "Razor"),
    checkboxInput("checkbox_tmt", label = "SPQC Normalized TMT sets"),
    checkboxInput("checkbox_isoform", label = "Use Peptide Isoform?"),
)
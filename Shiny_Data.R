cat(file = stderr(), "Shiny_Data.R", "\n")

#---------------------------------------------------------------------
load_data_file <- function(session, input, output){
  cat(file = stderr(), "Function load_data_file", "\n")
  showModal(modalDialog("Loading data...", footer = NULL))
  
  params$data_source <<- "unkown"
  data_sfb <- parseFilePaths(volumes, input$sfb_data_file)
  data_path <- str_extract(data_sfb$datapath, "^/.*/")
  params$data_file <<- basename(data_sfb$datapath)
  
  cat(file = stderr(), str_c("loading data file(s) from ", data_path[1]), "\n")
  
  #Proteome Discoverer
  if (nrow(data_sfb) > 1) {
    cat(file = stderr(), stringr::str_c("data source <---- Proteome Discoverer"), "\n")
    params$data_source <<-  "PD"
    load_PD_data(data_sfb)
  }else {
    cat(file = stderr(), str_c("data source <---- Spectronaut/Fragpipe"), "\n")
    bg_load_unknown_data <- callr::r_bg(func = load_unknown_data, args = list(data_sfb, params), stderr = str_c(params$error_path, "//error_load_unknown_data.txt"), supervise = TRUE)
    bg_load_unknown_data$wait()
    print_stderr("error_load_unknown_data.txt")
  }
  
  #parameters are written to db during r_bg (process cannot write to params directly)
  params <<- param_load_from_database()
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function load_data_file...end", "\n\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
load_unknown_data <- function(data_sfb, params){
  cat(file = stderr(), "Function load_unkown_data...", "\n")
  
  df <- data.table::fread(file = data_sfb$datapath, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
  
  if ("EG.PrecursorId" %in% names(df)) {
    cat(file = stderr(), "Spectronaut data, precursor...", "\n")
    
    params$data_source <- "SP"
    params$raw_data_format <- "precursor"
    
    if ("PTMProbabilities" %in% names(df)) {
      params$ptm <- TRUE
    }else {
      params$ptm <- FALSE
    }
    
    if (length(grep("EG.TotalQuantity", names(df))) > 1) {
      params$data_table_format <- "short"
    }else {
      params$data__table_format <- "long"
    }
  }
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  RSQLite::dbWriteTable(conn, "precursor_raw", df, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "function load_unkown_data...end", "\n\n")
}



#----------------------------------------------------------------------------------------
load_PD_data <- function(data_sfb){
  cat(file = stderr(), "function load_PD_data...", "\n")
  start_time <- Sys.time()
  
  #Loop through and load data in background
  for (i in 1:nrow(data_sfb) ) {
    raw_name <- data_sfb$datapath[i]
    if (grepl("_PeptideGroups.txt", raw_name)) {
      cat(file = stderr(), "loading raw peptide data...", "\n")
      temp_df1 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("_Proteins.txt", raw_name)) {
      cat(file = stderr(), "loading raw protein data...", "\n")
      temp_df2 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("_PSMs.txt", raw_name)) {
      cat(file = stderr(), "loading raw psm data...", "\n")
      temp_df3 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("MSMSSpectrumInfo.txt", raw_name)) {
      cat(file = stderr(), "loading raw msms data...", "\n")
      temp_df4 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("InputFiles.txt", raw_name)) {
      cat(file = stderr(), "loading raw inputfile data...", "\n")
      temp_df5 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("_DecoyPeptideGroups.txt", raw_name)) {
      cat(file = stderr(), "loading raw decoy peptide data...", "\n")
      temp_df6 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("_DecoyProteins.txt", raw_name)) {
      cat(file = stderr(), "loading raw decoy protein data...", "\n")
      temp_df7 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("_DecoyPSMs.txt", raw_name)) {
      cat(file = stderr(), "loading raw decoy psm data...", "\n")
      temp_df8 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    } else if (grepl("_PeptideIsoforms.txt", raw_name)) {
      cat(file = stderr(), "loading raw peptide isoform data...", "\n")
      temp_df9 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    }else if (grepl("_LCMSFeatures.txt", raw_name)) {
      cat(file = stderr(), "loading feature data...", "\n")
      temp_df10 <- callr::r_bg(func = Simple_fread, args = list(file = raw_name), supervise = TRUE)
    }
  }  
  #loop back through and write from background process
  conn <- dbConnect(RSQLite::SQLite(), database_path)
  
  for (i in 1:nrow(data_sfb) ) {
    raw_name <- data_sfb$datapath[i]
    if (grepl("_PeptideGroups.txt", raw_name)) {
      cat(file = stderr(), "assign raw peptide data...", "\n")
      temp_df1$wait()
      df <- temp_df1$get_result() %>% dplyr::select(contains(
        c("Confidence", "Accession", "Description", "Sequence", "Modifications", "Positions", "Abundance.F", "Retention.Time", "Ion.Score", "Percolator.SVM",
          "q.Value", "RT.in.min", "mz.in.Da.by.Search.Engine.", "Charge.by.Search.Engine.", "Quan.Info")
      ))
      df <- as.data.frame(df)
      dbWriteTable(conn, "raw_peptide", df, overwrite = TRUE)
    } else if (grepl("_Proteins.txt", raw_name)) {
      cat(file = stderr(), "assign raw protein data...", "\n")
      temp_df2$wait()
      df <- temp_df2$get_result() %>% dplyr::select(contains(
        c("Master", "Protein.FDR.Confidence.Combined", "Number.of.Razor.Peptides", "Accession", "Description",
          "Number.of.Peptides", "Coverage.in.Percent", "Number.of.Unique.Peptides", "Number.of.Razor.Peptides",
          "Number.of.Protein.Unique.Peptides", "Abundance.F")
      ))
      df <- as.data.frame(df)
      dbWriteTable(conn, "raw_protein", df, overwrite = TRUE)
    } else if (grepl("_PSMs.txt", raw_name)) {
      cat(file = stderr(), "assign raw psm data...", "\n")
      temp_df3$wait()
      df <- as.data.frame(temp_df3$get_result())
      dbWriteTable(conn, "raw_peptide", df, overwrite = TRUE)
    } else if (grepl("MSMSSpectrumInfo.txt", raw_name)) {
      cat(file = stderr(), "assign raw msms data...", "\n")
      temp_df4$wait()
      df <- as.data.frame(temp_df4$get_result())
      dbWriteTable(conn, "raw_msms", df, overwrite = TRUE)
    } else if (grepl("InputFiles.txt", raw_name)) {
      cat(file = stderr(), "assign raw inputfile data...", "\n")
      temp_df5$wait()
      df <- as.data.frame(temp_df5$get_result())
      dbWriteTable(conn, "raw_inputfile", df, overwrite = TRUE)
    } else if (grepl("_DecoyPeptideGroups.txt", raw_name)) {
      cat(file = stderr(), "assign raw decoy peptide data...", "\n")
      temp_df6$wait()
      df <- as.data.frame(temp_df6$get_result())
      dbWriteTable(conn, "raw_decopypeptide", df, overwrite = TRUE)
    } else if (grepl("_DecoyProteins.txt", raw_name)) {
      cat(file = stderr(), "assign raw decoy protein data...", "\n")
      temp_df7$wait()
      df <- as.data.frame(temp_df7$get_result())
      dbWriteTable(conn, "raw_decopyprotein", df, overwrite = TRUE)
    } else if (grepl("_DecoyPSMs.txt", raw_name)) {
      cat(file = stderr(), "assign raw decoy psm data...", "\n")
      temp_df8$wait()
      df <- as.data.frame(temp_df8$get_result())
      dbWriteTable(conn, "raw_decoypsm", df, overwrite = TRUE)
    } else if (grepl("_PeptideIsoforms.txt", raw_name)) {
      cat(file = stderr(), "assign raw peptide isoform data...", "\n")
      temp_df9$wait()
      df <- temp_df9$get_result() %>% dplyr::select(contains(
        c("Confidence", "Accession", "Description", "Sequence", "Modifications", "Positions", "Abundance.F", "Retention.Time", "Ion.Score", "Percolator.SVM",
          "q.Value", "RT.in.min", "mz.in.Da.by.Search.Engine.", "Charge.by.Search.Engine.", "Quan.Info")
      ))
      df <- as.data.frame(df)
      dbWriteTable(conn, "raw_isoform", df, overwrite = TRUE)
    }else if (grepl("_LCMSFeatures.txt", raw_name)) {
      cat(file = stderr(), "assign feature data...", "\n")
      temp_df10$wait()
      df <- as.data.frame(temp_df10$get_result())
      dbWriteTable(conn, "features", df, overwrite = TRUE)
    }
  }
  
  #backup text files
  cat(file = stderr(), "backing up text files...", "\n")
  backup_path <- param_query("backup_path")
  
  for (i in 1:nrow(data_sfb) ) {
    data_file = data_sfb$datapath[i]
    bg_savedata <- callr::r_bg(func = save_data_bg, args = list(file1 = data_file, dir1 = backup_path), supervise = TRUE)
  }
  
  
  
  RSQLite::dbDisconnect(conn)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), str_c("Data load time ---> ", Sys.time() - start_time), "\n")
  cat(file = stderr(), "function load_PD_data...end", "\n")
}


#--------------------------------------------------------
meta_data <- function(table_string){
  cat(file = stderr(), "Function meta_data...", "\n")
  showModal(modalDialog("Gathering meta data...", footer = NULL))
  
  table_name <- str_c("precursor_", table_string)
  error_file <- str_c("error_", table_string, "meta.txt")
  
  bg_meta <- callr::r_bg(func = meta_data_bg, args = list(table_name, table_string, params), stderr = str_c(params$error_path, "//", error_file), supervise = TRUE)
  bg_meta$wait()
  print_stderr(error_file)
  
  params <<- param_load_from_database()
  
  cat(file = stderr(), "Function meta_data...end", "\n\n")
  removeModal()
}

#--------------------------------------------------------
meta_data_bg <- function(table_name, data_format, params){
  cat(file = stderr(), "Function meta_data bg...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)

  precursor_name <- stringr::str_c("meta_precursor_", data_format)
  peptide_name <- stringr::str_c("meta_peptide_", data_format)
  protein_name <- stringr::str_c("meta_protein_", data_format)
  
  params[[precursor_name]] <- nrow(df)
  
  if (data_format == "raw") {
    params[[peptide_name]] <- length(unique(df$EG.ModifiedSequence))
    params[[protein_name]] <- length(unique(df$PG.ProteinAccessions))
  } else {
    params[[peptide_name]] <- length(unique(df$Sequence))
    params[[protein_name]] <- length(unique(df$Accession))
  }
  
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "Function meta_data bg...end", "\n\n")
  
}

#--------------------------------------------------------

#--------------------------------------------------------
impute_meta_data <- function(){
  cat(file = stderr(), "Function impute_meta_data...", "\n")
  showModal(modalDialog("Setting imputation parameters, calculating meta data, setting Duke impute intensity table...", footer = NULL))
  
  bg_meta <- callr::r_bg(func = impute_meta_data_bg, args = list("precursor_filter", params), stderr = str_c(params$error_path, "//impute_meta_data.txt"), supervise = TRUE)
  bg_meta$wait()
  print_stderr("impute_meta_data.txt")
  
  params <<- param_load_from_database()
  
  removeModal()
  cat(file = stderr(), "Function impute_meta_data...end", "\n\n")
}

#--------------------------------------------------------
impute_meta_data_bg <- function(table_name, params){
  cat(file = stderr(), "Function impute_meta_data bg...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  df_groups <- RSQLite::dbReadTable(conn, "sample_groups")

  #filter data if imputation stats based on modification data only
  if (params$impute_ptm) {
    df <- df[grep(params$impute_ptm_grep, df$Sequence, ignore.case = TRUE),]
  }
  
  df <- df[(ncol(df) - params$sample_number + 1):ncol(df)]
  df <- log(df,2)
  
  total_na <- list()
  for (i in 1:nrow(df_groups)) {
    temp_df <- df[df_groups$start[i]:df_groups$end[i]] 
    count_na <- sum(is.na(temp_df))
    total_na <- c(total_na, count_na)
    
    temp_df$average <- apply(temp_df, 1, FUN = function(x) {mean(x, na.rm = TRUE)})
    temp_df$sd <- apply(temp_df, 1, FUN = function(x) {sd(x, na.rm = TRUE)})
    temp_df$bin <- dplyr::ntile(temp_df$average, 20)  

    impute_bin_table <- subset(temp_df, !is.na(sd)) |> dplyr::group_by(bin) |> dplyr::summarize(min = min(average), max = max(average), sd = mean(sd))
    for (x in 1:(nrow(impute_bin_table) - 1)) {impute_bin_table$max[x] <- impute_bin_table$min[x + 1]}
    impute_bin_table$max[nrow(impute_bin_table)] <- 100
    impute_bin_table$min2 <- impute_bin_table$min
    impute_bin_table$min2[1] <- 0
    impute_bin_table <- impute_bin_table[-21,]
    
    impute_bin_table_name <- stringr::str_c("impute_bin_", df_groups$Group[i])
    RSQLite::dbWriteTable(conn, impute_bin_table_name, impute_bin_table, overwrite = TRUE)
  } 

  params$meta_impute_na <- toString(total_na)
  
  #create random table for imputation
  set.seed(123)
  rand_df = data.frame(matrix(runif(ncol(df) * nrow(df), min = -1, max = 1), ncol = ncol(df)))
  
  RSQLite::dbWriteTable(conn, "random", rand_df, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "Function impute_meta_data bg...end", "\n\n")
  
}

#--------------------------------------------------------








#----------------------------------------------------------------------------------------------------------
protein_to_peptide <- function(){
  cat(file = stderr(), "protein_to_peptide", "\n")
  protein <- dpmsr_set$data$data_raw_protein
  peptide_groups <- dpmsr_set$data$data_raw_peptide
  
  #add columns to preserve peptide to protein links
  peptide_groups$Proteins <- peptide_groups$Protein.Accessions
  peptide_groups$Unique <- peptide_groups$Quan.Info
  peptide_groups$Unique[peptide_groups$Unique == ""] <- "Unique"
  
  #protein raw has all confidence proteins - limit to high master
  protein_master <- subset(protein, Master %in% ("IsMasterProtein"))
  protein_high_master <- subset(protein_master, Protein.FDR.Confidence.Combined %in% ("High"))
  master_accessions <- protein_high_master$Accession 
  
  #PD will label which proteins get the razor peptides, 
  protein_razor <- subset(protein, Number.of.Razor.Peptides > 0)
  razor_accessions <- protein_razor$Accession
  
  #gather peptides that are shared
  peptide_shared <- subset(peptide_groups,  Quan.Info %in% ("NotUnique"))
  #gather peptides that have no quant values
  peptide_noquan <- subset(peptide_groups,  Quan.Info %in% ("NoQuanValues"))
  #gather unique peptides
  peptide_unique <- peptide_groups[peptide_groups$Quan.Info == "",]
  
  #expand shared peptides so that each protein has peptides listed separately
  peptide_shared_expand  <- peptide_shared %>% 
    mutate(Master.Protein.Accessions = strsplit(as.character(Master.Protein.Accessions), "; ", fixed = TRUE)) %>% 
    unnest(Master.Protein.Accessions)
  
  if (dpmsr_set$x$peptides_to_use == "Razor") {
    #reduce df to only peptides that have proteins that PD lists as having "razor" peptides
    peptide_shared_expand <- subset(peptide_shared_expand, Master.Protein.Accessions %in% razor_accessions )
    #gather df for razor proteins
    protein_razor_lookup <- protein_razor %>% dplyr::select(Accession, Description, Number.of.Peptides, 
                                                            Coverage.in.Percent, Number.of.Unique.Peptides, Number.of.Razor.Peptides)
    #add columns from protein to df
    peptide_shared_expand <- merge(peptide_shared_expand, protein_razor_lookup, by.x = "Master.Protein.Accessions", by.y = "Accession")
    #create column to check for duplicated peptides
    peptide_shared_expand$duplicated_test <- str_c(peptide_shared_expand$Annotated.Sequence, peptide_shared_expand$Modifications)
    peptide_shared_expand <- peptide_shared_expand[order(peptide_shared_expand$duplicated_test, -peptide_shared_expand$Number.of.Peptides, 
                                                         peptide_shared_expand$Coverage.in.Percent, 
                                                         -peptide_shared_expand$Number.of.Razor.Peptides),]
    #remove duplicated peptides
    peptide_final <- peptide_shared_expand[!duplicated(peptide_shared_expand$duplicated_test),]
    peptide_final$Master.Protein.Descriptions <- peptide_final$Description
    #remove extra columns
    peptide_final <- peptide_final[1:(ncol(peptide_groups))]
    #combine unique and razor/shared peptides
    peptide_final <- rbind(peptide_unique, peptide_final)
  }else if (dpmsr_set$x$peptides_to_use == "Shared") {
    peptide_final <- rbind(peptide_unique, peptide_shared_expand)
  }else{
    peptide_final <- peptide_unique
  }
  
  peptide_final <- peptide_final[order(peptide_final$Master.Protein.Accessions, peptide_final$Sequence),]
  peptide_out <- peptide_final %>% dplyr::select(Confidence, Master.Protein.Accessions, Master.Protein.Descriptions, Proteins, 
                                                 Sequence, Modifications, Unique,
                                                 contains('RT.in.min.by.Search.Engine.'), 
                                                 starts_with('mz.in.Da.by.Search.Engine.'), 
                                                 contains('Charge.by.Search.Engine.'), 
                                                 contains('Percolator.SVM'), 
                                                 contains("Percolator.q.Value"), contains("Abundance.F"))
  
  
  if (ncol(peptide_out) != (12 + dpmsr_set$y$sample_number))
  {
    shinyalert("Oops!", str_c("Number of columns extracted is not as expected ", ncol(peptide_out), "/", (10 + dpmsr_set$y$sample_number)), type = "error")  
  }
  
  colnames(peptide_out)[1:12] <- c("Confidence", "Accession", "Description", "All.Proteins", "Sequence", "Modifications", "Unique", "Retention.Time","Da","mz", "Ion.Score", "q-Value")
  peptide_out <- subset(peptide_out, Accession %in% master_accessions )
  Simple_Excel(peptide_out, "Protein_Peptide_Raw", str_c(dpmsr_set$file$extra_prefix,"_ProteinPeptide_to_Peptide_Raw.xlsx", collapse = " "))
  return(peptide_out)
}

#----------------------------------------------------------------------------------------
prepare_data <- function(session, input, output) {  #function(data_type, data_file_path){
  cat(file = stderr(), "Function prepare_data...", "\n")
  showModal(modalDialog("Preparing Data...", footer = NULL))
  
  if (params$raw_data_format == "protein_peptide") {
    cat(file = stderr(), "prepare data_type 1", "\n")
    protein_to_peptide()
    protein_to_protein()
    params$current_data_format <<- "peptide"
  }else if (params$raw_data_format == "protein") {
    cat(file = stderr(), "prepare data_type 2", "\n")
    protein_to_protein()
    params$current_data_format <<- "protein"
  }else if (params$raw_data_format == "peptide") {
    cat(file = stderr(), "prepare data_type 3", "\n")
    peptide_to_peptide()
    params$current_data_format <<- "peptide"
  }else if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "prepare data_type 4", "\n")
    bg_precursor_to_precursor <- callr::r_bg(func = precursor_to_precursor_bg, args = list(params), stderr = str_c(params$error_path, "//error_preparedata.txt"), supervise = TRUE)
    bg_precursor_to_precursor$wait()
    print_stderr("error_preparedata.txt")
    params$current_data_format <<- "precursor"
  }else if (params$raw_data_format == "fragment") {
    cat(file = stderr(), "prepare data_type 5", "\n")
    peptide_to_peptide()
    params$current_data_format <<- "fragment"
  }else{
    shinyalert("Oops!", "Invalid input output in design file", type = "error") 
    }
  
  if (params$use_isoform) {
    isoform_to_isoform()
  }
  
  cat(file = stderr(), "Function prepare_data...end", "\n\n")
  removeModal()
}


#----------------------------------------------------------------------------------------
precursor_to_precursor_bg <- function(params){
  cat(file = stderr(), "Function precursor_to_precursor_bg", "\n")
  source("Shiny_Rollup.R")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, "precursor_raw")
  
  
  df_colnames <- c("Accession", "Description", "Genes", "Organisms", "Sequence", "PrecursorId", "PeptidePosition")  
  n_col <- length(df_colnames)
  
  df <- df |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('Genes'), contains('Organisms'),
                                                      contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),
                                                      contains("TotalQuantity"))
  
  if (ncol(df) != (n_col + params$sample_number))
  {
    shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error") 
    cat(file = stderr(), "Number of columns extracted is not as expected", "\n")
  }
  
  colnames(df)[1:n_col] <- df_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  df[df ==  "Filtered"] <- NA
  df[(n_col + 1):ncol(df)] <- as.data.frame(lapply(df[(n_col + 1):ncol(df)], as.numeric))
  
  df$Description <- stringr::str_c(df$Description, ", org=", df$Organisms) 
  df$Organisms <- NULL
  
  #save copy of raw peptide (from precursor start)
  raw_peptide <- collapse_precursor_raw(df, info_columns = 0, stats = FALSE, params)
  
  RSQLite::dbWriteTable(conn, "precursor_start", df, overwrite = TRUE)
  RSQLite::dbWriteTable(conn, "raw_peptide", raw_peptide, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "precursor_to_precursor_bg complete", "\n\n")
}



#----------------------------------------------------------------------------------------
protein_to_protein <- function(){
  cat(file = stderr(), "protein_to_protein", "\n")
  protein <- dpmsr_set$data$data_raw_protein
  
  if (dpmsr_set$x$data_source == "PD") {
    cat(file = stderr(), "data type  -> PD", "\n")
    protein <- subset(protein, Master %in% ("IsMasterProtein"))
    protein <- subset(protein, Protein.FDR.Confidence.Combined %in% ("High"))
    protein_out <- protein %>% dplyr::select(Accession, Description, Number.of.Protein.Unique.Peptides, 
                                             contains("Abundance"), -contains("Abundance.Count"))
    colnames(protein_out)[1:3] <- c("Accession", "Description", "Unique.Peptides")
  }
  else if (dpmsr_set$x$data_source == "SP") { 
    cat(file = stderr(), "data type  -> SP", "\n")
    protein_out <- protein %>% dplyr::select(contains("ProteinAccessions"), contains("ProteinDescriptions"), 
                                             contains("ProteinNames"), contains("Genes"), contains("Quantity"))
    precursor_col <- protein %>% dplyr::select(contains("Precursors"))
    precursor_col$average <- round(rowMeans(precursor_col), 1)
    
    protein_out <- protein_out %>% add_column(precursor_col$average, .after = "PG.Genes")
    colnames(protein_out)[1:5] <- c("Accession", "Description", "ProteinName", "Gene", "PrecursorsAvg")
    
    #in case missing values reported as NaN
    protein_out[, 5:ncol(protein_out)] <- sapply(protein_out[, 5:ncol(protein_out)], as.numeric)
    protein_out[5:ncol(protein_out)][protein_out[5:ncol(protein_out)] == "NaN"] <- 0
    
  }else {
    cat(file = stderr(), "protein_to_protein data source not recognized", "\n")
  }
  Simple_Excel(protein_out, "Protein_Protein_Raw", str_c(dpmsr_set$file$extra_prefix, "_Protein_Protein_Raw", "_Protein_to_Protein_Raw.xlsx", collapse = " "))
  return(protein_out)
}

#----------------------------------------------------------------------------------------
peptide_to_peptide <- function(){
  cat(file = stderr(), "peptide_to_peptide", "\n")
  peptide_groups <- dpmsr_set$data$data_raw_peptide
  
  if (dpmsr_set$x$data_source == "PD") {
    cat(file = stderr(), "peptide_to_peptide, PD data", "\n")
    peptide_out <- peptide_groups %>% dplyr::select(Confidence, Master.Protein.Accessions, Master.Protein.Descriptions, 
                                                    Sequence, Modifications, 
                                                    (starts_with("Positions.in.") & ends_with("Proteins")), 
                                                    (starts_with("Modifications.in.") & ends_with("Proteins")), 
                                                    contains('RT.in.min.by.Search.Engine.'), 
                                                    contains('Percolator.SVM'),  
                                                    contains("Percolator.q.Value"), contains("Abundance.F"))
    
    if (ncol(peptide_out) != (10 + dpmsr_set$y$sample_number))
    {
      shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error")  
    }
    
    colnames(peptide_out)[1:10] <- c("Confidence", "Accession", "Description", "Sequence", "Modifications", "PositionMaster", "ModificationMaster",
                                     "Retention.Time", "SVM.Score", "q-Value")
    peptide_out <- subset(peptide_out, Confidence %in% ("High"))
    
  }else if (dpmsr_set$x$data_source == "SP") {
    cat(file = stderr(), "peptide_to_peptide, SP data", "\n")
    
    peptide_out <- peptide_groups %>% dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('Genes'), 
                                                    contains('ModifiedSequence'), contains('PeptidePosition'),
                                                    contains('ProteinPTMLocations'),
                                                    contains("TotalQuantity"))
    
    if (ncol(peptide_out) != (6 + dpmsr_set$y$sample_number))
    {
      shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error")  
    }
    
    colnames(peptide_out)[1:6] <- c("Accession", "Description", "Genes", "Sequence", "PeptidePosition", "PTMLocations")  
  }
  
  # set "Filtered" in TotalQuantity to NA
  peptide_out[peptide_out ==  "Filtered"] <- NA
  peptide_out[8:ncol(peptide_out)] <- as.data.frame(lapply(peptide_out[8:ncol(peptide_out)], as.numeric))
  
  Simple_Excel(peptide_out, "Peptide_Peptide_Raw",  str_c(dpmsr_set$file$extra_prefix, "_Peptide_to_Peptide_Raw.xlsx", collapse = " "))
  cat(file = stderr(), "peptide_to_peptide complete", "\n")
  return(peptide_out)
}


#----------------------------------------------------------------------------------------
precursor_PTM_to_precursor_PTM <- function(){
  cat(file = stderr(), "precursor_PTM_to_precursor_PTM", "\n")
  
  precursor_groups <- dpmsr_set$data$data_raw_precursor
  precursor_colnames <- c("Accession", "Description", "Genes", "Organisms", "Stripped_Seq", "Sequence", "PrecursorId", "PeptidePosition", "PTMLocations") 
  n_col <- length(precursor_colnames)
  
  precursor_out <- precursor_groups %>% dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('Genes'), contains('Organisms'),
                                                      contains('StrippedSequence'), contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),
                                                      contains('ProteinPTMLocations'), contains("TotalQuantity"))
  
  if (ncol(precursor_out) != (n_col + dpmsr_set$y$sample_number))
  {
    shinyalert("Oops!", "Number of columns extracted is not as expected", type = "error")  
  }
  
  colnames(precursor_out)[1:n_col] <- precursor_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  precursor_out[precursor_out ==  "Filtered"] <- NA
  precursor_out[(n_col + 1):ncol(precursor_out)] <- as.data.frame(lapply(precursor_out[(n_col + 1):ncol(precursor_out)], as.numeric))
  
  precursor_out$Description <- str_c(precursor_out$Description, ", org=", precursor_out$Organisms) 
  precursor_out$Organisms <- NULL
  
  Simple_Excel(precursor_out, "precursor_precursor_Raw",  str_c(dpmsr_set$file$extra_prefix, "_Precursor_to_Precursor_Raw.xlsx", collapse = " "))
  cat(file = stderr(), "precursor_to_precursor complete", "\n")
  return(precursor_out)
}


#Top.Apex.RT.in.min,
#----------------------------------------------------------------------------------------
isoform_to_isoform <- function(){
  cat(file = stderr(), "isoform_to_isoform", "\n")
  
  if (is.null(dpmsr_set$data$data_raw_isoform)) {
    cat(file = stderr(), "isoform text file NOT found", "\n")
    shinyalert("Oops!", "Isoform data not imported.  TMT datasets do not automatically export isoform data.", type = "error")
  }
  else {
    cat(file = stderr(), "isoform text file found", "\n")
    peptide_groups <- dpmsr_set$data$data_raw_isoform
    peptide_out <- try(peptide_groups %>% dplyr::select(contains("Confidence.by"), Master.Protein.Accessions, Master.Protein.Descriptions, 
                                                        Sequence, Modifications, 
                                                        (starts_with("Positions.in.") & ends_with("Proteins")), 
                                                        (starts_with("Modifications.in.") & ends_with("Proteins")), 
                                                        Top.Apex.RT.in.min, 
                                                        contains('Percolator.SVM'),  
                                                        contains("Percolator.q.Value"), contains("Abundance.F")))
    if (class(peptide_out) == 'try-error') {
      cat(file = stderr(), "column select error - retry", "\n")
      peptide_out <- peptide_groups %>% dplyr::select(contains("Confidence.by"), Master.Protein.Accessions, Master.Protein.Descriptions,
                                                      Sequence, Modifications, 
                                                      (starts_with("Positions.in.") & ends_with("Proteins")), 
                                                      (starts_with("Modifications.in.") & ends_with("Proteins")), 
                                                      contains("Positions."),
                                                      contains('RT.in.min.by.'), 
                                                      contains('Percolator.SVM'), 
                                                      contains("Percolator.q.Value"), contains("Abundance.F"))
    }
    
    
    cat(file = stderr(), str_c("There are ", ncol(peptide_out) - dpmsr_set$y$sample_number, "/10 info columns"), "\n")
    
    if ((ncol(peptide_out) - dpmsr_set$y$sample_number) < 10) {
      cat(file = stderr(), "If this is TMT phos you will need to manually export the isoform text file, load the correct layout file before export", "\n")
    }
    
    
    if (ncol(peptide_out) != (10 + dpmsr_set$y$sample_number))
    {
      shinyalert("Oops!", "Number of isoform columns extracted is not as expected", type = "error")  
    }
    
    colnames(peptide_out)[1:10] <- c("Confidence", "Accession", "Description", "Sequence", "Modifications", "PositionMaster", "ModificationMaster",
                                     "Retention.Time", "SVM.Score", "q-Value")
    
    peptide_out <- subset(peptide_out, Confidence %in% ("High"))
    Simple_Excel_bg(peptide_out, "Protein_Peptide_Raw", str_c(dpmsr_set$file$extra_prefix, "_Isoform_to_Isoform_Raw.xlsx", collapse = " "))
    cat(file = stderr(), "isoform_to_isoform complete", "\n")
    return(peptide_out)
  }
}


#----------------------------------------------------------------------------------------
order_rename_columns <- function(){
  cat(file = stderr(), "Function order_rename_columns...", "\n")
  showModal(modalDialog("Order and rename Data...", footer = NULL))
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "Function order_rename_columns...precursor", "\n")
    bg_order <- callr::r_bg(func = order_rename_columns_bg, args = list("precursor_start", params), stderr = str_c(params$error_path, "//error_orderrename.txt"), supervise = TRUE)
    bg_order$wait()
    print_stderr("error_orderrename.txt")
    params$info_col_precursor <<- bg_order$get_result()
  }
  
  cat(file = stderr(), "order_rename_columns end", "\n\n")
  removeModal()
  }


# Rearrange columns if raw data is psm, PD does not organize
order_rename_columns_bg <- function(table_name, params) {
  cat(file = stderr(), "Function order_columns_bg...", "\n")
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)
  design <- RSQLite::dbReadTable(conn, "design")
  
  info_columns <- ncol(df) - params$sample_number
  annotate_df <- df[, 1:(ncol(df) - params$sample_number)]
  df <- df[, (ncol(df) - params$sample_number + 1):ncol(df)]
  df <- df[, (design$Raw_Order)]
  
  #make sure data is numeric
  df <- dplyr::mutate_all(df, function(x) as.numeric(as.character(x)))
  
  colnames(df) <- design$Header1
  df <- cbind(annotate_df, df)
  
  RSQLite::dbWriteTable(conn, "precursor_start", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), "order columns end", "\n")
  return(info_columns)
}



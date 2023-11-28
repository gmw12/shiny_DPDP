cat(file = stderr(), "Shiny_Data.R", "\n")

#---------------------------------------------------------------------
load_data_file <- function(session, input, output){
  cat(file = stderr(), "Function load_data_file", "\n")
  
  data_source <- "unkown"
  data_sfb <<- parseFilePaths(volumes, input$sfb_data_file)
  data_path <<- str_extract(data_sfb$datapath, "^/.*/")
  
  cat(file = stderr(), str_c("loading data file(s) from ", data_path[1]), "\n")
  
  #Proteome Discoverer
  if (nrow(data_sfb) > 1) {
    cat(file = stderr(), str_c("data source <---- Proteome Discoverer"), "\n")
    data_source <-  "PD"
    load_PD_data(data_sfb)
  }else {
    cat(file = stderr(), str_c("data source <---- Spectronaut/Fragpipe"), "\n")
    load_data(data_path)
  }

  
  cat(file = stderr(), "Function load_data_file...end", "\n")
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
    testme <- callr::r_bg(func = save_data_bg, args = list(file1 = data_file, dir1 = backup_path), supervise = TRUE)
  }
  
  
  
  RSQLite::dbDisconnect(conn)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), str_c("Data load time ---> ", Sys.time() - start_time), "\n")
  cat(file = stderr(), "function load_PD_data...end", "\n")
}


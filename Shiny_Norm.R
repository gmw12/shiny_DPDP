cat(file = stderr(), "Shiny_Norm.R", "\n")
#--------------------------------------------------------------------

norm_filter <- function() {
  cat(file = stderr(), "Function - norm_filter...", "\n")
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "norm filter precursor...", "\n")
    bg_normfilter <- callr::r_bg(func = norm_filter_bg, args = list("precursor_filter", "precursor_normdata", params), stderr = str_c(params$error_path, "//error_normfilter.txt"), supervise = TRUE)
    bg_normfilter$wait()
    print_stderr("error_normfilter.txt")
    
    bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_normdata", "Precursor_NormData", params$qc_path, params), stderr = str_c(params$error_path, "//error_normdatabarplot.txt"), supervise = TRUE)
    bg_bar$wait()
    print_stderr("error_normdatabarplot.txt")
  } 

  cat(file = stderr(), "Function - norm_filter...end", "\n\n")
}


#--------------------------------------------------------------------------------------
norm_filter_bg <- function(table_name, new_table_name, params) {
  cat(file = stderr(), "Function - norm_filter_bg...", "\n")
  
  start <- Sys.time()
    
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  df <- RSQLite::dbReadTable(conn, table_name)

  #if include checked then only keep selected data
  if (params$norm_include) {
    cat(file = stderr(), stringr::str_c("Norm include filter = ", params$include_norm_grep), "\n")
    df <- df[grepl(params$include_norm_grep, df$Description, ignore.case = TRUE),]
  }
  
  #if exclude checked then remove selection from data
  if (params$norm_exclude) {
    cat(file = stderr(), stringr::str_c("Norm exclude filter = ", params$exclude_norm_grep), "\n")
    df <- df[!grepl(params$exclude_norm_grep, df$Description, ignore.case = TRUE),]
  }

  # if ptm norm checked then only use selected data
  if (params$norm_ptm) {
    df <- df[grep(params$norm_ptm_grep, df$Sequence, ignore.case = TRUE),]
  }
    
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  cat(file = stderr(), stringr::str_c("norm_filter completed in ", Sys.time() - start), "\n")
  
}



#--------------------------------------------------------------------------------------
norm_apply <- function(){
  cat(file = stderr(), "Function - norm_apply...", "\n")
  
  norm_type <- as.list(strsplit(params$norm_type, ",")[[1]])
  
  if (params$raw_data_format == "precursor") {
    cat(file = stderr(), "normalizing precursor data...", "\n")
    table_data <- "precursor_filter"
    table_norm_data <- "precursor_normdata"
    info_columns <- params$info_col_precursor
    new_table_name <- str_c("precursor_norm_", norm_type[1])
  } 
 
  #check if first normalization, if so then add impute only
  if (length(as.list(strsplit(params$norm_type, ",")[[1]])) == 2) {
    impute_table_name <- str_c("precursor_norm_", norm_type[2])
    impute_table_name <- stringr::str_replace_all(impute_table_name, " ", "")
    cat(file = stderr(), str_c("first pass adding impute only table... ", impute_table_name), "\n")

    bg_normapply_impute <- callr::r_bg(func = norm_apply_bg, args = list(table_data, table_norm_data, impute_table_name, info_columns, params, norm_type[2]), stderr = str_c(params$error_path, "//error_normapplyimpute.txt"), supervise = TRUE)
    bg_normapply_impute$wait()
    print_stderr("error_normapplyimpute.txt")
    cat(file = stderr(), "first pass adding impute only table...end", "\n")
  }
  
  
  bg_normapply <- callr::r_bg(func = norm_apply_bg, args = list(table_data, table_norm_data, new_table_name, info_columns, params, norm_type[1]), stderr = str_c(params$error_path, "//error_normapply.txt"), supervise = TRUE)
  bg_normapply$wait()
  print_stderr("error_normapply.txt")

  bg_normbar <- callr::r_bg(func = bar_plot, args = list(new_table_name, str_c("Precursor_",  norm_type[1]), params$qc_path, params), stderr = str_c(params$error_path, "//error_normbarplot.txt"), supervise = TRUE)
  bg_normbar$wait()
  print_stderr("error_normbarplot.txt")
  
  cat(file = stderr(), "Function - norm_apply...end", "\n\n")
}


#--------------------------------------------------------------------------------------
norm_apply_bg <- function(table_data, table_norm_data, new_table_name, info_columns, params, norm_type) {
  cat(file = stderr(), stringr::str_c("Function - norm_apply_bg...", norm_type), "\n")
  
  source('Shiny_Norm_Functions.R')
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  data_to_norm <- RSQLite::dbReadTable(conn, table_data)
  norm_data <- RSQLite::dbReadTable(conn, table_norm_data)
  
  norm_type <- stringr::str_replace_all(norm_type, " ", "")
  
  if (norm_type == "sl") {
    df <- sl_normalize(norm_data, data_to_norm, info_columns)
  }else if (norm_type == "tmm") {
    df <- tmm_normalize(norm_data, data_to_norm, info_columns)
  }else if (norm_type == "sltmm") {
    df <- sl_normalize(norm_data, data_to_norm, info_columns)
  }else if (norm_type == "protein") {
    df <- protein_normalize(data_to_norm, info_columns)
  }else if (norm_type == "ai") {
    df <- ai_normalize(data_to_norm, info_columns)
  }else if (norm_type == "ti") {
    df <- ti_normalize(data_to_norm, info_columns)
  }else if (norm_type == "mi") {
    df <- mi_normalize(data_to_norm, info_columns)
  }else if (norm_type == "mti") {
    df <- mti_normalize(data_to_norm, info_columns)
  }else if (norm_type == "vsn") {
    df <- vsn_normalize(data_to_norm, info_columns)
  }else if (norm_type == "loess") {
    df <- loess_normalize(data_to_norm, info_columns)
  }else if (norm_type == "lr") {
    df <- lr_normalize(data_to_norm, info_columns)
  } else if (norm_type == "quantile") {
    df <- quantile_normalize(data_to_norm, info_columns)
  }else if (norm_type == "directlfq") {
    df <- directlfq_normalize(data_to_norm, info_columns)
  }else if (norm_type == "impute") {
    df <- data_to_norm
  }
  
  cat(file = stderr(), stringr::str_c("write table... ", new_table_name), "\n")
  RSQLite::dbWriteTable(conn, new_table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}



#--------------------------------------------------------------------------------------
# global scaling value, sample loading normalization
sl_normalize <- function(norm_data, data_to_norm, data_title, info_columns){
  cat(file = stderr(), str_c("sl_normalize...", data_title), "\n")
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  excel_name <- "_Peptide_SL_Norm.xlsx"
  target <- mean(colSums(norm_data, na.rm = TRUE))
  norm_facs <- target / colSums(norm_data,na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  Simple_Excel(data_out, "data", str_c(dpmsr_set$file$extra_prefix, "_sl_norm.xlsx", collapse = " "))
  return(data_out)
}


#--------------------------------------------------------------------------------------
# DirectLFQ from Mann ()
directlfq_normalize <- function(data_to_norm, data_title){
  cat(file = stderr(), str_c("directlfq_normalize...", data_title), "\n")
  
  #data_to_norm <- dpmsr_set$data$data_to_norm
  
  info_columns <- ncol(data_to_norm) - dpmsr_set$y$sample_number
  info_column_names <- colnames(data_to_norm)[1:info_columns]
  sample_number <- ncol(data_to_norm) - info_columns
  
  #group and sum data (summed samples will be over written below)
  df <- data_to_norm %>% dplyr::select(Accession, Description, Genes)
  df <- cbind(df, data_to_norm[,(info_columns + 1):ncol(data_to_norm)])
  df$Peptides <- 1
  df <- df %>% group_by(Accession, Description, Genes) %>% summarise_all(list(sum))
  peptide_count <- df$Peptides
  df$Peptides <- NULL
  excel_name <- "_Peptide_DirectLFQ_Norm.xlsx"
  
  require(reticulate)
  use_python(python_path)     
  #"/home/dpmsr/miniconda3/envs/directlfq/bin/python3")
  directlfq <- import("directlfq.lfq_manager")
  
  protein <- data_to_norm$Accession
  df_data <- add_column(data_to_norm, protein, .before = 1)
  
  if (dpmsr_set$x$data_source == "PD") {
    ion <- str_c(df_data$Sequence, "_", df_data$Modifications, "_", 1:nrow(df_data))
  }else if (dpmsr_set$x$data_source == "SP") {
    ion <- df_data$PrecursorId
  }
  
  df_data <- add_column(df_data, ion, .after = 1)
  
  data_in <- df_data[, !(names(df_data) %in% info_column_names)]
  
  directlfq_file <- str_c(dpmsr_set$file$extra_dir, "directlfq.aq_reformat.tsv")
  directlfq_norm_file <- str_c(dpmsr_set$file$extra_dir, "directlfq.aq_reformat.tsv.ion_intensities.tsv")
  directlfq_protein_file <- str_c(dpmsr_set$file$extra_dir, "directlfq.aq_reformat.tsv.protein_intensities.tsv")
  write.table(data_in, file = directlfq_file, sep = "\t", row.names = FALSE)
  
  directlfq$run_lfq(directlfq_file)
  data_out <- Simple_fread((directlfq_norm_file))
  
  data_out <- df_data[1:(info_columns + 2)] %>% left_join(data_out, by = "ion")
  data_out <- data_out[, !(names(data_out) %in% c("ion", "protein.x", "protein.y"))]
  data_out[data_out == 0] <- NA
  
  Simple_Excel(data_out, "data", str_c(dpmsr_set$file$extra_prefix, "_directlfq_norm.xlsx", collapse = " "))
  dpmsr_set$data$directlfq$directlfq_norm <<- data_out
  
  #save protein level data
  protein_out <- Simple_fread((directlfq_protein_file))
  colnames(protein_out)[which(names(protein_out) == "protein")] <- "Accession"
  
  protein_out <- df[1:(ncol(df) - sample_number)] %>% left_join(protein_out, by = "Accession")
  protein_out <- protein_out[, !(names(protein_out) %in% c("V1"))]
  protein_out[protein_out == 0] <- NA
  
  protein_out <- add_column(protein_out, peptide_count, .after = "Genes")
  colnames(protein_out)[which(names(protein_out) == "peptide_count")] <- "Peptides"
  
  protein_out <- add_column(protein_out, dpmsr_set$data$protein_missing, .after = "Peptides")
  colnames(protein_out)[which(names(protein_out) == "dpmsr_set$data$protein_missing")] <- "Detected_Imputed"
  
  dpmsr_set$data$directlfq$directlfq_protein <<- protein_out
  
  #remove files
  file.remove(directlfq_file)
  file.remove(directlfq_norm_file)
  file.remove(directlfq_protein_file)
  
  return(ungroup(data_out))
}


# average global scaling value, sample loading normalization
ai_normalize <- function(norm_data, data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  excel_name <- "_Peptide_AI_Norm.xlsx"
  target <- mean(colMeans(norm_data, na.rm = TRUE))
  norm_facs <- target / colMeans(norm_data, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_ai_norm.xlsx", collapse = " "))
  return(data_out)
}

# intensity / sum of intensities then * median of sum of intensities
ti_normalize <- function(norm_data, data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  excel_name <- "_Peptide_TI_Norm.xlsx"
  target <- median(colSums(norm_data, na.rm = TRUE))
  norm_facs <- target / colSums(norm_data, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_ti_norm.xlsx", collapse = " "))
  return(data_out)
}

# intensity / sum of intensities then * median of sum of intensitites
mi_normalize <- function(norm_data, data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  excel_name <- "_Peptide_MI_Norm.xlsx"
  intensity_medians <- colMedians(data.matrix(norm_data), na.rm = TRUE)
  target <- mean(intensity_medians)
  norm_facs <- target / intensity_medians
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_mi_norm.xlsx", collapse = " "))
  return(data_out)
}


# global scaling value, sample loading normalization
vsn_normalize <- function(data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  excel_name <- "_Peptide_VSN_Norm.xlsx"
  data_to_norm[data_to_norm == 0] <- NA
  data_out <- normalizeVSN(data.matrix(data_to_norm))
  data_out < data.frame(data_out)
  data_out <- data.frame(2^data_out)
  data_out[data_out == -Inf] = 0  # fix log2 of 0  
  #data_out[is.na(data_out)] <- 0.0
  data_out[data_out == 0] <- NA
  data_out <- data_out / 10
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_vsn_norm.xlsx", collapse = " "))
  return(data_out)
}


# global scaling value, sample loading normalization
quantile_normalize <- function(data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  excel_name <- "_Peptide_Quantile_Norm.xlsx"
  data_out <- normalize.quantiles(data.matrix(data_to_norm))
  data_out <- data.frame(data_out)
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_quantile_norm.xlsx", collapse = " "))
  return(data_out)
}


# global scaling value, sample loading normalization
loess_normalize <- function(data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  excel_name <- "_Peptide_LOESS_Norm.xlsx"
  data_to_norm <- log2(data_to_norm)
  data_out <- normalizeCyclicLoess(data_to_norm, weights = NULL, span = 0.7, iterations = 3, method = "fast")
  data_out <- data.frame(data_out)
  data_out <- data.frame(2^data_out)
  data_out[data_out == -Inf] = 0  # fix log2 of 0  
  #data_out[is.na(data_out)] <- 0.0
  data_out[data_out == 0] <- NA
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_loess_norm.xlsx", collapse = " "))
  return(data_out)
}


#linear regression normalization
lr_normalize <- function(data_in, data_title, info_columns) {
  annotation_data <- data_in[1:info_columns]
  data_in <- data_in[(info_columns + 1):ncol(data_in)]
  excel_name <- "_Peptide_LR_Norm.xlsx"
  #normalize lr on data with no missing values, create new data frame
  data_nomissing <- data_in
  data_nomissing$missingvalues <- rowSums(data_nomissing == 0)
  data_nomissing <- subset(data_nomissing, missingvalues == 0)
  data_nomissing <- data_nomissing[1:dpmsr_set$y$sample_number]
  #log2 data
  data_nomissing <- log(data_nomissing,2)
  data_nomissing[data_nomissing == -Inf] = 0  # fix log2 of 0}
  data_in <- log(data_in,2)
  data_in[data_in == -Inf] = 0  # fix log2 of 0}  
  data_in[data_in == 0] <- NA
  data_out <- data_in
  #reorders data by intensity independent of indentification
  for (i in 1:dpmsr_set$y$sample_number) {
    temp <- data.frame(data_nomissing[,i])
    colnames(temp) <- "test"
    temp <- arrange(temp, test)
    data_nomissing[,i] <- temp
  }
  colnames(data_nomissing) <- seq(from = 1, to = dpmsr_set$y$sample_number)
  data_nomissing$avg <- apply(data_nomissing, 1, FUN = function(x) {median(x[x > 0])})
  for (i in 1:dpmsr_set$y$sample_number) {
    data_test <- data.frame(cbind(data_nomissing[,i], data_nomissing$avg))
    colnames(data_test) <- c("x", "y")
    LMfit <- rlm(x~y, data_test, na.action = na.exclude)
    Coeffs <- LMfit$coefficients
    m <- Coeffs[2] # y = mX + b
    b <- Coeffs[1] 
    normdata <- (data_in[,i] - b) / m
    data_out[,i] <- normdata
  }
  data_out <- data.frame(2^data_out)
  data_out[data_out == -Inf] = 0  # fix log2 of 0  
  data_out[data_out == 0] <- NA
  #data_out[is.na(data_out)] <- 0.0
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_lr_norm.xlsx", collapse = " "))
  return(data_out)
}


# global scaling value, sample loading normalization
protein_normalize <- function(data_to_norm, data_title, info_columns){
  protein_norm_raw <- subset(data_to_norm, Accession %in% dpmsr_set$x$protein_norm_list)
  protein_norm_raw <- protein_norm_raw[(info_columns + 1):(info_columns + dpmsr_set$y$sample_number)]
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  #protein_norm_raw$missings <- rowSums(protein_norm_raw == 0.0)
  #protein_norm_raw <- subset(protein_norm_raw, missings==0)
  #protein_norm_raw <- protein_norm_raw[1:sample_number]
  target <- mean(colSums(protein_norm_raw, na.rm = TRUE))
  norm_facs <- target / colSums(protein_norm_raw, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  #data_out <- finish_norm(data_out, excel_name, data_title)
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_protein_norm.xlsx", collapse = " "))
  return(data_out)
}


#TMM Normalized 
tmm_normalize <- function(data_to_norm, data_title, info_columns){
  cat(file = stderr(), str_c("TMM normalize started...", data_title), "\n")
  norm_data <- TMM_norm_data(data_to_norm)
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  norm_data[is.na(norm_data)] <- 0.0
  tmm_factor <- calcNormFactors(norm_data, method = "TMM", sumTrim = 0.1)
  data_out <- sweep(data_to_norm, 2, tmm_factor, FUN = "/") # this is data after SL and TMM on original scale
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_", data_title, ".xlsx", collapse = " "))
  cat(file = stderr(), "TMM normalize complete", "\n")
  return(data_out)
}


#function that recreates norm_data for TMM normalization
TMM_norm_data <- function(data_in){
  cat(file = stderr(), "TMM Filter norm_prep started...", "\n")
  
  if (dpmsr_set$x$raw_data_input == "Protein_Peptide" || dpmsr_set$x$raw_data_input == "Peptide") { 
    data_in <- remove_duplicates(data_in)  
  }
  
  if (as.logical(dpmsr_set$x$peptide_ptm_norm)) {
    if ("Modifications" %in% colnames(data_in)) {
      data_in <- data_in[grep(dpmsr_set$x$peptide_impute_grep, data_in$Modifications),]
    } else {
      data_in <- data_in[grep(dpmsr_set$x$peptide_impute_grep, data_in$Sequence),]
    }
  }
  
  #if include checked then only normalize filter results
  if (as.logical(dpmsr_set$x$norm_include)) {
    data_in <- data_in[grepl(dpmsr_set$x$include_norm_grep, data_in$Description, ignore.case = TRUE),]
  }
  #if exclude checked then only normalize filter results
  if (as.logical(dpmsr_set$x$norm_exclude)) {
    data_in <- data_in[!grepl(dpmsr_set$x$exclude_norm_grep, data_in$Description, ignore.case = TRUE),]
  }
  
  cat(file = stderr(), "TMM Filter norm_prep complete...", "\n")
  return(data_in)
}

#TMM Normalized 
tmm_normalize_old <- function(norm_data, data_to_norm, data_title, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns+1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns+1):ncol(norm_data)]
  #excel_name <- "_Peptide_TMM1_Norm.xlsx"
  #Simple_Excel(cbind(annotate_df, tmm), str_c(file_prefix3, "_Peptide_TMM_Norm_Impute.xlsx", collapse = " ")) 
  tmm <- norm_data
  tmm[is.na(tmm)] <- 0.0
  tmm_factor <- calcNormFactors(tmm, method = "TMM", sumTrim = 0.1)
  data_out <- sweep(tmm, 2, tmm_factor, FUN = "/") # this is data after SL and TMM on original scale
  #Simple_Excel(cbind(annotate_df, data_out), str_c(file_prefix3, "_Peptide_TMM2_Norm.xlsx", collapse = " "))
  #Plot_All_gw(data_out, data_title)
  #data_out <- cbind(annotate_df, data_out)
  #if (peptide_to_protein){data_out <- collapse_peptide(data_out)}
  #colnames(data_out) <- sample_header_final_norm
  #Simple_Excel(data_out, str_c(file_prefix3, "_", data_title, "_final.xlsx", collapse = " "))
  data_out <- cbind(annotation_data, data_out)
  #Simple_Excel(data_out,  str_c(dpmsr_set$file$extra_prefix, "_tmm_norm.xlsx", collapse = " "))
  return(data_out)
}








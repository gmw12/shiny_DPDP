cat(file = stderr(), "Shiny_Norm_Functions.R", "\n")

#--------------------------------------------------------------------------------------
# global scaling value, sample loading normalization
sl_normalize <- function(norm_data, data_to_norm, info_columns){
  cat(file = stderr(), "SL normalize started...", "\n")
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  target <- mean(colSums(norm_data, na.rm = TRUE))
  norm_facs <- target / colSums(norm_data,na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  cat(file = stderr(), "SL normalize started...end", "\n")
  return(data_out)
}

#-----------------------------------------------------------------------------------------------------
#TMM Normalized 
tmm_normalize <- function(norm_data, data_to_norm, info_columns){
  cat(file = stderr(), "TMM normalize started...", "\n")
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  norm_data[is.na(norm_data)] <- 0.0
  tmm_factor <- edgeR::calcNormFactors(norm_data, method = "TMM", sumTrim = 0.1)
  data_out <- sweep(data_to_norm, 2, tmm_factor, FUN = "/") # this is data after SL and TMM on original scale
  data_out <- cbind(annotation_data, data_out)
  cat(file = stderr(), "TMM normalize complete", "\n")
  return(data_out)
}

#---------------------------------------------------------------------------------------
#TMM Normalized 
norm_filter_exclude_include <- function(df, params){
  cat(file = stderr(), "Function norm_filter_include_exclude...", "\n")
  #if exclude checked then remove selection from data
  if (params$norm_exclude != "not_used") {
    cat(file = stderr(), stringr::str_c("Norm exclude filter = ", params$norm_exclude_grep), "\n")
    if (params$norm_exclude == "accession") {
      df <- df[!grepl(params$norm_exclude_grep, df$Accession, ignore.case = TRUE),]
    }
    if (params$norm_exclude == "description") {
      df <- df[!grepl(params$norm_exclude_grep, df$Description, ignore.case = TRUE),]
    }
    if (params$norm_exclude == "name") {
      df <- df[!grepl(params$norm_exclude_grep, df$Name, ignore.case = TRUE),]
    }
    if (params$norm_exclude == "genes") {
      df <- df[!grepl(params$norm_exclude_grep, df$Genes, ignore.case = TRUE),]
    }
    if (params$norm_exclude == "sequence") {
      df <- df[!grepl(params$norm_exclude_grep, df$Sequence, ignore.case = TRUE),]
    }
  }
  
  #if include checked then remove selection from data
  if (params$norm_include != "not_used") {
    cat(file = stderr(), stringr::str_c("Norm include filter = ", params$norm_include_grep), "\n")
    if (params$norm_include == "accession") {
      df <- df[grepl(params$norm_include_grep, df$Accession, ignore.case = TRUE),]
    }
    if (params$norm_include == "description") {
      df <- df[grepl(params$norm_include_grep, df$Description, ignore.case = TRUE),]
    }
    if (params$norm_include == "name") {
      df <- df[grepl(params$norm_include_grep, df$Name, ignore.case = TRUE),]
    }
    if (params$norm_include == "genes") {
      df <- df[grepl(params$norm_include_grep, df$Genes, ignore.case = TRUE),]
    }
    if (params$norm_include == "sequence") {
      df <- df[grepl(params$norm_include_grep, df$Sequence, ignore.case = TRUE),]
    }
  }
  
  # if ptm norm checked then only use selected data
  # if (params$norm_ptm) {
  #   df <- df[grep(params$norm_ptm_grep, df$Sequence, ignore.case = TRUE),]
  # }
  cat(file = stderr(), "Function norm_filter_include_exclude...end", "\n")
  return(df)
}


#------------------------------------------------------------------------------------------------
# global scaling value, sample loading normalization
protein_normalize <- function(data_to_norm, info_columns, params){
  #save(data_to_norm, file="testd2n"); save(info_columns, file="testic"); save(input_protein_norm_grep, file="testing")
  #load(file="testd2n"); load(file="testic"); load(file="testing")
  cat(file = stderr(), "Function - protein_normalize...", "\n")
  
  #protein_norm_raw <- data_to_norm |> dplyr::filter(stringr::str_detect(Name, input_protein_norm_grep) )
  
  if(params$protein_norm_search_field == "accession") {search_col <- "Accession"}
  if(params$protein_norm_search_field == "description") {search_col <- "Description"}
  if(params$protein_norm_search_field == "name") {search_col <- "Name"}
  if(params$protein_norm_search_field == "genes") {search_col <- "Genes"}
  
  protein_norm_raw <- data_to_norm[grepl(params$protein_norm_grep, data_to_norm[[search_col]], ignore.case = TRUE),]
  
  protein_norm_raw <- protein_norm_raw[(info_columns + 1):ncol(protein_norm_raw)]
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  target <- mean(colSums(protein_norm_raw, na.rm = TRUE))
  norm_facs <- target / colSums(protein_norm_raw, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  
  cat(file = stderr(), stringr::str_c("Column --> ", search_col, "      grep ---> ", params$protein_norm_grep), "\n")
  cat(file = stderr(), "Function - protein_normalize...end", "\n")
  return(data_out)
}


#------------------------------------------------------------------------------------------------
# average global scaling value, sample loading normalization
ai_normalize <- function(norm_data, data_to_norm, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  target <- mean(colMeans(norm_data, na.rm = TRUE))
  norm_facs <- target / colMeans(norm_data, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}

# intensity / sum of intensities then * median of sum of intensities
ti_normalize <- function(norm_data, data_to_norm, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  target <- median(colSums(norm_data, na.rm = TRUE))
  norm_facs <- target / colSums(norm_data, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}

# intensity / sum of intensities then * median of sum of intensitites
mi_normalize <- function(norm_data, data_to_norm, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  intensity_medians <- colMedians(data.matrix(norm_data), na.rm = TRUE)
  target <- mean(intensity_medians)
  norm_facs <- target / intensity_medians
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}


# global scaling value, sample loading normalization
vsn_normalize <- function(data_to_norm,  info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  data_to_norm[data_to_norm == 0] <- NA
  data_out <- limma::normalizeVSN(data.matrix(data_to_norm))
  data_out < data.frame(data_out)
  data_out <- data.frame(2^data_out)
  data_out[data_out == -Inf] = 0  # fix log2 of 0  
  data_out[data_out == 0] <- NA
  data_out <- data_out / 10
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}


# global scaling value, sample loading normalization
quantile_normalize <- function(data_to_norm, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  data_out <- preprocessCore::normalize.quantiles(data.matrix(data_to_norm))
  data_out <- data.frame(data_out)
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}


# global scaling value, sample loading normalization
loess_normalize <- function(data_to_norm, info_columns){
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  data_to_norm <- log2(data_to_norm)
  data_out <- limma::normalizeCyclicLoess(data_to_norm, weights = NULL, span = 0.7, iterations = 3, method = "fast")
  data_out <- data.frame(data_out)
  data_out <- data.frame(2^data_out)
  data_out[data_out == -Inf] = 0  # fix log2 of 0  
  data_out[data_out == 0] <- NA
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}


#linear regression normalization
lr_normalize <- function(data_in, info_columns, params) {
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
  for (i in 1:params$sample_number) {
    temp <- data.frame(data_nomissing[,i])
    colnames(temp) <- "test"
    temp <- arrange(temp, test)
    data_nomissing[,i] <- temp
  }
  colnames(data_nomissing) <- seq(from = 1, to = params$sample_number)
  data_nomissing$avg <- apply(data_nomissing, 1, FUN = function(x) {median(x[x > 0])})
  for (i in 1:params$sample_number) {
    data_test <- data.frame(cbind(data_nomissing[,i], data_nomissing$avg))
    colnames(data_test) <- c("x", "y")
    LMfit <- MASS::rlm(x~y, data_test, na.action = na.exclude)
    Coeffs <- LMfit$coefficients
    m <- Coeffs[2] # y = mX + b
    b <- Coeffs[1] 
    normdata <- (data_in[,i] - b) / m
    data_out[,i] <- normdata
  }
  data_out <- data.frame(2^data_out)
  data_out[data_out == -Inf] = 0  # fix log2 of 0  
  data_out[data_out == 0] <- NA
  data_out <- cbind(annotation_data, data_out)
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


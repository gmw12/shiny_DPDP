cat(file = stderr(), "Shiny_Norm_Functions.R", "\n")

#--------------------------------------------------------------------------------------
# global scaling value, sample loading normalization
sl_normalize <- function(norm_data, data_to_norm, info_columns){
  cat(file = stderr(), stringr::str_c("sl_normalize...", "\n"))
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  norm_data <- norm_data[(info_columns + 1):ncol(norm_data)]
  target <- mean(colSums(norm_data, na.rm = TRUE))
  norm_facs <- target / colSums(norm_data,na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}

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

# global scaling value, sample loading normalization
protein_normalize <- function(data_to_norm, info_columns){
  protein_norm_raw <- data_to_norm[grepl(params$protein_norm_grep, data_to_norm$Accession, ignore.case = TRUE),]
  protein_norm_raw <- protein_norm_raw[(info_columns + 1):ncol(protein_norm_raw)]
  annotation_data <- data_to_norm[1:info_columns]
  data_to_norm <- data_to_norm[(info_columns + 1):ncol(data_to_norm)]
  target <- mean(colSums(protein_norm_raw, na.rm = TRUE))
  norm_facs <- target / colSums(protein_norm_raw, na.rm = TRUE)
  data_out <- sweep(data_to_norm, 2, norm_facs, FUN = "*")
  data_out <- cbind(annotation_data, data_out)
  return(data_out)
}

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

cat(file = stderr(), "Shiny_Rollup_Functions.R", "\n")

#--------------------------------------------------------------------------------

rollup_selector <- function(df, df_design, params, sample_count){
  cat(file = stderr(), "function rollup_selector...", "\n")
  
  #save(df, file="test_df")
  #. load(file="test_df")
  
  df_info <- df |> dplyr::select(contains(c("Accession", "Description", "Name", "Genes"))) |> 
    dplyr::mutate(Precursors = 1, .after = Genes)
  
  #hard coded from above selection
  info_columns <- 5 # add 1 for Precursors
  
  df_sample <- df[,(ncol(df)-sample_count+1):ncol(df)]
  df <- cbind(df_info, df_sample)

  if (params$rollup_method == "sum") {df <-  rollup_sum(df)}
    else if (params$rollup_method == "median") {df <-  rollup_median(df)}
    else if (params$rollup_method == "median_polish") {df <-  rollup_median_polish(df, info_columns)}
    else if (params$rollup_method == "mean") {df <-  rollup_mean(df)}
    else if (params$rollup_method == "topn") {df <-  rollup_topn(df, params)}
    else if (params$rollup_method == "iq_maxlfq") {df <-  rollup_maxlfq(df, info_columns, params)}
  
  cat(file = stderr(), "function rollup_selector...end", "\n\n")
  return(df)
}

#--------------------------------------------------------------------------------
rollup_maxlfq <- function(peptide_data, info_columns, params){
  cat(file = stderr(), "rollup_maxlfq triggered...", "\n")
  require(iq)
  
  #save(peptide_data, file="maxlfqpeptidedata"); save(info_columns, file="maxlfqinfocolumns")
  #  load(file="maxlfqpeptidedata"); load(file="maxlfqinfocolumns"); 
  
  samples_columns <- ncol(peptide_data) - info_columns
  
  #group and sum data (summed samples will be over written below)
  df <- peptide_data |> dplyr::group_by(Accession, Description, Name, Genes) |> dplyr::summarise_all(list(sum))
  
  #select data and log
  df_data <- peptide_data[,(info_columns + 1):ncol(peptide_data)]
  df_data[df_data == 0] <- NA
  df_data <- log2(df_data)
  
  #build iq:maxlfq data input, build first sample manually then loop
  peptide_data$id <- 1:nrow(peptide_data)
  df_maxlfq <- data.frame(peptide_data$Accession)
  sample_list <- colnames(peptide_data[info_columns + 1])
  df_maxlfq$sample_list <- sample_list
  quant <- df_data[,1]
  df_maxlfq$quant <- quant
  df_maxlfq$id <- peptide_data$id
  colnames(df_maxlfq) <- c("protein_list", "sample_list", "quant", "id")
  
  for (i in 2:ncol(df_data)) {
    df_temp <- data.frame(peptide_data$Accession)
    sample_list <- colnames(peptide_data[info_columns + i])
    df_temp$sample_list <- sample_list
    quant <- df_data[,i]
    df_temp$quant <- quant
    df_temp$id <- peptide_data$id
    colnames(df_temp) <- c("protein_list", "sample_list", "quant", "id") 
    df_maxlfq <- rbind(df_maxlfq, df_temp)
  }
  
  #remove missing values, iq functions below do not like them!
  df_maxlfq <- df_maxlfq[!is.na(df_maxlfq$quant),]
  
  protein_table <- iq::fast_MaxLFQ(df_maxlfq)
  result <- data.frame(2^protein_table$estimate)
  
  #save(result, file = "lfq3"); save(df, file = "lfq4")
  #load(file="lfq3"); load(file="lfq4")
  
  #combine annotation data with protein rollup, (replacing summed data)
  result <- result[order(row.names(result)),]
  df <- df[order(df$Accession),]
  rownames(df) <- NULL
  
  if(params$maxlfq_scale) {
    cat(file = stderr(), "rollup_maxlfq results scaled...", "\n")
    df_average <- rowMeans(df[,(info_columns + 1):ncol(df)], na.rm = TRUE)
    result_average <- rowMeans(result, na.rm = TRUE)
    scale_factor <- df_average/result_average
    result <- result * scale_factor
  }
  
  df[(info_columns + 1):ncol(df)] <- result
  
  cat(file = stderr(), "rollup_maxlfq triggered...end", "\n\n")
  return(df)  
}
#--------------------------------------------------------------------------------
rollup_sum <- function(df){
  cat(file = stderr(), "function rollup_sum...", "\n")
  
  protein_df <- df |> dplyr::group_by(Accession, Description, Name, Genes) |> dplyr::summarise_all(list(sum))
  protein_df <- data.frame(dplyr::ungroup(protein_df))

  #save(df, file="test_df"); save(protein_df, file="test_protein_df")
  #. load(file="test_df"); load(file="test_protein_df")
  
  cat(file = stderr(), "function rollup_sum...end", "\n\n")
  return(protein_df)
}

#--------------------------------------------------------------------------------
rollup_sum_peptide <- function(df, df_design, comp_number, stats_comp, params){
  cat(file = stderr(), "function rollup_sum_peptide...", "\n")
  source('Shiny_Misc_Functions.R')
  
  #save(list = c("df", "df_design", "comp_number", "stats_comp"), file = "zrollupsumpeptide")   #  load(file = "zrollupsumpeptide")
  
  if (comp_number > 0) {
    design_order <- str_to_numlist(c(stats_comp$N_loc[comp_number], stats_comp$D_loc[comp_number], stats_comp$SPQC_loc[comp_number]))
    sample_ID <- df_design$ID[design_order]
  }else{
    sample_ID <- df_design$ID
  }
  
  if (params$data_source == "PD" &  params$raw_data_format == "precursor") {
    df <- df |> dplyr::select(contains(c("Accession", "Description", "Name", "Genes", "Sequence", "Modifications", sample_ID))) |> 
      dplyr::mutate(PSMs = 1, .after = Modifications)
    peptide_df <- df |> dplyr::group_by(Accession, Description, Name, Genes, Sequence, Modifications) |> dplyr::summarise_all(list(sum))
    
  }else {
    df <- df |> dplyr::select(contains(c("Accession", "Description", "Name", "Genes", "Sequence", "PeptidePosition", sample_ID))) |> 
      dplyr::mutate(Precursors = 1, .after = PeptidePosition)
    peptide_df <- df |> dplyr::group_by(Accession, Description, Name, Genes, Sequence, PeptidePosition) |> dplyr::summarise_all(list(sum))
  }
  
  
  
  peptide_df <- data.frame(dplyr::ungroup(peptide_df))
  
  cat(file = stderr(), "function rollup_sum_peptide...end", "\n\n")
  return(peptide_df)
}

#--------------------------------------------------------------------------------
rollup_median <- function(df){
  cat(file = stderr(), "function rollup_median...", "\n")
  
  #group and sum for precursors column
  df_info <- df |> dplyr::select(contains(c("Accession", "Description", "Name", "Gene", "Precursors"))) |> 
    dplyr::group_by(Accession, Description, Genes) |> dplyr::summarise_all(list(sum))
  df_info <- data.frame(dplyr::ungroup(df_info))
  
  #select data and log
  df[,(ncol(df_info) + 1):ncol(df)] <- log2(df[,(ncol(df_info) + 1):ncol(df)])
  
  df <- df |> dplyr::group_by(Accession, Description, Genes) |> dplyr::summarise_all(list(median))
  df <- data.frame(dplyr::ungroup(df))
  
  df[,(ncol(df_info) + 1):ncol(df)] <- 2^(df[,(ncol(df_info) + 1):ncol(df)])
  df$Precursors <- df_info$Precursors
  
  cat(file = stderr(), "function rollup_median...", "\n\n")
  return(df)
}

#--------------------------------------------------------------------------------
rollup_mean <- function(peptide_data, info_columns){
  cat(file = stderr(), "rollup_mean triggered...", "\n")
  
  #group and sum for precursors column
  df_info <- df |> dplyr::select(contains(c("Accession", "Description", "Name", "Gene", "Precursors"))) |> 
    dplyr::group_by(Accession, Description, Genes) |> dplyr::summarise_all(list(sum))
  df_info <- data.frame(dplyr::ungroup(df_info))
  
  #select data and log
  df[,(ncol(df_info) + 1):ncol(df)] <- log2(df[,(ncol(df_info) + 1):ncol(df)])
  
  df <- df |> dplyr::group_by(Accession, Description, Genes) |> dplyr::summarise_all(list(mean))
  df <- data.frame(dplyr::ungroup(df))
  
  df[,(ncol(df_info) + 1):ncol(df)] <- 2^(df[,(ncol(df_info) + 1):ncol(df)])
  df$Precursors <- df_info$Precursors
  
  cat(file = stderr(), "rollup_mean triggered...", "\n\n")
  return(df)
}

#--------------------------------------------------------------------------------
rollup_median_polish <- function(peptide_data, info_columns){
  cat(file = stderr(), "rollup_median_polish triggered...", "\n")
  
  #group and sum data (summed samples will be over written below)
  df <- peptide_data |> group_by(Accession, Description, Name, Genes) |> summarise_all(list(sum))
  df <- data.frame(ungroup(df))
  
  #select data and log
  df_data <- peptide_data[,(info_columns + 1):ncol(peptide_data)]
  df_data[df_data == 0] <- NA
  df_data <- log2(df_data)
  
  #loop through each grouped protein and rollup
  for (i in 1:nrow(df)) {
    df_temp <- df_data[which(peptide_data$Accession == df$Accession[i] & peptide_data$Description == df$Description[i] & peptide_data$Genes == df$Genes[i]),] 
    df_temp <- medpolish(df_temp, trace.iter = FALSE, na.rm = TRUE)
    data_out <- df_temp$overall + df_temp$col
    data_out <- replace_na(data_out, 0)
    df[i,(info_columns + 1):ncol(df)] <- 2^data_out
  }
  
  cat(file = stderr(), "rollup_median_polish triggered...end", "\n\n")
  return(df)
}


#--------------------------------------------------------------------------------
rollup_topn <- function(df, topn_count){
  cat(file = stderr(), "rollup_topn triggered...", "\n")
  
  #group and sum data (summed samples will be over written below)
  df_info <- df |> dplyr::group_by(Accession, Description, Name, Genes) |> dplyr::summarise_all(list(sum))
  df_info <- data.frame(dplyr::ungroup(df_info))
  
  #select data and log, hardcoded start of data for now...
  df_data <- log2(df[,5:ncol(df)])
  
  #loop through each grouped protein and rollup
  for (i in 1:nrow(df_info)) {
    df_temp <- df_data[which(df$Accession == df_info$Accession[i] & df$Description == df_info$Description[i] & df$Genes == df_info$Genes[i]),] 
    data_mean_row <- rowMeans(df_temp, na.rm = TRUE)
    data_mean_row_sorted <- sort(data_mean_row, decreasing = TRUE, na.last = TRUE, index.return = TRUE)
    data_out <- colMeans(df_temp[data_mean_row_sorted$ix[1:min(topn_count, length(data_mean_row))],], na.rm = TRUE)
    data_out <- tidyr::replace_na(data_out, 0)
    df_info[i,5:ncol(df_info)] <- 2^data_out
  }
  
  cat(file = stderr(), "rollup_topn triggered...end", "\n\n")
  return(df_info)
}





#--------------------------------------------------------------------------------------
# DirectLFQ from Mann ()
rollup_directlfq <- function(peptide_data, info_columns){
  cat(file = stderr(), str_c("directlfq_rollup..."), "\n")
  
  peptide_data <<- peptide_data
  info_columns <<- info_columns
  
  #group and sum data (summed samples will be over written below)
  df <- peptide_data |> group_by(Accession, Description, Genes) |> summarise_all(list(sum))
  sample_number <- ncol(peptide_data) - info_columns
  info_column_names <- colnames(peptide_data)[1:info_columns]
  
  require(reticulate)
  use_python("/home/dpmsr/miniconda3/envs/directlfq/bin/python3")
  directlfq <- import("directlfq.lfq_manager")
  
  protein <- peptide_data$Accession
  df_data <- add_column(peptide_data, protein, .before = 1)
  
  # if (dpmsr_set$x$data_source == "PD"){
  #   ion <- str_c(df_data$Sequence, "_", df_data$Modifications, "_", 1:nrow(df_data))
  # }else if (dpmsr_set$x$data_source == "SP"){
  #   ion <- df_data$PrecursorId
  # }
  # 
  
  ion <- 1:nrow(peptide_data)
  
  df_data <- add_column(df_data, ion, .after = 1)
  
  data_in <- df_data[, !(names(df_data) %in% info_column_names)]
  data_in[data_in == 0] <- NA
  
  directlfq_file <- str_c(dpmsr_set$file$extra_dir, "directlfq.aq_reformat.tsv")
  directlfq_protein_file <- str_c(dpmsr_set$file$extra_dir, "directlfq.aq_reformat.tsv.protein_intensities.tsv")
  directlfq_ion_file <- str_c(dpmsr_set$file$extra_dir, "directlfq.aq_reformat.tsv.ion_intensities.tsv")
  write.table(data_in, file = directlfq_file, sep = "\t", row.names = FALSE)
  
  directlfq$run_lfq(directlfq_file)
  data_out <- Simple_fread((directlfq_protein_file))
  colnames(data_out)[which(names(data_out) == "protein")] <- "Accession"
  
  data_out <- df[1:(ncol(df) - sample_number)] |> left_join(data_out, by = "Accession")
  data_out <- data_out[, !(names(data_out) %in% c("V1"))]
  data_out[data_out == 0] <- NA
  
  file.remove(directlfq_file)
  file.remove(directlfq_ion_file)
  file.remove(directlfq_protein_file)
  
  cat(file = stderr(), str_c("directlfq_rollup...end"), "\n\n")
  return(ungroup(data_out))
}





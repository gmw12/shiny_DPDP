file1 = "/mnt/h_black2/10651/20241015_104450_10651_GluC_ZrIMAC_100924_Report_GW.tsv"
file2 = "/mnt/h_black2/10651/20241015_104458_10651_GluC_TiO2_100924_Report_GW.tsv"
file3 = "/mnt/h_black2/10651/20241015_104503_10651_Trypsin_ZrIMAC_100924_Report_GW.tsv"
file4 = "/mnt/h_black2/10651/20241015_104508_10651_Trypsin_TiO2_10092_Report_GW.tsv"

desc1 = "GluC_ZrIMAC"
desc2 = "GluC_TiO2"
desc3 = "Trypsin_ZrIMAC"
desc4 = "Trypsin_TiO2"

#------------------------------

file = file4
desc = desc4
sample_columns =4

file_name <- stringr::str_c("/mnt/h_black2/10651/", desc, "_102324.xlsx")

data_list <- qual_phos(file, desc, sample_columns)

excel_list_name <- c("Raw", "Refined", "Phos_Precursor", "Phos_Peptide")

nextsheet <- 1
wb <- openxlsx::createWorkbook()
for (i in 1:length(excel_list_name)) {
  openxlsx::addWorksheet(wb, excel_list_name[i])
  openxlsx::writeData(wb, sheet = nextsheet, data_list[[i]])
  nextsheet <- nextsheet + 1 
}  
openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)



#--------------------------------------------------------------------------------------------------

qual_phos <- function(file, desc, sample_columns) {

  df_raw <- data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE)
  
  df_phos_prob <- df_raw |> dplyr::select(contains('PTMProbabilities [Phospho')) 
  
  df_colnames <- c("Accession", "Description", "Name", "Genes", "Organisms", "Sequence", "PrecursorId", "PeptidePosition", "ProteinPTMLocations")  
  n_col <- length(df_colnames)
  
  df_info <- df_raw |> dplyr::select(contains('ProteinAccessions'), contains('ProteinDescriptions'), contains('ProteinNames'), contains('Genes'), contains('Organisms'),
                            contains('ModifiedSequence'), contains('PrecursorId'), contains('PeptidePosition'),contains('ProteinPTMLocations'))
  
  df_data <- df_raw |> dplyr::select(contains("TotalQuantity"))
  df_data <- df_data[,2:sample_columns]
  
  
  colnames(df_info) <- df_colnames  
  
  # set "Filtered" in TotalQuantity to NA
  df_data[df_data ==  "Filtered"] <- NA
  df_data <- data.frame(lapply(df_data, as.numeric))
  
  df <- cbind(df_info, df_data)
  
  df$Description <- stringr::str_c(df$Description, ", org=", df$Organisms) 
  df$Organisms <- NULL
  
  phos_which <- which(grepl("Phospho", df$Sequence))
  df_phos <- df[phos_which,]
  df_phos_prob <- df_phos_prob[phos_which,]
  
  local_df <- data.frame(localize_summary(df_phos, df_phos_prob))
  df_phos <- tibble::add_column(df_phos, "Protein_PTM_Loc" = local_df$Protein_PTM_Loc, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "PTM_Loc" = local_df$PTM_Loc, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "Local2" = local_df$Local2, .after="PrecursorId")
  df_phos <- tibble::add_column(df_phos, "Local" = local_df$Local, .after="PrecursorId")
  
  #save(df_phos, file="z1")
  
  #save copy of raw peptide (from precursor start)
  raw_peptide <- collapse_precursor_ptm_raw(df_phos, sample_columns, info_columns = (ncol(df_phos)-sample_columns+1))
 
  return(list(df_raw, df, df_phos, raw_peptide)) 
}


#-----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
localize_summary <- function(df_phos, df_phos_prob){
  cat(file = stderr(), "Function localize_summary...", "\n")
  
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  #Step 1 consolicate localization into one list of max local for each position
  #create df of just probabilities
  df_phos_prob[df_phos_prob=="Filtered"] <- ""
  
  df_local <- data.frame(cbind(df_phos$Sequence, df_phos$PeptidePosition, df_phos$ProteinPTMLocations))
  colnames(df_local) <- c("ModSequence", "PeptidePosition", "ProteinPTMLocations")
  
  df_local$Stripped <- gsub("\\[.*?\\]", "", df_local$ModSequence)
  df_local$Stripped <- gsub("_", "", df_local$Stripped)
  
  #Step 2 reduce modified sequence to STY with phos residue marked with *
  df_local$phos_seq <- gsub("\\[Phospho \\(STY\\)\\]", "*", df_local$ModSequence)
  df_local$phos_seq <- gsub("_", "", df_local$phos_seq)
  df_local$phos_seq <- gsub("\\[.*?\\]", "", df_local$phos_seq)
  df_local$phos_seq <- gsub("[^STY*]", "", df_local$phos_seq)
  
  
  #new step
  df_local$ModSequence2 <- df_local$ModSequence
  df_local$ModSequence2 <- gsub("S\\[Phospho \\(STY\\)\\]", "s", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("T\\[Phospho \\(STY\\)\\]", "t", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("Y\\[Phospho \\(STY\\)\\]", "y", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("\\[.*?\\]", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("_", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("\\[.*?\\]", "", df_local$ModSequence2)
  df_local$ModSequence2 <- gsub("_", "", df_local$ModSequence2)
  
  
  #new step
 df_local$PTM_Loc <- ""
  
  for (r in (1:nrow(df_local))) {
    find_s <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "s"))
    find_t <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "t"))
    find_y <- unlist(stringr::str_locate_all(df_local$ModSequence2[r], "y"))
    
    
    if (length(find_s) > 0) {
      find_s <- unlist(stringr::str_split(paste("S", find_s, collapse = " ", sep = ""), pattern=" "))
      find_s <- find_s[1:(length(find_s)/2)]
    }else{
      find_s <- ""
    }
    
    
    if (length(find_t) > 0) {
      find_t <- unlist(stringr::str_split(paste("T", find_t, collapse = " ", sep = ""), pattern=" "))
      find_t <- find_t[1:(length(find_t)/2)]
    }else{
      find_t <- ""
    }
    
    if (length(find_y) > 0) {
      find_y <- unlist(stringr::str_split(paste("Y", find_y, collapse = " ", sep = ""), pattern=" "))
      find_y <- find_y[1:(length(find_y)/2)]
    }else{
      find_y <- ""
    }
    
    final_all <- c(find_s, find_t, find_y)
    final_all <- final_all[final_all != ""]
    final_all <- paste(final_all, collapse = ",", sep = ",")
    df_local$PTM_Loc[r] <- final_all  
  }
  
  
 #new step
 df_local$Protein_PTM_Loc <- gsub("([CM][0-9]+)", "", df_local$ProteinPTMLocations) 
 df_local$Protein_PTM_Loc <- gsub("\\(,", "\\(",  df_local$Protein_PTM_Loc) 
 df_local$Protein_PTM_Loc <- gsub("\\),", "\\)",  df_local$Protein_PTM_Loc) 
 df_local$Protein_PTM_Loc <- gsub(",\\(", "\\(",  df_local$Protein_PTM_Loc) 
 df_local$Protein_PTM_Loc <- gsub(",\\)", "\\)",  df_local$Protein_PTM_Loc) 
 df_local$Protein_PTM_Loc <- gsub("\\(", "",  df_local$Protein_PTM_Loc)  
 df_local$Protein_PTM_Loc <- gsub("\\)", "",  df_local$Protein_PTM_Loc)  
 
  # determines residue location for phos on sequence reduced to STY
  parallel_result1 <- foreach(r = 1:nrow(df_local), .combine = c) %dopar% {
    phos_count <- stringr::str_count(df_local$phos_seq[r], "\\*")
    temp_list <- c()
    if (phos_count >= 1) {
      phos_loc <- stringr::str_locate_all(df_local$phos_seq[r], "\\*")
      for(c in (1:phos_count)){
        temp_list <- c(temp_list, (phos_loc[[1]][[c]] - c))
      }
    }
    list(temp_list)
  }
  
  df_local$phos_res <- parallel_result1
  
  
  #consolidates probabilities for each sample and takes the highest prob for each residue
  parallel_result2 <- foreach(r = 1:nrow(df_phos_prob), .combine = c) %dopar% {
    first_value <- FALSE
    for (c in (1:ncol(df_phos_prob))) {
      if (!first_value) { 
        temp1 <- unlist(stringr::str_split(df_phos_prob[[r,c]], ";")) |> as.numeric() 
        if (!is.na(temp1[[1]])) {
          first_value <- TRUE
        }
      }else {
        temp2 <- unlist(stringr::str_split(df_phos_prob[[r,c]], ";")) |> as.numeric()
        if (!is.na(temp2[[1]])) {
          temp1 <- pmax(temp1, temp2)
        }
      }
    }
    list(temp1)
  }
  
  df_local$pr <- parallel_result2
  
  #mark as localized or not
  parallel_result3 <- foreach(r = 1:nrow(df_local), .combine = rbind) %dopar% {
    prob <- unlist(df_local$pr[r])
    residue <- unlist(df_local$phos_res[r])
    local <- c()
    for (c in length(residue)) {
      local <- c(local, prob[residue]) 
    }
    if (max(local) >= 0.75) {
      if (min(local) >= 0.75) {
        local2 <- "Y"
      } else {
        local2 <- "P"
      }
    }else {
      local2 <- "N"
    }
    list(local, local2)
  }
  
  parallel_result3 <- data.frame(parallel_result3)
  colnames(parallel_result3) <- c("Local", "Local2")
  row.names(parallel_result3) <- NULL
  
  numlist_to_string <- function(x) {
    return(toString(paste(unlist(x$Local) |> as.character() |> paste(collapse = ","))))
  }
  
  numlist_to_string2 <- function(x) {
    return(toString(paste(unlist(x$Local2) |> as.character() |> paste(collapse = ","))))
  }
  
  parallel_result3$Local <- apply(parallel_result3, 1, numlist_to_string)
  parallel_result3$Local2 <- apply(parallel_result3, 1, numlist_to_string2)
  
  parallel_result3$Protein_PTM_Loc <- df_local$Protein_PTM_Loc
  parallel_result3$PTM_Loc <-  df_local$PTM_Loc
  
  stopCluster(cl) 
  cat(file = stderr(), "Function localize_summary...end", "\n")
  return(parallel_result3) 
}

#--- collapse precursor to peptide-------------------------------------------------------------
collapse_precursor_ptm_raw <- function(precursor_data, sample_columns, info_columns) {
  cat(file = stderr(), "function collapse_precursor_ptm_raw...", "\n")

  # precursor_data <- df_phos
  
  localized_data <- precursor_data |> dplyr::select("Local", "Local2")
  
  #drop columns 
  col_count <- ncol(precursor_data)
  precursor_data$PrecursorId <- NULL
  precursor_data$ProteinPTMLocations <- NULL
  precursor_data$Local <- NULL
  precursor_data$Local2 <- NULL
  col_count <- col_count - ncol(precursor_data)

  info_columns = ncol(precursor_data) - sample_columns +1
  columns = names(precursor_data)[1:info_columns]
  
  #setting info columns the same so that all rollups group the same way
  localized_data <- cbind(precursor_data$Sequence, localized_data)
  colnames(localized_data) <- c("Sequence", "Local", "Local2")
  precursor_count_df <- precursor_data[,1:info_columns]
  
  #precursor_data[is.na(precursor_data)] <- 0
  #precursor_data[precursor_data==""] <- "na"
  
  #rollup data and ungroup
  peptide_data <- precursor_data |>
    dplyr::group_by_at(dplyr::vars(all_of(columns))) |> dplyr::summarise_all(list(sum))
  
  peptide_data <- data.frame(dplyr::ungroup(peptide_data))
  
  local_rollup_df <- rollup_local(localized_data)
  
  local_rollup_df <- local_rollup_df[match(peptide_data$Sequence, local_rollup_df$Sequence),]
  
  peptide_data <- tibble::add_column(peptide_data, "Local2"=local_rollup_df$Local2, .after="Sequence")
  peptide_data <- tibble::add_column(peptide_data, "Local"=local_rollup_df$Local, .after="Sequence")
  
  precursor_count_df$Precusors <- 1
  precursor_count_df <- precursor_count_df |>
    dplyr::group_by_at(dplyr::vars(all_of(columns))) |> dplyr::summarise_all(list(sum))
  Precursors <- precursor_count_df$Precusors
  
  peptide_data <- tibble::add_column(peptide_data, Precursors, .after="Sequence")
  
  cat(file = stderr(), "function collapse_precursor_ptm_raw...end", "\n")
  return(peptide_data)
}

#------------------------------------------------------------
rollup_local <- function(localized_data) {
  cat(file = stderr(), "Function rollup_local...", "\n")
  
  str_to_numlist <- function(str_in) {
    num_out <- strsplit(str_in, ",") |> unlist() |> as.numeric()
    return(num_out)
  }
  
  require(foreach)
  require(doParallel)
  cores <- detectCores()
  cl <- makeCluster(cores - 2)
  registerDoParallel(cl)
  
  local_unique <- data.frame(unique(localized_data$Sequence))
  local_unique$Local <- ""
  local_unique$Local2 <- ""
  colnames(local_unique) <- c("Sequence", "Local", "Local2")
  
  #for (i in (1:nrow(local_unique))) {
  parallel_result <- foreach(i = 1:nrow(local_unique), .combine = rbind) %dopar% { 
    if(grepl("Phospho", local_unique$Sequence[i])) {
      
      test_df <- localized_data[localized_data$Sequence == local_unique$Sequence[i],]
      
      if(nrow(test_df) > 1) {
        first_value <- TRUE
        for (r in (1:nrow(test_df))) {
          if (first_value) { 
            temp1 <- str_to_numlist(test_df$Local[r])
            if (!is.na(temp1[[1]])) {
              first_value <- FALSE
            }
          }else {
            temp2 <- str_to_numlist(test_df$Local[r])
            if (!is.na(temp2[[1]])) {
              temp1 <- pmax(temp1, temp2)
            }
          }
        }
      }else {
        temp1 <- str_to_numlist(test_df$Local[1])
      }
      
      if (max(temp1) >= 0.75) {
        if (min(temp1) >= 0.75) {
          local2 <- "Y"
        } else {
          local2 <- "P"
        }
      }else {
        local2 <- "N"
      }
      
      #local_unique$Local[i] <- list(temp1) 
      #local_unique$Local2[i] <- local2
      
    }else {
      temp1 <- ""
      local2 <- ""
    }
    
    #local_unique$Local[i] <- list(temp1) 
    #local_unique$Local2[i] <- local2
    list(temp1, local2)
  }
  
  stopCluster(cl) 
  
  parallel_result <- data.frame(parallel_result)
  row.names(parallel_result) <- NULL
  parallel_result <- cbind(local_unique$Sequence, parallel_result)
  colnames(parallel_result) <- c("Sequence", "Local", "Local2")
  
  numlist_to_string <- function(x) {
    return(toString(paste(unlist(x$Local) |> as.character() |> paste(collapse = ","))))
  }
  
  numlist_to_string2 <- function(x) {
    return(toString(paste(unlist(x$Local2) |> as.character() |> paste(collapse = ","))))
  }
  
  parallel_result$Local <- apply(parallel_result, 1, numlist_to_string)
  parallel_result$Local2 <- apply(parallel_result, 1, numlist_to_string2)
  
  cat(file = stderr(), "Function rollup_local...end", "\n")
  return(parallel_result)
}




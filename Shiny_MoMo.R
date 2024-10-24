cat(file = stderr(), "Shiny_MoMo.R", "\n")


#----------------------------------------------------------------------------------------- 
run_momo <- function(session, input, output, params){
  cat(file=stderr(), stringr::str_c("Function run_momo..." ), "\n")
  showModal(modalDialog("Creating files for MoMo...", footer = NULL))  
  
  arg_list <- list(input$select_data_comp_momo, input$momo_direction, params)
  bg_run_momo <- callr::r_bg(func = run_momo_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_run_momo.txt"), supervise = TRUE)
  bg_run_momo$wait()
  print_stderr("error_run_momo.txt")
  momo_data_list <- bg_run_momo$get_result()
  momo_file1 <- momo_data_list[[1]]
  momo_path1 <- momo_data_list[[2]]
  momo_file2 <- momo_data_list[[3]]
  momo_path2 <- momo_data_list[[4]]
  
  output$download_momo_file1 <- downloadHandler(
    file = function(){
      momo_file1
    },
    content = function(file){
      cat(file = stderr(), stringr::str_c("download_momo_file = ", momo_path1), "\n")
      file.copy(momo_path1, file)
    }
  )
  
  output$download_momo_file2 <- downloadHandler(
    file = function(){
      momo_file2
    },
    content = function(file){
      cat(file = stderr(), stringr::str_c("download_momo_file = ", momo_path2), "\n")
      file.copy(momo_path2, file)
    }
  )
  
  removeModal()
  cat(file=stderr(), stringr::str_c("Function run_momo...end" ), "\n")
}

#----------------------------------------------------------------------------------------- 
run_momo_bg <- function(input_select_data_comp_momo, input_momo_direction, params){
  cat(file=stderr(), stringr::str_c("Function run_momo_bg..." ), "\n")

  source('Shiny_File.R')
  source('Shiny_MotifX.R')
  require(stringr)
  
  stats_comp <- read_table_try("stats_comp", params)
  #  comp_string <- stats_comp$Name[1]
  comp_string <- input_select_data_comp_momo
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  cat(file=stderr(), stringr::str_c("run_momo_bg...1" ), "\n")
  
  table_name <- stats_comp$Final_Table_Name_Peptide[comp_number]
  data_in <- read_table_try(table_name, params)
  
  if(input_momo_direction == 'Up') {
    momo_df <- subset(data_in, data_in$Stats == "Up" ) 
  }else if (input_momo_direction == 'Down') {
    momo_df <- subset(data_in, data_in$Stats == "Down" ) 
  }else{
    momo_df <- subset(data_in, data_in$Stats == "Up" | data_in$Stats == "Down") 
  }

  cat(file=stderr(), stringr::str_c("run_momo_bg...2" ), "\n")
  #keep only localized
  momo_df <- momo_df[momo_df$Local2 == "Y",]
  
  parsed_ref <- read_table_try("phos_fasta", params)
  ptm_data <- create_motifx_input(momo_df, parsed_ref, input_select_data_comp_momo, input_momo_direction)
  
  # Locate PTMs within full-length proteins and extract neighboring motifs
  phindPTMs <- PTMphinder::phindPTMs(ptm_data, parsed_ref)
  
  cat(file=stderr(), stringr::str_c("run_momo_bg...3" ), "\n")
  # Reformat foreground sequences for motif-x
  foreground_Seqs <- unlist(strsplit(phindPTMs[,"Flank_Seq"], split = "[.]"))
  foreground_Seqs_Filtered <- foreground_Seqs[which(lapply(foreground_Seqs, nchar)==15)]
  
  filename1 <- stringr::str_c("ptm_file_", comp_string, "_", input_momo_direction,  ".tsv")
  filepath1 <- stringr::str_c(params$phos_path, filename1)
  write.table(foreground_Seqs_Filtered , file=filepath1, quote=FALSE, sep='\t', col.names = FALSE, row.names = FALSE)
  
  cat(file=stderr(), stringr::str_c("run_momo_bg...4" ), "\n")
  parsed_ref$Accession <- sub("^",">",parsed_ref$Accession)
  parsed_ref$Sequence <- gsub("U", "X", parsed_ref$Sequence)
  
  fasta_df <- data.frame(matrix(nrow=nrow(parsed_ref)*2, ncol = 1))
  colnames(fasta_df) <- "fasta"
  odd_lines <- seq(from = 1, to=nrow(fasta_df), by=2)
  even_lines <- seq(from = 2, to=nrow(fasta_df), by=2)
  fasta_df$fasta[odd_lines] <- parsed_ref$Accession
  fasta_df$fasta[even_lines] <- parsed_ref$Sequence
  
  filename2 <- stringr::str_c("context_sequences_", comp_string, "_", input_momo_direction,  ".tsv")
  filepath2 <- stringr::str_c(params$phos_path, filename2)
  write.table(fasta_df, file=filepath2, quote=FALSE, sep='\t', col.names = FALSE, row.names = FALSE)
  
  cat(file=stderr(), stringr::str_c("Function run_momo_bg..." ), "\n")
  return(list(filename1, filepath1, filename2, filepath2))
}
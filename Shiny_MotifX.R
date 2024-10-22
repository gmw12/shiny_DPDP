cat(file = stderr(), "Shiny_MotifX.R", "\n")

#----------------------------------------------------------------------------------------- 
create_phos_database <- function(session, input, output, params){
  cat(file=stderr(), "Function create_phos_database...", "\n")
  
  fasta_path <- parseFilePaths(volumes, input$motif_fasta_file)
  
  args_list <- list(input$fasta_grep1, input$fasta_grep2, fasta_path, params)
  bg_phos_db <- callr::r_bg(func = create_phos_database_bg, args = args_list, stderr = stringr::str_c(params$error_path, "//error_phos_db.txt"), supervise = TRUE)
  bg_phos_db$wait()
  print_stderr("error_phos_db.txt")
  
  bg_fasta_list <- bg_phos_db$get_result()
  start_fasta_sample <- bg_fasta_list[[1]]
  colnames(start_fasta_sample) <- c("Fasta")
  end_fasta_sample <- bg_fasta_list[[2]]
  
  
  output$start_fasta_example<- renderRHandsontable({
    rhandsontable(start_fasta_sample, rowHeaders = NULL) 
  })
  
  output$end_fasta_example<- renderRHandsontable({
    rhandsontable(end_fasta_sample, rowHeaders = NULL) %>%
      hot_cols(colWidths = 80, halign = "htCenter" ) %>%
      hot_col(col = "Accession", halign = "htCenter", colWidths = 200) %>%
      hot_col(col = "Sequence", halign = "htLeft", colWidths = 800)
  })
  
  cat(file=stderr(), "Function create_phos_database...end", "\n") 
}

#----------------------------------------------------------------------------------------- 

create_phos_database_bg <- function(input_fasta_grep1, input_fasta_grep2, fasta_path, params){
  cat(file=stderr(), "Function create_phos_database_bg...", "\n")
  source('Shiny_File.R')
  
  save(list = c("input_fasta_grep1", "input_fasta_grep2", "fasta_path"), file="zz2343")
  #   load(file="zz2343")
  
  cat(file=stderr(), stringr::str_c("fasta_path... ",fasta_path), "\n")
  
  fasta_txt_file <- fasta_path$datapath
  raw_fasta <- data.frame(readr::read_lines(fasta_txt_file))
  raw_fasta_sample <- data.frame(raw_fasta[1:20,])
  
  raw_fasta <- data.frame(lapply(raw_fasta, function(x) gsub(input_fasta_grep1, ">", x)), stringsAsFactors=F)
  raw_fasta <- data.frame(lapply(raw_fasta, function(x) gsub(input_fasta_grep2, "", x)), stringsAsFactors=F)
  colnames(raw_fasta) <- c("fasta")
  
  a_rows <- which(grepl(">", raw_fasta$fasta))
  new_fasta <- data.frame(matrix(nrow=length(a_rows), ncol=2))
  colnames(new_fasta) <- c("Accession", "Sequence")
  
  for(r in (1:length(a_rows))) {
    new_fasta$Accession[r] <- raw_fasta$fasta[a_rows[r]]
    if (r < length(a_rows)) {
      new_fasta$Sequence[r] <- paste(raw_fasta$fasta[(a_rows[r]+1):(a_rows[r+1]-1)], collapse = "")
    }else {
      new_fasta$Sequence[r] <- paste(raw_fasta$fasta[(a_rows[r]+1):nrow(raw_fasta)], collapse = "")
    }
  }
  
  new_fasta$Accession <- gsub(">", "", new_fasta$Accession)
  
  write_table_try("phos_fasta", new_fasta, params)

  cat(file=stderr(), "Function create_phos_database_bg...end", "\n")
  return(list(raw_fasta_sample, new_fasta[1:15,]))
}

#----------------------------------------------------------------------------------------- 
run_motifx <- function(session, input, output, params){
  cat(file=stderr(), "Function run_motifx..." , "\n") 
  showModal(modalDialog("Motifx running...", footer = NULL))
  source('Shiny_MotifX.R')
  
  args_list <- list(input$pval_motif, input$motif_min_seq, input$pvalue_cutoff, input$foldchange_cutoff, 
                    input$select_data_comp_motif, params)
  bg_motifx <- callr::r_bg(func = run_motifx_bg, args = args_list, stderr = stringr::str_c(params$error_path, "//error_motifx.txt"), supervise = TRUE)
  bg_motifx$wait()
  print_stderr("error_motifx.txt")
  
  cat(file=stderr(), "Function run_motifx...1" , "\n") 
  motif_data <- bg_motifx$get_result() 
  
  cat(file=stderr(), "Function run_motifx...2" , "\n") 
  if ( class(motif_data) != "try-error"){
    options_DT <- list(
      selection = 'single',
      #dom = 'Bfrtipl',
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = 500,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0),
          visibile = TRUE,
          "width" = '6',
          className = 'dt-center'
        ),
        list(
          targets = c(1),
          width = '15',
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 35 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
            "}"
          )
        ),
        list(
          targets = c(2),
          width = '20',
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 20 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
            "}"
          )
        )
      ),
      ordering = TRUE,
      orderClasses = TRUE,
      fixedColumns = list(leftColumns = 1),
      pageLength = 10,
      lengthMenu = c(10, 50, 100, 200)
      #formatRound(columns = c(sample_col_numbers + 1), digits = 0)
    )
  
  
    cat(file=stderr(), "Function run_motifx...3" , "\n") 
    output$motif_table <-  DT::renderDataTable(motif_data, rownames = FALSE, extensions = c("FixedColumns"), 
                                              selection = 'single', options=options_DT,
                                              callback = DT::JS('table.page(3).draw(false);'))
    
    cat(file=stderr(), "Function run_motifx...4" , "\n") 
    

    output$download_motif_table <- downloadHandler(
      file = function(){
        input$motif_data_filename
      },
      content = function(file){
        fullName <- stringr::str_c(params$phos_path, input$motif_data_filename)
        cat(file = stderr(), stringr::str_c("download_motif_data fullname = ", fullname), "\n")
        file.copy(fullName, file)
      }
    )
  
  }

  removeModal()
  cat(file=stderr(), "Function run_motifx...end" , "\n") 
}
#----------------------------------------------------------------------------------------- 


run_motifx_bg <- function(input_pval_motif, input_motif_min_seq, input_pvalue_cutoff, input_foldchange_cutoff, 
                          input_select_data_comp_motif, params){
  cat(file=stderr(), "Function run_motifx_bg..." , "\n") 
  source('Shiny_File.R')
  source('Shiny_MotifX.R')
  
  stats_comp <- read_table_try("stats_comp", params)
  comp_string <- input_select_data_comp_motif
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  table_name <- stringr::str_c(stats_comp$Final_Table_Name_Peptide[comp_number])
  data_in <- read_table_try(table_name, params)
  
  #---------Create background data ----------------------------------------
  # Set parameters for extractBackground (s - sequence list; c - central character; w - width of motifs
  parsed_ref <- read_table_try("phos_fasta", params)
  s <- parsed_ref$Sequence
  w <- 15
  pval_motif <- input_pval_motif * 1e-5
  min_seq <- as.integer(input_motif_min_seq)
  

  cat(file=stderr(), "motifx up..." , "\n") 
  filter_df <- subset(data_in, data_in$Stats == "Up" ) 
  if (nrow(filter_df > 0)){
    FC <- "Up"
    ptm_data_up <- create_motifx_input(filter_df, parsed_ref, input_select_data_comp_motif, "Up")
    cat(file=stderr(), stringr::str_c("ptm_data has ", nrow(ptm_data_up), " entries") , "\n") 
    motifx_S <- motifx_calc(s, "S", w, "Up", ptm_data_up, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    cat(file=stderr(), stringr::str_c("motifx_S has ", nrow(motifx_S), " entries") , "\n") 
    motifx_T <- motifx_calc(s, "T", w, "Up", ptm_data_up, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    cat(file=stderr(), stringr::str_c("motifx_T has ", nrow(motifx_T), " entries") , "\n") 
    motifx_Y <- motifx_calc(s, "Y", w, "Up", ptm_data_up, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    cat(file=stderr(), stringr::str_c("motifx_Y has ", nrow(motifx_Y), " entries") , "\n") 
    motifx_up <- rbind(motifx_S, motifx_T, motifx_Y)
    cat(file=stderr(), stringr::str_c("motifx_up has ", nrow(motifx_up), " entries") , "\n") 
  }else{
    motifx_up <- NULL
  }
  
  
  cat(file=stderr(), "motifx down..." , "\n")     
  filter_df <- subset(data_in, data_in$Stats == "Down" )
  if (nrow(filter_df > 0)){
    FC <- "Down"
    ptm_data_down <- create_motifx_input(filter_df, parsed_ref, input$select_data_comp_motif, "Down")
    motifx_S <- motifx_calc(s, "S", w, "Down", ptm_data_down, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    motifx_T <- motifx_calc(s, "T", w, "Down", ptm_data_down, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    motifx_Y <- motifx_calc(s, "Y", w, "Down", ptm_data_down, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    motifx_down <- rbind(motifx_S, motifx_T, motifx_Y)
  }else{
    motifx_down <- NULL
  }
  
  cat(file=stderr(), "motifx updown..." , "\n")  
  filter_df<- subset(data_in, data_in$Stats == "Up" | data_in$Stats == "Down") 
  if (nrow(filter_df > 0)){
    FC <- "UpDown"
    ptm_data_updown <- create_motifx_input(filter_df, parsed_ref, input$select_data_comp_motif, "UpDown")
    motifx_S <- motifx_calc(s, "S", w, "UpDown", ptm_data_updown, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    motifx_T <- motifx_calc(s, "T", w, "UpDown", ptm_data_updown, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    motifx_Y <- motifx_calc(s, "Y", w, "UpDown", ptm_data_updown, parsed_ref, pval_motif, min_seq, input_pvalue_cutoff, input_foldchange_cutoff, input_select_data_comp_motif)
    motifx_updown <- rbind(motifx_S, motifx_T, motifx_Y)
  }else{
    motifx_updown <- NULL
  }
  
  cat(file=stderr(), stringr::str_c("compiling motifs... ", nrow(motifx_up), "... ", nrow(motifx_down), "... ", nrow(motifx_updown)) , "\n")   
  
  motifx_all <- rbind(motifx_up, motifx_down, motifx_updown)
  motifx_all$score <- round(motifx_all$score, digits = 2)
  motifx_all$fold.increase <- round(motifx_all$fold.increase, digits = 2)
  
  #save(motifx_all, file="zz09") # load(file="zz09")
  
  ptm_data_all <- rbind(ptm_data_up, ptm_data_down, ptm_data_updown)
  
  write_table_try("MotifX_table", motifx_all, params)
  write_table_try("MotifX_ptm_data", ptm_data_all, params)
  
  cat(file=stderr(), "Function run_motifx_bg...end" , "\n") 
  #save(list=c("motifx_all", "motif_table_name"), file="z093")  #  load(file="z093")
  return(motifx_all)
}


#--------------------------------------------------------------------------------------------

create_motifx_input <- function(filter_df, parsed_ref, comparison, direction_filter){
  cat(file=stderr(), "Function run_motifx_input..." , "\n") 
  #---------Create input file for PTM data ----------------------------------------
  source('Shiny_File.R')
  
  MotifPhos <- filter_df |> dplyr::select(contains(c("Accession", "Sequence", "Local"))) 
  MotifPhos <- MotifPhos[MotifPhos$Local2=="Y",]
  
  MotifPhos$Accession <- gsub(";.*", "", MotifPhos$Accession)
  MotifPhos$temp1 <- MotifPhos$Sequence
  MotifPhos$temp1 <- gsub("S\\[Phospho \\(STY\\)\\]", "s", MotifPhos$temp1)
  MotifPhos$temp1 <- gsub("T\\[Phospho \\(STY\\)\\]", "t", MotifPhos$temp1)
  MotifPhos$temp1 <- gsub("Y\\[Phospho \\(STY\\)\\]", "y", MotifPhos$temp1)
  MotifPhos$temp1 <- gsub("\\[.*?\\]", "", MotifPhos$temp1)
  MotifPhos$temp1 <- gsub("_", "", MotifPhos$temp1)
  MotifPhos$Sequence <- gsub("\\[.*?\\]", "", MotifPhos$Sequence)
  MotifPhos$Sequence <- gsub("_", "", MotifPhos$Sequence)
  
  MotifPhos$PTM_Loc <- ""
  
  for (r in (1:nrow(MotifPhos))) {
    find_s <- unlist(stringr::str_locate_all(MotifPhos$temp1[r], "s"))
    find_t <- unlist(stringr::str_locate_all(MotifPhos$temp1[r], "t"))
    find_y <- unlist(stringr::str_locate_all(MotifPhos$temp1[r], "y"))

    
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
    
    final_all <- paste(final_all, collapse = ";", sep = ";")
    
    MotifPhos$PTM_Loc[r] <- final_all  
    
  }
  
  MotifPhos$Total_Sites <- stringr::str_count(MotifPhos$PTM_Loc, "S")+stringr::str_count(MotifPhos$PTM_Loc, "T")+stringr::str_count(MotifPhos$PTM_Loc, "Y")
  MotifPhos$Identifier <- stringr::str_c("PhosPeptide", seq.int(nrow(MotifPhos)))
  
  ptm_data <- MotifPhos |> dplyr::select(contains(c("Identifier", "Accession", "Sequence", "Total_Sites", "PTM_Loc", "Local"))) 
  ptm_data$Local2 <- NULL
  colnames(ptm_data) <- c("Identifier", "Protein_ID", "Peptide_Seq", "Total_Sites", "PTM_Loc", "PTM_Score")
  
  #--------- Subset to insure proteins are in database ----------------------------------------
  ptm_data$Protein_ID <- gsub( " .*$", "", ptm_data$Protein_ID)
  ptm_data <-subset(ptm_data, Protein_ID %in% parsed_ref$Accession)
  
  cat(file=stderr(), "Function run_motifx_input...end" , "\n") 
  return(ptm_data)
}



#--------------------------------------------------------------------------------------------

motifx_calc <- function(s, c, w, FC, ptm_data, parsed_ref, pval_motif, min_seq, pval_filter, fc_filter, comparison){
  cat(file=stderr(), "Function motif_calc..." , "\n") 
  
  extractBack <- PTMphinder::extractBackground(s, c, w)
  
  # Locate PTMs within full-length proteins and extract neighboring motifs
  phindPTMs <- PTMphinder::phindPTMs(ptm_data, parsed_ref)
  
  
  # Reformat foreground sequences for motif-x
  foreground_Seqs <- unlist(strsplit(phindPTMs[,"Flank_Seq"], split = "[.]"))
  
  foreground_Seqs_Filtered <- foreground_Seqs[which(lapply(foreground_Seqs, nchar)==15)]
  
  cat(file=stderr(), stringr::str_c("Number of foreground sequences for = ", length(foreground_Seqs_Filtered)), "\n")  
  
  # Run motif-x using foreground and background sequences from PTMphinder functions above
  motifx_data <- try(rmotifx::motifx(foreground_Seqs_Filtered, extractBack, central.res = c, min.seqs = min_seq, pval.cutoff = pval_motif))
  
  if ((!is.null(motifx_data)) & (class(motifx_data) != "try-error")){
    motifx_data <- tibble::add_column(motifx_data, FC, .before = 1)
    motifx_data <- tibble::add_column(motifx_data, pval_filter, .before = 1)
    motifx_data <- tibble::add_column(motifx_data, fc_filter, .before = 1)
    motifx_data <- tibble::add_column(motifx_data, c, .before = 1)
    motifx_data <- tibble::add_column(motifx_data, comparison, .before = 1)
    names(motifx_data)[1:5] <- c("comparison", "central.res", "foldchange", "pval", "direction")
  } else {
    motifx_data <- NULL
  }
  
  cat(file=stderr(), "Function motif_calc...end" , "\n") 
  return(motifx_data)
} 


#-------------------------------------------------------------------------------------------------------------  
motif_data_save_excel <- function(session, input, output, params) {
  cat(file = stderr(), "Function motif_data_save_excel...", "\n")
  showModal(modalDialog("Saving motif table to excel...", footer = NULL))  
  
  arg_list <- list(input$motif_data_filename, params)
  
  bg_motif_data_save_excel <- callr::r_bg(func = motif_data_save_excel_bg , args = arg_list, stderr = str_c(params$error_path, "//error_motif_data_save_excel.txt"), supervise = TRUE)
  bg_motif_data_save_excel$wait()
  print_stderr("error_motif_data_save_excel.txt")
  
  cat(file = stderr(), "Function motif_data_save_excel...end", "\n")
  removeModal()
  
}
#-------------------------------------------------------------------------------------------------------------  
motif_data_save_excel_bg <- function(input_motif_data_filename, params) {
  cat(file = stderr(), "Function motif_data_save_excel_bg...", "\n")
  source('Shiny_File.R')
  
  filename <- stringr::str_c(params$phos_path, input_motif_data_filename)
  file_dir <- stringr::str_c(params$phos_path) 
  
  if(!fs::is_dir(file_dir)) {
    cat(file = stderr(), stringr::str_c("create_dir...", file_dir), "\n")
    dir_create(file_dir)
  }
  
  cat(file = stderr(), stringr::str_c("filename = ", filename) , "\n")
  
  motif_data <- read_table("MotifX_table", params)
  Simple_Excel(motif_data, "data", filename)
  
  cat(file = stderr(), "Function motif_data_save_excel_bg...end", "\n")
  
}


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

create_phos_database_custom <- function(){
  require(stringr)
  raw_fasta <- read.csv2("/Users/gregwaitt/Documents/Data/5560_crov2_Sep2020.fasta", header=FALSE)
  colnames(raw_fasta) <- "fasta"
  raw_fasta$fasta <- as.character(raw_fasta$fasta)
  test_s <- ""
  test_a <- ""
  test_d <- ""
  find_split <- str_locate(raw_fasta[1,], " ")
  test_a <- c(test_a, substring(raw_fasta[1,], 2, find_split[1]-1))
  test_d <- c(test_d, substring(raw_fasta[1,], find_split[1]+1))
  temp_seq <-""
  for (i in 2:nrow(raw_fasta)) {
    testa <- substring(raw_fasta[i,],1,1)
    if (identical(testa, ">" )) {
      find_split <- str_locate(raw_fasta[i,], " ")
      test_a <- c(test_a, substring(raw_fasta[i,], 2, find_split[1]-1) )
      test_d <- c(test_d, substring(raw_fasta[i,], find_split[1]+1))
      test_s <- c(test_s, temp_seq)
      temp_seq <- ""
    }else{
      temp_seq <- stringr::str_c(temp_seq, raw_fasta[i,])
    }
  }
  test_s <- c(test_s, temp_seq)
  
  test_a <- as.data.frame(test_a)
  test_d <- as.data.frame(test_d)
  test_s <- as.data.frame(test_s)
  new_fasta<-(cbind(test_a, test_d, test_s))
  new_fasta <- as.data.frame(new_fasta[-1,])
  colnames(new_fasta) <- c("Accession", "Description", "Sequence")
  new_fasta$Accession <- as.character(new_fasta$Accession)
  new_fasta$Description <- as.character(new_fasta$Description)
  new_fasta$Sequence <- as.character(new_fasta$Sequence)
  if (site_user=="dpmsr"){
    Simple_Excel_bg(new_fasta, "MotifX", "5560_crov2_Sep2020.xlsx")
  }
}

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
testonly <- function(){
  parsed_ref <- parsed_refx
  s <- sx
  w <- wx
  pval_motif <- pval_motifx
  data_in <- data_inx
  
  parsed_ref <- dpmsr_set$data$phos$background
  names(parsed_ref) <- c("V1", "V2")
  s <- list(parsed_ref$Sequence)
  s <- unlist(s)
  
  s <- unlist(parsed_ref[,2])
  extractBack <- extractBackground(s, "S", 15)
  
  filter_df <- subset(data_in, data_in$Stats == "Up" ) 
  FC <- "Up"
  ptm_data <- create_motifx_input(filter_df, parsed_ref)
  motifx_S <- motifx_calc(s, "S", w, "Up", ptm_data, parsed_ref, pval_motif, input$pvalue_cutoff, input$foldchange_cutoff, input$select_data_comp_motif)
  
  test_parsed_ref <- read.table("Mmusculus_110419.txt", header=FALSE, row.names=NULL, sep="\t")                     
  test_s <- unlist(test_parsed_ref[,2])
  extractBack <- extractBackground(test_s, "S", 15)
  
  parsed_ref <- dpmsr_set$data$phos$background
  names(parsed_ref) <- c("V1", "V2")
  parsed_ref <- rbind(c("Accession", "Sequence"), parsed_ref)
  s <- unlist(parsed_ref[,2])
  extractBack <- extractBackground(s, "S", 15)
  
  s<-parsed_ref$Sequence
  
  
  
}
#--------------------------------------------------------------------------------------------

create_padded_seq <- function(){
  
  filter_df <- dpmsr_set$data$final$sltmm
  
  parsed_ref <- dpmsr_set$data$phos$background
  
  #---------Create input file for PTM data ----------------------------------------
  
  MotifPhos <- data.frame(cbind(filter_df$Accession, filter_df$Sequence, filter_df$Modifications))
  
  colnames(MotifPhos) <- c("Accession", "Sequence", "Modifications")
  MotifPhos$Accession <- as.character(MotifPhos$Accession)
  MotifPhos$Sequence <- as.character(MotifPhos$Sequence)
  MotifPhos$Modifications <- as.character(MotifPhos$Modifications)
  
  MotifPhos$Accession <- gsub(";.*", "", MotifPhos$Accession)
  MotifPhos <- subset(MotifPhos, Accession != 1000 )
  
  MotifPhos$PhosOnly <- str_extract(MotifPhos$Modifications, "\\dxP.+\\]")
  MotifPhos$Total_Sites <- substr(MotifPhos$PhosOnly,0,1)
  MotifPhos$PTM_Loc <- str_extract_all(MotifPhos$PhosOnly, "[STY]\\d+")
  MotifPhos$PTM_Loc <- gsub("[(]", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub("[)]", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub("[\"]", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub(",", ";", MotifPhos$PTM_Loc)
  MotifPhos <- subset(MotifPhos, PTM_Loc != 'character0')
  MotifPhos$PTM_Loc <- gsub("c", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub(" ", "", MotifPhos$PTM_Loc)
  
  MotifPhos$PTM_Score <- str_extract_all(MotifPhos$PhosOnly, "[(]\\d+\\.*\\d*")
  MotifPhos$PTM_Score <- gsub("[(]", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub("[)]", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub("[\"]", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub(",", ";", MotifPhos$PTM_Score)
  MotifPhos <- subset(MotifPhos, PTM_Score != 'character0')
  MotifPhos$PTM_Score <- gsub("c", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub(" ", "", MotifPhos$PTM_Score)
  
  MotifPhos$Identifier <- stringr::str_c("PhosPeptide", seq.int(nrow(MotifPhos)))
  
  df <- data.frame(cbind(MotifPhos$Identifier, MotifPhos$Accession, MotifPhos$Sequence, 
                         MotifPhos$Total_Sites, MotifPhos$PTM_Loc, MotifPhos$PTM_Score))
  colnames(df) <- c("Identifier", "Protein_ID", "Peptide_Seq", "Total_Sites", "PTM_Loc", "PTM_Score")
  
  #--------- Subset to insure proteins are in database ----------------------------------------
  ptm_data <- df
  ptm_data$Protein_ID <- gsub( " .*$", "", ptm_data$Protein_ID)
  ptm_data <-subset(ptm_data, Protein_ID %in% parsed_ref$Accession)
  
  if (site_user=="dpmsr"){
    cat(file=stderr(), "write ptm_data to excel..." , "\n") 
    Simple_Excel_bg(ptm_data, "MotifX", stringr::str_c(dpmsr_set$file$phos, "Padded_Seq.xlsx"))
  }
  
  phindPTMs_Example <- phindPTMs(ptm_data, parsed_ref)
  df_PTMs <- data.frame(phindPTMs_Example)
  
  ptm_data$Prot_Loc <- df_PTMs$Prot_Loc
  ptm_data$Flank_Seq <- df_PTMs$Flank_Seq
  ptm_data$Ambiguity <- df_PTMs$Ambiguity
  ptm_data$Prot_Seq <- df_PTMs$Prot_Seq
  
  if (site_user=="dpmsr"){
    Simple_Excel_bg(ptm_data, "MotifX", stringr::str_c(dpmsr_set$file$phos, "Padded_Seq.xlsx"))
  }
  
  return(ptm_data)
}





#--------------------------------------------------------------------------------------------

create_motifx_input_PD <- function(filter_df, parsed_ref, comparison, direction_filter){
  #---------Create input file for PTM data ----------------------------------------
  
  MotifPhos <- data.frame(cbind(filter_df$Accession, filter_df$Sequence, filter_df$Modifications))
  colnames(MotifPhos) <- c("Accession", "Sequence", "Modifications")
  MotifPhos$Accession <- as.character(MotifPhos$Accession)
  MotifPhos$Sequence <- as.character(MotifPhos$Sequence)
  MotifPhos$Modifications <- as.character(MotifPhos$Modifications)
  
  MotifPhos$Accession <- gsub(";.*", "", MotifPhos$Accession)
  MotifPhos <- subset(MotifPhos, Accession != 1000 )
  
  MotifPhos$PhosOnly <- str_extract(MotifPhos$Modifications, "\\dxP.+\\]")
  MotifPhos$Total_Sites <- substr(MotifPhos$PhosOnly,0,1)
  MotifPhos$PTM_Loc <- str_extract_all(MotifPhos$PhosOnly, "[STY]\\d+")
  MotifPhos$PTM_Loc <- gsub("[(]", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub("[)]", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub("[\"]", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub(",", ";", MotifPhos$PTM_Loc)
  MotifPhos <- subset(MotifPhos, PTM_Loc != 'character0')
  MotifPhos$PTM_Loc <- gsub("c", "", MotifPhos$PTM_Loc)
  MotifPhos$PTM_Loc <- gsub(" ", "", MotifPhos$PTM_Loc)
  
  MotifPhos$PTM_Score <- str_extract_all(MotifPhos$PhosOnly, "[(]\\d+\\.*\\d*")
  MotifPhos$PTM_Score <- gsub("[(]", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub("[)]", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub("[\"]", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub(",", ";", MotifPhos$PTM_Score)
  MotifPhos <- subset(MotifPhos, PTM_Score != 'character0')
  MotifPhos$PTM_Score <- gsub("c", "", MotifPhos$PTM_Score)
  MotifPhos$PTM_Score <- gsub(" ", "", MotifPhos$PTM_Score)
  
  MotifPhos$Identifier <- stringr::str_c("PhosPeptide", seq.int(nrow(MotifPhos)))
  
  df <- data.frame(cbind(MotifPhos$Identifier, MotifPhos$Accession, MotifPhos$Sequence, 
                         MotifPhos$Total_Sites, MotifPhos$PTM_Loc, MotifPhos$PTM_Score))
  colnames(df) <- c("Identifier", "Protein_ID", "Peptide_Seq", "Total_Sites", "PTM_Loc", "PTM_Score")
  
  #--------- Subset to insure proteins are in database ----------------------------------------
  ptm_data <- df
  ptm_data$Protein_ID <- gsub( " .*$", "", ptm_data$Protein_ID)
  ptm_data <-subset(ptm_data, Protein_ID %in% parsed_ref$Accession)
  
  if (site_user=="dpmsr"){
    cat(file=stderr(), "write ptm_data to excel..." , "\n")
    Simple_Excel_bg(ptm_data, "MotifX", stringr::str_c(dpmsr_set$file$phos, "MotifX_ptmdata_", comparison, "_", direction_filter, ".xlsx"))
  }
  
  return(ptm_data)
}



#--------------------------------------------------------------------------------------------
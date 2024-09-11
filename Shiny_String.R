cat(file = stderr(), "Shiny_String.R", "\n")

#----------------------------------------------------------------------------------------- 

setup_string_bg <- function(input_checkbox_filter_adjpval, params){
  cat(file=stderr(), stringr::str_c("function setup_string_bg..."), "\n")
  
  require(STRINGdb)
  require(httr)
  source('Shiny_File.R')
  
  string_version <- "11.5"
  
  string_db <- STRINGdb$new(version=string_version, species=params$string_species, 
                            score_threshold=0, input_directory=params$string_path)
  
  #creates protein list and gets stringId's
  string_id_call <- function(content_type, df, row_start, row_stop, species)
  {
    cat(file=stderr(), "function string_id_call", "\n")
    proteins <- df$Accession[row_start]
    for(i in (row_start+1):row_stop)
    {
      proteins <- stringr::str_c(proteins,"%0d", df$Accession[i])
    } 
    string_id_api <- stringr::str_c("https://string-db.org/api/", content_type, "/get_string_ids?identifiers=", 
                                    proteins,
                                    "&species=", params$string_species, 
                                    "&limit=1", "&echo_query=1",
                                    "&caller_identity=DukeProteomics" )
    res_id <- GET(string_id_api)
    test_id <- rawToChar(res_id$content)
    cat(file=stderr(), "function string_id_call read_delim", "\n")
    df_temp <- readr::read_delim(test_id, delim="\t", col_names = TRUE, show_col_types = FALSE)
    gc()
    return(df_temp)
  }
  
  
  #get stringIDs for all proteins in set
  df <- read_table_try("protein_impute_final", params)
  
  #get first 500 or all if less than 500
  if (nrow(df) < 501) {
    cat(file=stderr(), "get string id's, less than 500 in set", "\n")
    string_IDs <- string_id_call("tsv", df, 1, nrow(df), params$string_species)
  }else{
    string_IDs <- string_id_call("tsv", df, 1, 500, params$string_species)
  }
  
  cat(file=stderr(), stringr::str_c("string setup...2"), "\n")
  row_start = 501
  row_stop = min(row_start + 499, nrow(df))
  
  while (row_start < row_stop)
  {
    cat(file=stderr(), stringr::str_c("get stringid's ", row_start, "-", row_stop), "\n")
    df_temp <- string_id_call("tsv", df, row_start, row_stop, params$string_species)
    if (ncol(df_temp) > 2) {
      df_check <- df_temp
      cat(file=stderr(), stringr::str_c("concat list "), "\n")
      string_IDs <- rbind(string_IDs, df_temp)
    }
    row_start = row_stop + 1
    row_stop = min(row_start + 499, nrow(df))
  }
  
  #set String Background
  cat(file=stderr(), stringr::str_c("string setup...3"), "\n")
  backgroundV <- string_IDs$stringId 
  cat(file=stderr(), stringr::str_c("background data has ", nrow(string_IDs), " lines"), "\n")
  string_db$set_background(backgroundV)
  
  cat(file=stderr(), stringr::str_c("string setup...4"), "\n")
  stats_comp <- read_table_try("stats_comp", params)
  
  for (i in 1:nrow(stats_comp)){
    cat(file=stderr(), stringr::str_c("string setup comp #  ", i), "\n")
    comp_name <- stats_comp$Name[i]
    data_in <- read_table_try(stats_comp$Final_Table_Name[i], params)
    if(input_checkbox_filter_adjpval){
      pval_col <- data_in |> dplyr::select(contains("_adjpval")) 
    }else{
      pval_col <- data_in |> dplyr::select(contains("_pval")) 
    }
    fc2_col <- data_in |> dplyr::select(contains("_fc2")) 
    df <- data.frame(cbind(pval_col, fc2_col, data_in$Accession), stringsAsFactors = FALSE)
    names(df) <- c("pvalue", "logFC", "Uniprot")
    df$logFC <- as.numeric(df$logFC)
    df$pvalue <- as.numeric(df$pvalue)
    df$logFC <- log(df$logFC, 2)
    
    df <- dplyr::left_join(df, string_IDs[, c("queryItem", "stringId")], by=c("Uniprot" = "queryItem"))
    df <- df[stats::complete.cases(df),]
    table_name <- stringr::str_c('String_', stats_comp$Final_Table_Name[i])
    write_table_try(table_name, df, params)
    
    cat(file=stderr(), "", "\n")
    cat(file=stderr(), stringr::str_c("data for ", comp_name, " has ", nrow(df), " lines"), "\n")
  }
  
  gc()
  cat(file=stderr(), stringr::str_c("function setup_string_bg...end"), "\n")
  
  return()
}  

#--------------------------------------------------------------------------------
run_string <- function(session, input, output, params) {
  cat(file=stderr(), stringr::str_c("function run_string..."), "\n")
  showModal(modalDialog("String Analysis...", footer = NULL))  
  
  stats_comp <- read_table_try("stats_comp", params)
  string_check <- sum(stringr::str_count(list_tables(params), "String_"))
  
  if(string_check >= nrow(stats_comp)) {
    cat(file = stderr(), "go string triggered", "\n")
    
    arg_list <- list(input$foldchange_cutoff, input$pvalue_cutoff, input$select_data_comp_string, input$string_direction, 
                     input$protein_number, stats_comp, params)
    bg_run_string <- callr::r_bg(func = run_string_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_run_string.txt"), supervise = TRUE)
    bg_run_string$wait()
    print_stderr("error_run_string.txt")
    string_list <- bg_run_string$get_result()
    string_file_name <- string_list[[1]]
    string_link_network <- string_list[[2]]
    
    cat(file = stderr(), "string list complete, create plot", "\n")
    
    output$string_plot <- renderImage({
      list(src=string_file_name,  
           contentType = 'image/png', width=800, height=700, alt="this is alt text")
    }, deleteFile = FALSE)
    
    
    fullName <- string_list$string_file_name
    output$download_string_plot <- downloadHandler(
      filename = function(){
        stringr::str_c(input$select_data_comp_string, ".png")
      },
      content = function(file){
        file.copy(fullName, file)
      }
    )
    
    # depreciated
    cat(file = stderr(), "create string link", "\n")
    url <- a(string_link_network, href= string_link_network, target="_blank")
    output$string_link <- renderUI({
      tagList("URL link: ", url)
    })
    
  }else{
    shinyalert("Oops!", "Need to run String Setup first...", type = "error")
  }
  
  removeModal()
  cat(file=stderr(), stringr::str_c("function run_string...end"), "\n")
}

#-------------------------------------------------------------------

run_string_bg <- function(input_foldchange_cutoff, input_pvalue_cutoff, input_select_data_comp_string, input_string_direction, 
                          input_protein_number, stats_comp, params){
  cat(file=stderr(), stringr::str_c("function run_string_bg..."), "\n")
  require(httr)
  require(png)
  source("Shiny_File.R")
  
  cat(file=stderr(), "run_string_bg 1", "\n")
  input_fc_up <- log(input_foldchange_cutoff, 2)
  input_fc_down <- log(1/input_foldchange_cutoff, 2)

  cat(file=stderr(), "run_string_bg 2", "\n")
  
  comp_number <- which(stats_comp$Name == input_select_data_comp_string)
  
  table_name <- stringr::str_c("String_", stats_comp$Final_Table_Name[comp_number])
  df <- read_table_try(table_name, params)
  df <- subset(df, pvalue <= input_pvalue_cutoff)

  cat(file=stderr(), stringr::str_c("length of dataframe...", nrow(df)), "\n")
  
  cat(file=stderr(), "run_string_bg 3", "\n")
  
  if (input_string_direction == "Up"){
    df <- subset(df, logFC >= input_fc_up)
  }else if (input_string_direction == "Down"){
    df <- subset(df, logFC <= input_fc_down)
  }else {
    df <- subset(df, logFC >= input_fc_up | logFC <= input_fc_down )
  }
  
  df <- df[order(-df$logFC),]

  cat(file=stderr(), stringr::str_c("length of dataframe...", nrow(df)), "\n")
  cat(file=stderr(), "run_string_bg 4", "\n")
  
  if (nrow(df) > as.numeric(input_protein_number)){
    hits <- df$stringId[1:as.numeric(input_protein_number)]
  }else{
    hits <- df$stringId
  }
  
  cat(file=stderr(), stringr::str_c("number of hits searched...", length(hits)), "\n")
  
  cat(file=stderr(), "run_string_bg 5", "\n")
  
  hit_list <- hits[1]
  for(i in 2:length(hits)){
    hit_list <- stringr::str_c(hit_list,"%0d", hits[i])
  }
  
  cat(file=stderr(), "run_string_bg 6", "\n")
  string_file_name <- stringr::str_c(params$string_path, input_select_data_comp_string, ".png")
  cat(file=stderr(), stringr::str_c("string file name... ", string_file_name ), "\n")
  
  
  cat(file=stderr(), "run_string_bg 7", "\n")
  
  test_hits <<- hit_list  
  string_api <- stringr::str_c("https://string-db.org/api/highres_image/network?identifiers=",
                      hit_list,
                      "&species=", params$string_species, 
                      "&caller_identity=DukeProteomics" )
  
  res <- GET(string_api)
  res_image <- readPNG(res$content)
  writePNG(res_image, target=string_file_name)
  
  #save string png
  cat(file=stderr(), "run_string_bg 8", "\n")
  
  string2_api <- stringr::str_c("https://string-db.org/api/tsv-no-header/get_link?identifiers=", 
                       hit_list,
                       "&species=", params$string_species, 
                       "&caller_identity=DukeProteomics" )

  res_link <- GET(string2_api)
  link_network <- rawToChar(res_link$content)
  cat(file=stderr(), stringr::str_c("string link ", link_network), "\n")
  
  gc()
  cat(file=stderr(), stringr::str_c("function run_string_bg...end"), "\n")
  return(list(string_file_name, link_network))
}


#--------------------------------------------------------------------

run_string_enrich <- function(session, input, output, params){
  cat(file=stderr(), stringr::str_c("function run_string_enrich_bg..."), "\n")
  
  input_fc_up <- log(input$foldchange_cutoff, 2)
  input_fc_down <- log(1/input$foldchange_cutoff, 2)
  
  input_pval <- input$pvalue_cutoff
  
  input_comp <- input$select_data_comp_string_enrich
  
  df <- dpmsr_set$string[[input_comp]]
  cat(file=stderr(), stringr::str_c("dataframe size...", nrow(df)), "\n")
  df <- subset(df, pvalue <= input_pval)
  cat(file=stderr(), stringr::str_c("dataframe subset size...", nrow(df)), "\n")
  
  if (input$string_enrich_direction == "Up"){
    df <- subset(df, logFC >= input_fc_up)
  }else if (input$string_enrich_direction == "Down"){
    df <- subset(df, logFC <= input_fc_down)
  }else {
    df <- subset(df, logFC >= input_fc_up | logFC <= input_fc_down )
  }
  
  df <- df[order(-df$logFC),]
  
  hits <- df$stringId
  
  cat(file=stderr(), stringr::str_c("number of hits searched...", length(hits)), "\n")
  
  enrichment <- dpmsr_set$string$string_db$get_enrichment(hits) #, category = input$select_string_enrich )
  
  gc()
  cat(file=stderr(), stringr::str_c("enrichment output...", nrow(enrichment)), "\n")
  
  cat(file=stderr(), stringr::str_c("function run_string_enrich..end"), "\n")
  return(enrichment)
  
  
}

#--------------------------------------------------------------------

run_string_enrich_bg <- function(params){
  cat(file=stderr(), stringr::str_c("function run_string_enrich_bg..."), "\n")
  input_fc_up <- log(input$foldchange_cutoff, 2)
  input_fc_down <- log(1/input$foldchange_cutoff, 2)
  
  input_pval <- input$pvalue_cutoff
  
  input_comp <- input$select_data_comp_string_enrich
  
  df <- dpmsr_set$string[[input_comp]]
  cat(file=stderr(), stringr::str_c("dataframe size...", nrow(df)), "\n")
  df <- subset(df, pvalue <= input_pval)
  cat(file=stderr(), stringr::str_c("dataframe subset size...", nrow(df)), "\n")
  
  if (input$string_enrich_direction == "Up"){
    df <- subset(df, logFC >= input_fc_up)
  }else if (input$string_enrich_direction == "Down"){
    df <- subset(df, logFC <= input_fc_down)
  }else {
    df <- subset(df, logFC >= input_fc_up | logFC <= input_fc_down )
  }
  
  df <- df[order(-df$logFC),]
  
  hits <- df$stringId
  
  cat(file=stderr(), stringr::str_c("number of hits searched...", length(hits)), "\n")
  
  enrichment <- dpmsr_set$string$string_db$get_enrichment(hits) #, category = input$select_string_enrich )
  
  gc()
  cat(file=stderr(), stringr::str_c("enrichment output...", nrow(enrichment)), "\n")
  
  cat(file=stderr(), stringr::str_c("function run_string_enrich_bg...end"), "\n")
  return(enrichment)
  
  
}





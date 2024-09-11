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






#-------------------------------------------------------------------

run_string <- function(session, input, output){
  require(httr)
  require(png)
  
  cat(file=stderr(), "run string step 1", "\n")
  input_fc_up <- log(input$foldchange_cutoff, 2)
  input_fc_down <- log(1/input$foldchange_cutoff, 2)
  
  input_pval <- input$pvalue_cutoff
  input_comp <- input$select_data_comp_string
  
  cat(file=stderr(), "run string step 2", "\n")
  
  df <- dpmsr_set$string[[input_comp]]
  test1 <<- df
  df <- subset(df, pvalue < input_pval)
  test2 <<-df
  
  cat(file=stderr(), stringr::str_c("length of dataframe...", nrow(df)), "\n")
  
  cat(file=stderr(), "run string step 3", "\n")
  
  if (input$string_direction == "Up"){
    df <- subset(df, logFC >= input_fc_up)
  }else if (input$string_direction == "Down"){
    df <- subset(df, logFC <= input_fc_down)
  }else {
    df <- subset(df, logFC >= input_fc_up | logFC <= input_fc_down )
  }
  
  df <- df[order(-df$logFC),]
  test3 <<-df
  
  cat(file=stderr(), stringr::str_c("length of dataframe...", nrow(df)), "\n")
  cat(file=stderr(), "run string step 4", "\n")
  
  if (nrow(df) > as.numeric(input$protein_number)){
    hits <- df$stringId[1:as.numeric(input$protein_number)]
  }else{
    hits <- df$stringId
  }
  
  cat(file=stderr(), stringr::str_c("number of hits searched...", length(hits)), "\n")
  
  cat(file=stderr(), "run string step 5", "\n")
  
  hit_list <- hits[1]
  for(i in 2:length(hits)){
    hit_list <- stringr::str_c(hit_list,"%0d", hits[i])
  }
  
  cat(file=stderr(), "run string step 6", "\n")
  string_file_name <- stringr::str_c(params$string_path, input_comp, ".png")
  cat(file=stderr(), stringr::str_c("string file name... ", string_file_name ), "\n")
  
  
  cat(file=stderr(), "run string step 7", "\n")
  
  test_hits <<- hit_list  
  string_api <- stringr::str_c("https://string-db.org/api/highres_image/network?identifiers=",
                      hit_list,
                      "&species=", dpmsr_set$string$string_db$species, 
                      "&caller_identity=DukeProteomics" )
  
  res <- GET(string_api)
  res_image <- readPNG(res$content)
  writePNG(res_image, target=string_file_name)
  
  #save string png
  cat(file=stderr(), "run string step 8", "\n")
  
  # string_plot <- try(dpmsr_set$string$string_db$plot_network(hits, add_link = TRUE, add_summary = TRUE), silent = TRUE)
  # cat(file=stderr(), "run string step 9 - plot object created", "\n")
  # 
  # if ( string_plot != "try-error"){
  #   png(filename=string_file_name, units="px", width = 1200, height = 1200)
  #   string_plot
  #   dev.off()
  #   cat(file=stderr(), "run string step 20 - saved plot", "\n")
  # }else{
  #   shinyalert("Oops!", "StringDB server failed to return data...", type = "error")
  # }
  
  # png(filename=string_file_name, units="px", width = 1200, height = 1200)
  # dpmsr_set$string$string_db$plot_network(hits, add_link = TRUE, add_summary = TRUE)
  # dev.off()
  # cat(file=stderr(), "run string step 9 - saved plot", "\n")
  
  
  # cat(file=stderr(), "run string step 8", "\n")
  
  string2_api <- stringr::str_c("https://string-db.org/api/tsv-no-header/get_link?identifiers=", 
                       hit_list,
                       "&species=", dpmsr_set$string$string_db$species, 
                       "&caller_identity=DukeProteomics" )
  
  
  res_link <- GET(string2_api)
  link_network <- rawToChar(res_link$content)
  cat(file=stderr(), stringr::str_c("string link ", link_network), "\n")
  dpmsr_set$string$link_network <<- link_network
  #dpmsr_set$string$link_network <<- substr(link_network, 1, nchar(link_network)-2)
  
  gc()
  return(list("string_file_name" = string_file_name))
}


#--------------------------------------------------------------------

run_string_enrich <- function(input, output){
  
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
  
  return(enrichment)
  
  
}






cat(file = stderr(), "Shiny_ViSEAGO.R", "\n")


#----------------------------------------------------------------------------------------- 
run_go_analysis <- function(session, input, output, db_path){
  cat(file=stderr(), stringr::str_c("Function run_go_analysis..." ), "\n")
  showModal(modalDialog("Processing Go Analysis...", footer = NULL))  
  
  params <- get_params(db_path)
  
  arg_list <- list(input$select_go_data_comp, input$go_direction, input$select_ont_go, params, db_path)
  bg_run_go_analysis <- callr::r_bg(func = run_go_analysis_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_run_go_analysis.txt"), supervise = TRUE)
  bg_run_go_analysis$wait()
  print_stderr("error_run_go_analysis.txt", db_path)
  go_data <- bg_run_go_analysis$get_result()
  
  
  if (class(go_data) != "try-error") {
    arg_list <- list(input$select_go_data_comp, input$checkbox_filter_adjpval, params, db_path)
    bg_setup_go_volcano <- callr::r_bg(func = setup_go_volcano_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_setup_go_volcano.txt"), supervise = TRUE)
    #moved wait step to end, data not need immediately

    options_DT <- list(
      selection = 'single',
      #autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = 500,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0),
          visibile = TRUE,
          "width" = '10',
          className = 'dt-center'
        ),
        list(
          targets = c(1),
          width = '300',
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 50 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
            "}"
          )
        ),
        list(
          targets = c(2,3,4,4,5,6,7),
          visibile = TRUE,
          "width" = '10',
          className = 'dt-center'
        )
      ),
      ordering = TRUE,
      orderClasses = TRUE,
      fixedColumns = list(leftColumns = 1),
      pageLength = 10,
      lengthMenu = c(10, 50, 100, 200)
    )
    
    output$go_table <-  DT::renderDataTable(go_data, rownames = FALSE, #extensions = c("FixedColumns"), 
                                                    selection = 'single', options=options_DT,
                                                    callback = DT::JS('table.page(3).draw(false);'))
                                    
    output$go_volcano_selection <- renderPrint(go_data$GO.ID[as.numeric(unlist(input$go_table_rows_selected)[1])] ) 
    observe({
      updateTextInput(session, "go_volcano_id", 
                      value = go_data$GO.ID[as.numeric(unlist(input$go_table_rows_selected)[1])] ) 
    })
    
  }else{
    shinyalert("Oops!", "Go Analysis failed...", type = "error")
  } 
  
  fullName <- stringr::str_c(params$string_path, input$go_filename)
  output$download_go_table <- downloadHandler(
    file = function(){
      stringr::str_c(input$go_filename)
    },
    content = function(file){
      file.copy(fullName, file)
    }
  )
  
  bg_setup_go_volcano$wait()
  print_stderr("error_setup_go_volcano.txt", db_path)
  
  removeModal()
  cat(file=stderr(), stringr::str_c("Function run_go_analysis..end." ), "\n")
}
#----------------------------------------------------------------------------------------- 

run_go_analysis_bg <- function(input_select_go_data_comp, input_go_direction, input_select_ont_go, params, db_path){
  cat(file=stderr(), stringr::str_c("Function run_go_analysis_bg..." ), "\n")
  require(topGO, quietly = TRUE)
  require(ViSEAGO, quietly = TRUE)
  source('Shiny_File.R')
  source('Shiny_ViSEAGO.R')
  
  stats_comp <- read_table_try("stats_comp", db_path)
  comp_string <- input_select_go_data_comp
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  cat(file = stderr(), stringr::str_c("run_go_analysis_bg...1" ), "\n")
  
  table_name <- stats_comp$Final_Table_Name[comp_number]
  data_in <- read_table_try(table_name, db_path)
  
  if (input_go_direction == 'Up') {
    go_df <- subset(data_in, data_in$Stats == "Up" ) 
  }else if (input_go_direction == 'Down') {
    go_df <- subset(data_in, data_in$Stats == "Down" ) 
  }else{
    go_df <- subset(data_in, data_in$Stats == "Up" | data_in$Stats == "Down") 
  }
  
  cat(file=stderr(), stringr::str_c("Run go analysis...2" ), "\n")

  myGENE2GO <- get_mygene2go(params$tax_choice)
  
  topgodata <- ViSEAGO::create_topGOdata(
    geneSel = go_df$Accession,
    allGenes = data_in$Accession,
    gene2GO = myGENE2GO, 
    ont = input_select_ont_go,
    nodeSize = 5
  )

  cat(file = stderr(), stringr::str_c("Run go analysis...3" ), "\n")  
  fun_resultFisher <- function(topgodata) {
    resultFisher <- topGO::runTest(
      topgodata,
      algorithm = "classic",
      statistic = "fisher"
    )
    return(resultFisher)
  }
  
  
  cat(file = stderr(), stringr::str_c("Run go analysis...4" ), "\n")
  fun_resultKS <- function(topgodata){
    resultKS <- topGO::runTest(
      topgodata,
      algorithm = "classic",
      statistic = "ks"
    )
    return(resultKS)
  }
  
  cat(file = stderr(), stringr::str_c("Run go analysis...5" ), "\n")
  fun_resultKS.elim <- function(topgodata){
    resultKS.elim <- topGO::runTest(
      topgodata,
      algorithm = "elim",
      statistic = "ks"
    )
    return(resultKS.elim)
  }

  resultFisher <- callr::r_bg(func = fun_resultFisher, args = list(topgodata), stderr = stringr::str_c(params$error_path, "//error_resultfisher.txt"), supervise = TRUE)
  resultKS <- callr::r_bg(func = fun_resultKS, args = list(topgodata), stderr = stringr::str_c(params$error_path, "//error_resultks.txt"), supervise = TRUE)
  resultKS.elim <- callr::r_bg(func = fun_resultKS.elim, args = list(topgodata), stderr = stringr::str_c(params$error_path, "//error_resultkselim.txt"), supervise = TRUE)
  
  resultFisher$wait()
  resultKS$wait()
  resultKS.elim$wait()
  
  cat(file = stderr(), stringr::str_c("Run go analysis...6" ), "\n")
  #print_stderr("error_resultfisher.txt")
  #print_stderr("error_resultks.txt")
  #print_stderr("error_resultkselim.txt")
  
  resultFisher <- resultFisher$get_result() 
  resultKS <- resultKS$get_result()   
  resultKS.elim <- resultKS.elim$get_result()  
  
  
  cat(file = stderr(), stringr::str_c("Run go analysis...7" ), "\n")
  
  # merge results from topGO
  allRes <- try(topGO::GenTable(topgodata, classicFisher = resultFisher,
                     classicKS = resultKS, elimKS = resultKS.elim,
                     orderBy = "elimKS", ranksOf = "classicFisher", topNodes = 10)
              )
  
  write_table_try("go_data", allRes, db_path)
  
  cat(file=stderr(), stringr::str_c("Function run_go_analysis_bg...end" ), "\n")
  return(allRes)
}


#----create go lookup data for volcano plots ---------------------
setup_go_volcano_bg <- function(input_select_go_data_comp, input_checkbox_filter_adjpval, params, db_path){  
  cat(file = stderr(), stringr::str_c("Function setup_go_volcano_bg..." ), "\n")
  source('Shiny_File.R')
  source('Shiny_ViSEAGO.R')
  
  stats_comp <- read_table_try("stats_comp", db_path)
  comp_string <- input_select_go_data_comp
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  table_name <- stats_comp$Final_Table_Name[comp_number]
  data_in <- read_table_try(table_name, db_path)
  
  cat(file = stderr(), stringr::str_c("Function setup_go_volcano_bg...1" ), "\n")
  if(input_checkbox_filter_adjpval){
    pval_col <- data_in |> dplyr::select(contains("_adjpval")) 
  }else{
    pval_col <- data_in |> dplyr::select(contains("_pval")) 
  }
  fc2_col <- data_in |> dplyr::select(contains("_fc2")) 
  volcano_df <- data.frame(cbind(data_in$Accession, pval_col, fc2_col), stringsAsFactors = FALSE)
  names(volcano_df) <- c("Accession", "pvalue", "foldchange")
  selection <- as.character(volcano_df$Accession)
  
  cat(file = stderr(), stringr::str_c("Function setup_go_volcano_bg...2" ), "\n")
  
  myGENE2GO <- get_mygene2go(params$tax_choice)
  
  myGENE2GO_df <- myGENE2GO@BP
  
  myGENE2GO_lookup <- data.frame(names(myGENE2GO_df),stringsAsFactors = FALSE)
  myGENE2GO_lookup$Go <- 1
  names(myGENE2GO_lookup)[1] <- "Accession"
  
  for (i in 1:nrow(myGENE2GO_lookup)) {
    test <- myGENE2GO_df[[myGENE2GO_lookup$Accession[i]]]
    test_str <- paste(test, collapse = ", ")
    myGENE2GO_lookup$Go[i] <- test_str
  }
  
  mergedf <- merge(x = volcano_df, y = myGENE2GO_lookup, by.x = "Accession", by.y = "Accession")
  
  write_table_try("go_volcano_mergedf", mergedf, db_path)
  
  cat(file = stderr(), stringr::str_c("Function setup_go_volcano_bg... end" ), "\n")
  return(mergedf)
  }

#-------------------------------------------------------------------------------------------
create_go_volcano <- function(session, input, output, db_path){
  cat(file = stderr(), stringr::str_c("Function create_go_volcano..." ), "\n")
  source('Shiny_ViSEAGO.R')
  source('Shiny_Interactive.R')
  #GO:0032259
  
  params <- get_params(db_path)
  
  if (input$go_volcano_id != ""){
    
    showModal(modalDialog("Preparing Go Volcano...", footer = NULL))  
    
    arg_list <- list(input$select_go_data_comp, input$go_volcano_id, params, db_path)
    bg_create_go_volcano <- callr::r_bg(func = create_go_volcano_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_create_go_volcano.txt"), supervise = TRUE)
    bg_create_go_volcano$wait()
    print_stderr("error_create_go_volcano.txt", db_path)
    volcano_data_list <- bg_create_go_volcano$get_result()
    volcano_data <- volcano_data_list[[1]]
    volcano_go_data <- volcano_data_list[[2]]
    sample_cols <- volcano_data_list[[3]]
    
    interactive_go_volcano(session, input, output, volcano_data)

    options_DT <- list(
      selection = 'single',
      #autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = 500,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0),
          visibile = TRUE,
          "width" = '10',
          className = 'dt-center'
        ),
        list(
          targets = c(sample_cols),
          visibile = TRUE,
          "width" = '10',
          className = 'dt-center'
        ),
        list(
          targets = c(1),
          width = '200',
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 50 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
            "}"
          )
        )
      ),
      ordering = TRUE,
      orderClasses = TRUE,
      fixedColumns = list(leftColumns = 1),
      pageLength = 10,
      lengthMenu = c(10, 50, 100, 200)
    )
    
    output$volcano_data_final <-  DT::renderDataTable(volcano_go_data, rownames = FALSE, #extensions = c("FixedColumns"), 
                                            selection = 'single', options=options_DT,
                                            callback = DT::JS('table.page(3).draw(false);')
    )
    
    fullName <- stringr::str_c(params$string_path, input$go_volcano_filename)
    output$download_go_volcano_table <- downloadHandler(
      file = function(){
        stringr::str_c(input$go_volcano_filename)
      },
      content = function(file){
        file.copy(fullName, file)
      }
    )
    
    
    removeModal()
  }else{
    shinyalert("Oops!", "GO ID is missing!", type = "error")
  }
  

  cat(file = stderr(), stringr::str_c("Function create_go_volcano...end" ), "\n") 
}

#-------------------------------------------------------------------------------------------
create_go_volcano_bg <- function(input_select_go_data_comp, input_go_volcano_id, params, db_path){
  cat(file=stderr(), stringr::str_c("create_go_volcano_bg..." ), "\n")
  source('Shiny_File.R')
  source('Shiny_Misc_Functions.R')

  stats_comp <- read_table_try("stats_comp", db_path)
  comp_string <- input_select_go_data_comp
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  table_name <- stats_comp$Final_Table_Name[comp_number]
  data_in <- read_table_try(table_name, db_path)
  
  mergedf <- read_table_try("go_volcano_mergedf", db_path)
  sub_df <- mergedf[grep(as.character(input_go_volcano_id), mergedf$Go),]
  
  cat(file=stderr(), stringr::str_c("create_go_volcano_bg...1" ), "\n")
  sub_df$log_pvalue <- -log(as.numeric(sub_df$pvalue), 10)
  sub_df$log_fc <- log(as.numeric(sub_df$foldchange), 2)
  
  cat(file=stderr(), stringr::str_c("create_go_volcano_bg...2" ), "\n")
  testdf <- data.frame(cbind(data_in$Accession, data_in$Description))
  colnames(testdf) <- c("Accession", "Description")
  volcano_data <- merge(x=sub_df, y=testdf, by.x="Accession", by.y="Accession")
  
  #-----
  volcano_go_data <- subset(data_in, Accession %in% volcano_data$Accession  )
  
  volcano_go_data_colnames <- colnames(volcano_go_data )
  volcano_go_data_colnames <- gsub("_v_", " v ", volcano_go_data_colnames)
  volcano_go_data_colnames <- gsub("_FC", " FC", volcano_go_data_colnames)
  volcano_go_data_colnames <- gsub("_CV", " CV", volcano_go_data_colnames)
  volcano_go_data_colnames <- gsub("_MF", " MF", volcano_go_data_colnames)
  volcano_go_data_colnames <- gsub("_pval", " pval", volcano_go_data_colnames)
  volcano_go_data_colnames <- gsub("_", ".", volcano_go_data_colnames)
  colnames(volcano_go_data) <-  volcano_go_data_colnames
  
  
  #pval_cols <- which(stringr::str_detect(colnames(volcano_go_data), "pval"))
  #cv_cols <- which(stringr::str_detect(colnames(volcano_go_data), "CV"))
  #fc_cols <- which(stringr::str_detect(colnames(volcano_go_data), "FC"))
  
  start_sample_col <- min(which(stringr::str_detect(colnames(volcano_go_data), gsub("_", ".", stats_comp$FactorsN[comp_number]) )))
  sample_cols <- seq(start_sample_col, (as.numeric(stats_comp$Total[comp_number]) + start_sample_col - 1))   
  
  #volcano_go_data <- volcano_go_data |> dplyr::mutate(dplyr::across(pval_cols, round, 7))
  #volcano_go_data <- volcano_go_data |> dplyr::mutate(dplyr::across(cv_cols, round, 2))
  #volcano_go_data <- volcano_go_data |> dplyr::mutate(dplyr::across(fc_cols, round, 2))
  #volcano_go_data <- volcano_go_data |> dplyr::mutate(dplyr::across(sample_cols, round, 2))
  
  volcano_go_data <- round_columns(volcano_go_data, "pval", 7)
  volcano_go_data <- round_columns(volcano_go_data, "CV", 2)
  volcano_go_data <- round_columns(volcano_go_data, "FC", 2)
  volcano_go_data <- round_columns(volcano_go_data, sample_cols, 2)
  
  write_table_try("volcano_go_data", volcano_go_data, db_path)
  
  cat(file=stderr(), stringr::str_c("create_go_volcano...end" ), "\n")
  return(list(volcano_data, volcano_go_data, sample_cols))
}



#----------------------------------------------------------------------------------------- 
get_mygene2go <- function(tax_choice) {
  source('Shiny_File.R')
  
  if (tax_choice == "Human") {
    mygene2go <- loadRData(stringr::str_c(getwd(), "/Pathway_myGENE2GO_Human"))
  }
  
  if (tax_choice == "Mouse") {
    mygene2go <- loadRData(stringr::str_c(getwd(), "/Pathway_myGENE2GO_Mouse"))
  }
  
  if (tax_choice == "Rat") {
    mygene2go <- loadRData(stringr::str_c(getwd(), "/Pathway_myGENE2GO_Rat"))
  } 
  
  return(mygene2go) 
}
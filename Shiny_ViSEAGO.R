cat(file = stderr(), "Shiny_ViSEAGO.R", "\n")


#----------------------------------------------------------------------------------------- 
run_go_analysis <- function(session, input, output, params){
  cat(file=stderr(), stringr::str_c("Function run_go_analysis..." ), "\n")
  showModal(modalDialog("Processing Go Analysis...", footer = NULL))  
  
  arg_list <- list(input$select_data_comp_wiki, input$wiki_direction, params)
  bg_run_go_analysis <- callr::r_bg(func = run_go_analysis_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_run_go_analysis.txt"), supervise = TRUE)
  bg_run_go_analysis$wait()
  print_stderr("error_run_go_analysis.txt")
  go_data <- bg_run_go_analysis$get_result()
  
  
  if (class(go_data) != "try-error") {
    arg_list <- list(input$select_go_data_comp, params)
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
          targets = c(0,1,2),
          visibile = TRUE,
          "width" = '10',
          className = 'dt-center'
        ),
        list(
          targets = c(3),
          visibile = TRUE,
          "width" = '20',
          className = 'dt-center'
        ),
        list(
          targets = c(4),
          width = '10',
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 35 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
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
    
    output$go_table <-  DT::renderDataTable(go_data, rownames = FALSE, extensions = c("FixedColumns"), 
                                                    selection = 'single', options=options_DT,
                                                    callback = DT::JS('table.page(3).draw(false);')
    )
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
  print_stderr("error_setup_go_volcano.txt")
  
  removeModal()
  cat(file=stderr(), stringr::str_c("Function run_go_analysis..end." ), "\n")
}
#----------------------------------------------------------------------------------------- 

run_go_analysis_bg <- function(input_select_go_data_comp, input_go_direction, input_select_ont_go, params){
  cat(file=stderr(), stringr::str_c("Function run_go_analysis_bg..." ), "\n")
  require(topGO)
  source('Shiny_File.R')
  
  stats_comp <- read_table_try("stats_comp", params)
  comp_string <- input_select_go_data_comp
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  cat(file = stderr(), str_c("run_go_analysis_bg...1" ), "\n")
  
  table_name <- stats_comp$Final_Table_Name[comp_number]
  data_in <- read_table_try(table_name, params)
  
  if (input_go_direction == 'Up') {
    go_df <- subset(data_in, data_in$Stats == "Up" ) 
  }else if (input_go_direction == 'Down') {
    go_df <- subset(data_in, data_in$Stats == "Down" ) 
  }else{
    go_df <- subset(data_in, data_in$Stats == "Up" | data_in$Stats == "Down") 
  }
  
  cat(file=stderr(), str_c("Run go analysis...2" ), "\n")

  myGENE2GO <- get_mygene2go(params$tax_choice)
  
  topgodata <- ViSEAGO::create_topGOdata(
    geneSel = go_df$Accession,
    allGenes = data_in$Accession,
    gene2GO = myGENE2GO, 
    ont = input_select_ont_go,
    nodeSize = 5
  )

  cat(file = stderr(), str_c("Run go analysis...3" ), "\n")  
  fun_resultFisher <- function(topgodata) {
    resultFisher <- topGO::runTest(
      topgodata,
      algorithm = "classic",
      statistic = "fisher"
    )
    return(resultFisher)
  }
  
  
  cat(file = stderr(), str_c("Run go analysis...4" ), "\n")
  fun_resultKS <- function(topgodata){
    resultKS <- topGO::runTest(
      topgodata,
      algorithm = "classic",
      statistic = "ks"
    )
    return(resultKS)
  }
  
  cat(file = stderr(), str_c("Run go analysis...5" ), "\n")
  fun_resultKS.elim <- function(topgodata){
    resultKS.elim <- topGO::runTest(
      topgodata,
      algorithm = "elim",
      statistic = "ks"
    )
    return(resultKS.elim)
  }
  
  resultFisher <- callr::r_bg(fun_resultFisher, args = list(topgodata),  
                          stderr = stringr::str_c(params$error_path, "//error_resultfisher.txt"), supervise = TRUE)
  resultKS <- callr::r_bg(fun_resultKS, args = list(topgodata), 
                          stderr = stringr::str_c(params$error_path, "//error_resultks.txt"), supervise = TRUE)
  resultKS.elim <- callr::r_bg(fun_resultKS.elim, args = list(topgodata), 
                          stderr = stringr::str_c(params$error_path, "//error_resultkselim.txt"), supervise = TRUE)
  
  resultFisher$wait()
  resultKS$wait()
  resultKS.elim$wait()
  
  print_stderr("error_resultfisher.txt")
  print_stderr("error_resultks.txt")
  print_stderr("error_resultkselim.txt")
  
  resultFisher <- resultFisher$get_result() 
  resultKS <- resultKS$get_result()   
  resultKS.elim <- resultKS.elim$get_result()  
  
  
  cat(file = stderr(), str_c("Run go analysis...6" ), "\n")
  
  # merge results from topGO
  allRes <- try(topGo::GenTable(topgodata, classicFisher = resultFisher,
                     classicKS = resultKS, elimKS = resultKS.elim,
                     orderBy = "elimKS", ranksOf = "classicFisher", topNodes = 10)
              )
  
  write_table_try("go_data", allRes, params)
  
  cat(file=stderr(), stringr::str_c("Function run_go_analysis_bg...end" ), "\n")
  return(allRes)
}


#----create go lookup data for volcano plots ---------------------
setup_go_volcano_bg <- function(input_select_go_data_comp, params){  
  cat(file = stderr(), str_c("Function setup_go_volcano_bg..." ), "\n")
  source('Shiny_File.R')
  
  stats_comp <- read_table_try("stats_comp", params)
  comp_string <- input_select_go_data_comp
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  table_name <- stats_comp$Final_Table_Name[comp_number]
  data_in <- read_table_try(table_name, params)
  
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
  
  write_table_try("go_volcano", mergedf, params)
  
  cat(file = stderr(), str_c("Function setup_go_volcano_bg... end" ), "\n")
  return(mergedf)
  }

#-------------------------------------------------------------------------------------------
create_go_volcano <- function(session, input, output, params){
  cat(file = stderr(), str_c("Function create_go_volcano..." ), "\n")
  
  if (input$go_volcano_id != ""){
    
    showModal(modalDialog("Preparing Go Volcano...", footer = NULL))  
    
    comp_string <- input$select_data_comp_go
    comp_number <- which(grepl(comp_string, dpmsr_set$y$stats$groups$comp_name))
    data_in <- dpmsr_set$data$stats[[comp_string]]
    
    volcano_data <- interactive_go_volcano(session, input, output)
    volcano_go_data <- subset(data_in, Accession %in% volcano_data$Accession  )
    
    volcano_go_data_colnames <- colnames(volcano_go_data )
    volcano_go_data_colnames <- gsub("_v_", " v ", volcano_go_data_colnames)
    volcano_go_data_colnames <- gsub("_FC", " FC", volcano_go_data_colnames)
    volcano_go_data_colnames <- gsub("_CV", " CV", volcano_go_data_colnames)
    volcano_go_data_colnames <- gsub("_MF", " MF", volcano_go_data_colnames)
    volcano_go_data_colnames <- gsub("_pval", " pval", volcano_go_data_colnames)
    volcano_go_data_colnames <- gsub("_", ".", volcano_go_data_colnames)
    colnames(volcano_go_data) <-  volcano_go_data_colnames
    
    
    pval_cols <- colnames(volcano_go_data %>% dplyr::select(contains("pval") ) )
    sample_cols <- c(colnames(volcano_go_data %>% dplyr::select(contains("Normalized"))),
                     colnames(volcano_go_data %>% dplyr::select(contains("Imputed"))) )
    sample_col_numbers <- list(match(sample_cols, names(volcano_go_data)))
    sample_col_numbers <- unlist(sample_col_numbers)
    cv_cols <- colnames(volcano_go_data %>% dplyr::select(contains("CV") ) )
    mf_cols <- colnames(volcano_go_data %>% dplyr::select(contains("MF") ) )
    
    volcano_DT <-  DT::datatable(volcano_go_data,
                                 rownames = FALSE,
                                 extensions = c("FixedColumns"), #, "Buttons"),
                                 options=list(
                                   #dom = 'Bfrtipl',
                                   autoWidth = TRUE,
                                   scrollX = TRUE,
                                   scrollY=500,
                                   scrollCollapse=TRUE,
                                   columnDefs = list(list(targets = c(0), visibile = TRUE, "width"='30', className = 'dt-center'),
                                                     list(targets = c(2), visible = TRUE, "width"='20', className = 'dt-center'),
                                                     list(
                                                       targets = c(1),
                                                       width = '250',
                                                       render = JS(
                                                         "function(data, type, row, meta) {",
                                                         "return type === 'display' && data.length > 35 ?",
                                                         "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
                                                         "}")
                                                     ),
                                                     list(
                                                       targets = c(3),
                                                       width = '100',
                                                       render = JS(
                                                         "function(data, type, row, meta) {",
                                                         "return type === 'display' && data.length > 20 ?",
                                                         "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                         "}")
                                                     )
                                   ),
                                   ordering = TRUE,
                                   orderClasses= TRUE,
                                   fixedColumns = list(leftColumns = 1),
                                   pageLength = 100, lengthMenu = c(10,50,100,200)),
                                 #buttons=c('copy', 'csv', 'excelHtml5', 'pdf')),
                                 callback = JS('table.page(3).draw(false);'
                                 ))
    
    volcano_DT <- volcano_DT %>%  formatRound(columns=c(sample_col_numbers), digits=0)
    
    output$volcano_data_final <-  DT::renderDataTable({volcano_DT })
    
    Simple_Excel(volcano_go_data, "GoVolcano", str_c(dpmsr_set$file$string, "GoVolcano_", input$select_data_comp_go, "_", input$go_volcano_id, "_", 
                                                     input$select_ont_go, ".xlsx", collapse = " "))
    
    
    fullName <- str_c(dpmsr_set$file$string, "GoVolcano_", input$select_data_comp_go, "_", input$go_volcano_id, "_", 
                      input$select_ont_go, ".xlsx", collapse = " ")
    output$download_go_volcano_table <- downloadHandler(
      filename = function(){
        str_c("GoVolcano_", input$select_data_comp_go, "_", input$go_volcano_id, "_", 
              input$select_ont_go, ".xlsx", collapse = " ")
      },
      content = function(file){
        file.copy(fullName, file)
      }
    )
    removeModal()
  }else{
    shinyalert("Oops!", "GO ID is missing!", type = "error")
  }
  

  cat(file = stderr(), str_c("Function create_go_volcano...end" ), "\n") 
}

#-------------------------------------------------------------------------------------------
create_go_volcano_bg <- function(session, input, output){
  cat(file=stderr(), str_c("create_go_volcano...1" ), "\n")
  comp_string <- input$select_data_comp_go
  comp_number <- which(grepl(comp_string, dpmsr_set$y$stats$groups$comp_name))
  data_in <- dpmsr_set$data$stats[[comp_string]]
  
  sub_df <- dpmsr_set$data$pathway$mergedf[grep(as.character(input$go_volcano_id), dpmsr_set$data$pathway$mergedf$Go),]
  
  cat(file=stderr(), str_c("create_go_volcano...2" ), "\n")
  sub_df$log_pvalue <- -log(as.numeric(sub_df$pvalue), 10)
  sub_df$log_fc <- log(as.numeric(sub_df$foldchange), 2)
  
  cat(file=stderr(), str_c("create_go_volcano...3" ), "\n")
  testdf <- data.frame(cbind(data_in$Accession, data_in$Description))
  colnames(testdf) <- c("Accession", "Description")
  volcano_data <- merge(x=sub_df, y=testdf, by.x="Accession", by.y="Accession")
  cat(file=stderr(), str_c("create_go_volcano...end" ), "\n")
  gc()
  return(volcano_data)
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
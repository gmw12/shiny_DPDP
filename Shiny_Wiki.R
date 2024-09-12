cat(file = stderr(), "Shiny_Wiki.R", "\n")

#----------------------------------------------------------------------------------------- 
run_wiki <- function(session, input, output, params){
  cat(file=stderr(), stringr::str_c("Functgion run_wiki..." ), "\n")
  showModal(modalDialog("Processing Wiki Pathway...", footer = NULL))  
  
  arg_list <- list(input$select_data_comp_wiki, input$wiki_direction, params)
  bg_run_wiki <- callr::r_bg(func = run_wiki_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_run_wiki.txt"), supervise = TRUE)
  bg_run_wiki$wait()
  print_stderr("error_run_wiki.txt")
  wiki_data <- bg_run_wiki$get_result()
  
  if ( class(wiki_data) != "try-error"){
    options_DT <- list(
      selection = 'single',
      #dom = 'Bfrtipl',
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = 500,
      scrollCollapse = TRUE,
      columnDefs = list(
        list(
          targets = c(0,1,2,3,4,5),
          visibile = TRUE,
          "width" = '6',
          className = 'dt-center'
        ),
        list(
          targets = c(10),
          width = '15',
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 35 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;",
            "}"
          )
        ),
        list(
          targets = c(6),
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
    
    output$wiki_table <-  DT::renderDataTable(wiki_data, rownames = FALSE, extensions = c("FixedColumns"), 
                                                selection = 'single', options=options_DT,
                                                callback = DT::JS('table.page(3).draw(false);')
        )
  }else{
    shinyalert("Oops!", "Wiki Pathway enrichment failed due to insufficient gene IDs mapping to pathways", type = "error")
  }
  
  fullName <- stringr::str_c(params$string_path, input$wiki_data_filename)
  
  output$download_wiki_table <- downloadHandler(
    file = function(){
      stringr::str_c(input$wiki_data_filename)
    },
    content = function(file){
      file.copy(fullName, file)
    }
  )

  removeModal()
  cat(file=stderr(), stringr::str_c("Function run_wiki...end" ), "\n")
}


#----------------------------------------------------------------------------------------- 
run_wiki_bg <- function(input_select_data_comp_wiki, input_wiki_direction, params){
  cat(file=stderr(), stringr::str_c("Function run_wiki_bg..." ), "\n")
  require(clusterProfiler)
  source('Shiny_File.R')
  source('Shiny_Wiki.R')
  
  stats_comp <- read_table_try("stats_comp", params)
  #comp_string <- stats_comp$Name[1]
  comp_string <- input_select_data_comp_wiki
  comp_number <- which(grepl(comp_string, stats_comp$Name))
  
  cat(file=stderr(), stringr::str_c("Run wiki...1" ), "\n")
  
  table_name <- stats_comp$Final_Table_Name[comp_number]
  data_in <- read_table_try(table_name, params)
  
  if(input_wiki_direction == 'Up') {
    go_df <- subset(data_in, data_in$Stats == "Up" ) 
  }else if (input_wiki_direction == 'Down') {
    go_df <- subset(data_in, data_in$Stats == "Down" ) 
  }else{
    go_df <- subset(data_in, data_in$Stats == "Up" | data_in$Stats == "Down") 
  }
  
  cat(file=stderr(), stringr::str_c("Run wiki...2" ), "\n")
  atest <- go_df$Accession
  tax_db <- get_tax_db(params$tax_choice)
  wp2gene <- get_wp2gene(params$tax_choice)
  
  test.df <- clusterProfiler::bitr(atest, fromType = "UNIPROT",
                                   toType = c("ENTREZID", "ENSEMBL", "SYMBOL","GENENAME","PATH", "ONTOLOGY"),
                                   OrgDb = tax_db)
  
  cat(file=stderr(), stringr::str_c("Run wiki...3" ), "\n")
  universe.df <- clusterProfiler::bitr(data_in$Accession, fromType = "UNIPROT",
                      toType = c("ENTREZID", "ENSEMBL", "SYMBOL","GENENAME","PATH", "ONTOLOGY"),
                      OrgDb = tax_db)
  
  cat(file=stderr(), stringr::str_c("Run wiki...4" ), "\n")
  wpid2gene <- wp2gene |> dplyr::select(wpid, gene) #TERM2GENE
  wpid2name <- wp2gene |> dplyr::select(wpid, name) #TERM2NAME
  
  cat(file=stderr(), stringr::str_c("Run wiki...5" ), "\n")
  ewp <- clusterProfiler::enricher(gene=test.df$ENTREZID, universe=universe.df$ENTREZID, TERM2GENE = wpid2gene, TERM2NAME = wpid2name)
  ewp <- try(clusterProfiler::setReadable(ewp, tax_db, keyType = "ENTREZID"))
  
  cat(file=stderr(), stringr::str_c("Run wiki...6" ), "\n")
  #Simple_Excel_bg(ewp@result, "Wiki", stringr::str_c(dpmsr_set$file$string, "Wiki_", input$select_data_comp_wiki, "_", 
  #                                          input$select_ont_wiki,input$select_level_wiki, ".xlsx", collapse = " "))
  #ggo_result <<- ggo@result[1:5]
  #ggo_result <- ggo_result[order(-ggo_result$Count),]
  cat(file=stderr(), stringr::str_c("Functgion run_wiki_bg...end" ), "\n")
  gc()
  
  df <- ewp@result
  df$RichFactor <- round(df$RichFactor,3)
  df$FoldEnrichment <- round(df$FoldEnrichment,3)
  df$zScore <- round(df$zScore,3)
  df$pvalue <- round(df$pvalue,5)
  df$p.adjust <- round(df$p.adjust,3)
  df$qvalue <- round(df$qvalue,3)
  
  write_table_try("wiki_table", df, params)
  
  return(df)
}


#----------------------------------------------------------------------------------------- 
run_profile <- function(session, input, output){
  require(clusterProfiler)
  cat(file=stderr(), "run_profile ...1", "\n")
  comp_string <- input$select_data_comp_profile
  comp_number <- which(grepl(comp_string, dpmsr_set$y$stats$groups$comp_name))
  
  cat(file=stderr(), stringr::str_c("Run go profile..." ), "\n")
  
  data_in <- dpmsr_set$data$stats[[comp_string]]
  
  cat(file=stderr(), "run_profile ...2", "\n")
  if(input$profile_direction == 'Up') {
    go_df <- subset(data_in, data_in$Stats == "Up" ) 
  }else if (input$profile_direction == 'Down') {
    go_df <- subset(data_in, data_in$Stats == "Down" ) 
  }else{
    go_df <- subset(data_in, data_in$Stats == "Up" | data_in$Stats == "Down") 
  }
  
  atest <- go_df$Accession
  
  cat(file=stderr(), "run_profile ...3", "\n")
  test.df <- bitr(atest, fromType = "UNIPROT",
                  toType = c("ENTREZID", "ENSEMBL", "SYMBOL","GENENAME","PATH", "ONTOLOGY"),
                  OrgDb = tax_db)
  
  cat(file=stderr(), "run_profile ...4", "\n")
  ggo <- groupGO(gene     = test.df$ENTREZID,
                 OrgDb    = tax_db,
                 ont      = input$select_ont_profile,
                 level    = as.numeric(input$select_level_profile),
                 readable = TRUE) 
  
  Simple_Excel_bg(ggo, "Go_Profile", stringr::str_c(dpmsr_set$file$string, "GO_Profile_", input$select_data_comp_profile, "_", 
                                           input$select_ont_profile,input$select_level_profile, ".xlsx", collapse = " "))
  
  #ggo_result <<- ggo@result[1:5]
  #ggo_result <- ggo_result[order(-ggo_result$Count),]
  
  #test_ggo <<- ggo
  
  #save profile png
  profile_plot_name <- stringr::str_c(dpmsr_set$file$string, "GO_Profile_", input$select_data_comp_profile, "_", 
                             input$select_ont_profile,input$select_level_profile, ".png", collapse = " ")
  p <- barplot(ggo, title = stringr::str_c("Go Profile ", input$select_ont_profile, " level=", input$select_level_profile), 
               drop=TRUE, showCategory=12, order=TRUE)
  ggsave(profile_plot_name, p, width=10, height=8)
  
  gc()
  return(ggo)
}

#----------------------------------------------------------------------------------------- 
get_tax_db <- function(tax_choice) {

  if (tax_choice == "Human") {
    cat(file = stderr(), stringr::str_c("Load tax library...", tax_choice ), "\n")
    library(org.Hs.eg.db)
    tax_db <- org.Hs.eg.db}
  
  if (tax_choice == "Mouse") {
    cat(file = stderr(), stringr::str_c("Load tax library...", tax_choice ), "\n")
    library(org.Mm.eg.db)
    tax_db <- org.Mm.eg.db}
  
  if (tax_choice == "Rat") {
    cat(file = stderr(), stringr::str_c("Load tax library...", tax_choice ), "\n")
    library(org.Rn.eg.db)
    tax_db <- org.Rn.eg.db} 
 
  return(tax_db) 
}

#----------------------------------------------------------------------------------------- 
get_wp2gene <- function(tax_choice) {
  source('Shiny_File.R')
  
  if (tax_choice == "Human") {
    wp2gene <- loadRData(stringr::str_c(getwd(), "/Pathway_wp2gene_Human"))
    }
  
  if (tax_choice == "Mouse") {
    wp2gene <- loadRData(stringr::str_c(getwd(), "/Pathway_wp2gene_Mouse"))
    }
  
  if (tax_choice == "Rat") {
    wp2gene <- loadRData(stringr::str_c(getwd(), "/Pathway_wp2gene_Rat"))
    } 
  
  return(wp2gene) 
}


cat(file = stderr(), "server.R started", "\n")
#app_version <- '2024.01.05'

options(shiny.maxRequestSize = 4000*1024^2)

source("Shiny_Setup.R")
source("Shiny_Startup.R")

if (!exists('params')) {
  cat(file = stderr(), "params file does not exist...", "\n")
  create_default_params() 
  
  #set user
  set_user()
  
} else {
  cat(file = stderr(), "params file exists...", "\n")
}

shinyServer(function(session, input, output) {
  cat(file = stderr(), "\n\n", "Shiny Server started ...1", "\n")
  showModal(modalDialog("Loading app...", footer = NULL))
  
  source("Shiny_Source.R")
  hide_enable(session, input, output)
  set_comp_names(session, input,output)
  
  #app start conditions
  source('Shiny_UI_Update.R')
  app_startup(session, input, output)

  #set file choosers
  set_file_choosers(session, input, output)
  
  #update UI
  ui_render_load_design(session, input, output)
  if (params$data_path != "")  {
    create_design_table(session, input, output)
    create_stats_design_table(session, input, output)
    }
  if (table_exists("summary_cv"))  {create_cv_table(session, input, output, params)}
  removeModal()
  
  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_design_file, {
    
    cat(file = stderr(), "\n\n", "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_design_file)) {
      
      #load design from excel, create database
      load_design_file(session, input, output)

      #set file locations
      file_set()
      
      #update UI
      ui_render_load_design(session, input, output)
      
      #create design table, save to database
      create_design_table(session, input, output)
      create_stats_design_table(session, input, output)
      
      #backup design table
      design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
      save_data(design_sbf$datapath)
      
    }

  })
  

  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_data_file, {
    
    cat(file = stderr(), "\n\n","sfb_data_file button clicked...", "\n")
    
    if (is.list(input$sfb_data_file)) {
      
      #read data files
      load_data_file(session, input, output)
      output$data_file_name <- renderText({get_data_file_name()})
      
      #update UI
      ui_render_load_data(session, input, output)
      ui_render_load_design(session, input, output)
      
    }

  }) 

   
#------------------------------------------------------------------------------------------------------  
#accept parameters
 observeEvent(input$accept_parameters, {
   cat(file = stderr(), "\n\n", "accept parameters clicked", "\n")

   #save inputs to params
   parameter_widget_save(session, input, output)
   
   #organize design table with parameter input
   set_sample_groups(session, input, output, params)

   # Organize raw data into selected columns and info column names
   prepare_data(session, input, output)
   
   # order columns and rename sample column names
   order_rename_columns()
   
   # gather info on raw data for ui
   meta_data("raw")
   
   # create graphs
   parameter_create_plots(sesion, input, output, params)
   
   #create histogram and calculate cutoff values
   filter_histogram_plot(sesion, input, output, params, "precursor_start", "Precursor_Start_Histogram")

 })

  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$noise_inflection_apply, {
    cat(file = stderr(), "noise inflection apply clicked", "\n")  
    
    #calculate noise inflection and create plot
    noise_inflection(session, input, output, params)
    
    render_noise_graphs(session, input, output)
    
    cat(file = stderr(), "noise inflection apply...end", "\n")  
  })
  
  
  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$noise_apply, {
    cat(file = stderr(), "noise apply clicked", "\n")  

    #save noise inputs to params file
    noise_widget_save(session, input, output)
    
    #remove noise from df and save new df
    noise_remove(session, input, output, params)
      
    cat(file = stderr(), "noise apply...end", "\n")
  })
  
  
  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$filter_cutoff, {
    cat(file = stderr(), "filter cutoff clicked", "\n")  
    
    #save filter inputs to params file
    filter_widget_save(session, input, output)
    
    #create histogram and calculate cutoff values
    filter_histogram_plot(sesion, input, output, params, "precursor_noise", "Precursor_NoiseFiltered_Histogram")
    
    cat(file = stderr(), "filter cutoff...end", "\n")  
  })
    
#------------------------------------------------------------------------------------------------------  
 
 observeEvent(input$filter_apply, {
   cat(file = stderr(), "filter apply clicked", "\n")
   
   #save filter inputs to params file
   filter_widget_save(session, input, output)
   
   #apply data filters, save to new dataframe
   filter_data(session, input, output)
   
   #create plots
   filter_create_plots(sesion, input, output, params)
   
   #gather meta data for filtered dataframe
   meta_data("filter")
   
   cat(file = stderr(), "filter apply clicked...end", "\n")
 })
 
  
#------------------------------------------------------------------------------------------------------   

 observeEvent(input$norm_parameters, {
   cat(file = stderr(), "norm parameters clicked", "\n")
   
   norm_widget_save(session, input, output)
   
   norm_filter()
   
   render_norm_graphs(session, input, output)

   cat(file = stderr(), "norm parameters clicked...end", "\n")
 })
 
#------------------------------------------------------------------------------------------------------  
 
 observeEvent(input$norm_apply, {
   cat(file = stderr(), "norm apply clicked", "\n")
   
   norm_apply_widget_save(session, input, output)
   
   norm_apply()
   
   render_norm_apply_graphs(session, input, output)
   
   cat(file = stderr(), "norm apply clicked...end", "\n")
 })

   
#------------------------------------------------------------------------------------------------------   
  
 observeEvent(input$impute_parameters, {
   cat(file = stderr(), "impute parameters clicked", "\n")

   #save inputs
   impute_apply_widget_save(session, input, output)
   
   #gather info on raw data for ui, create random table for imputing
   impute_meta_data()
   
   #render text
   #render_impute_parameters(session, input, output) 
   
   #render plots
   create_impute_table(session, input, output, params)
   impute_create_plots(session, input, output, params)
   render_impute_graphs(session, input, output)
   
   cat(file = stderr(), "impute parameters clicked...end", "\n")
 })
 
  #------------------------------------------------------------------------------------------------------   
  
  observeEvent(input$impute_apply, {
    cat(file = stderr(), "impute_apply clicked", "\n")
    
    impute_apply(session, input, output)
    
    cat(file = stderr(), "impute_apply clicked...end", "\n")
  })
 
  #------------------------------------------------------------------------------------------------------   
  
  observeEvent(input$rollup_apply, {
    cat(file = stderr(), "rollup_apply clicked", "\n")
    
    #save parameters
    rollup_widget_save(session, input, output) 
    
    #start rollup
    rollup_apply(session, input, output)
    
    #start qc stats
    qc_stats(session, input, output, params)
    create_cv_table(session, input, output, params)
    render_qc_graphs(session, input, output)
    
    cat(file = stderr(), "rollup_apply clicked...end", "\n")
  }) 
 
  
  #------------------------------------------------------------------------------------------------------   
  observeEvent(input$qc_protein_plot_apply, {
    cat(file = stderr(), "\n", "protein_plot_apply clicked", "\n")
    
    qc_protein_plots(session, input, output, params)
    
    cat(file = stderr(), "\n", "protein_plot_apply clicked...end", "\n")
  }) 
  
  #------------------------------------------------------------------------------------------------------   
  observeEvent(input$qc_spike_plot_apply, {
    cat(file = stderr(), "protein_plot_apply clicked", "\n")
    
    qc_spike_plots(session, input, output, params)
    
    cat(file = stderr(), "protein_plot_apply clicked...end", "\n")
  }) 
  
  #------------------------------------------------------------------------------------------------------   
  observeEvent(input$stat_options, {
    cat(file = stderr(), "stat_options clicked", "\n")
    
    save_stat_options(session, input, output, params)
    
    cat(file = stderr(), "stat_options clicked... end", "\n")
  }) 
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$check_stats, {
    cat(file = stderr(), "check_stats clicked...", "\n")
    
    #save parameters
    stat_widget_save(session, input, output) 
    
    check_comp_names(session, input, output)

    if (is.null(input$comp_spqc)) {
      shinyalert("Oops!", "Please choose and SPQC group!", type = "error")
    }

    cat(file = stderr(), "check_stats clicked...end", "\n")
  })  
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$start_stats, {
    cat(file = stderr(), "start_stats clicked...", "\n")
    
    stat_calc(session, input, output)

    cat(file = stderr(), "start_stats clicked...end", "\n")
  })   
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$save_stats, {
    cat(file = stderr(), "save_stats clicked...", "\n")
    
    stats_Final_Excel(session, input, output, params)
    
    cat(file = stderr(), "save_stats clicked...end", "\n")
  })   
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$create_stats_plots1, {
    cat(file = stderr(), "create_stats_plots1 clicked...", "\n")
    
    if (input$plot_type1 == "Volcano") {
      if (length(input$stats_plot_comp1) == 1)  {
        if(input$stats_plot_comp1 != params$comp_spqc) {
          create_volcano(session, input, output, params, plot_number = 1)
        }
      }
    }else{
      create_plot(session, input, output, params, plot_number = 1) 
    }
    
    cat(file = stderr(), "create_stats_plots1...end", "\n")
  })   
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$create_stats_plots2, {
    cat(file = stderr(), "create_stats_plots2 clicked...", "\n")
    
    if (input$plot_type2 == "Volcano") {
      if (length(input$stats_plot_comp2) == 1)  {
        if(input$stats_plot_comp2 != params$comp_spqc) {
          create_volcano(session, input, output, params, plot_number = 2)
        }
      }
    }else{
      create_plot(session, input, output, params, plot_number = 2) 
    }
    
    cat(file = stderr(), "create_stats_plots2...end", "\n")
  })     

  
  #------------------------------------------------------------------------------------------------------------- 
  observeEvent(input$stats_data_show, { 
    cat(file = stderr(), "stats_data_show clicked..." , "\n")
    
    create_stats_data_table(session, input, output, params)
    
    cat(file = stderr(), "stats_data_show clicked...end" , "\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$stats_data_save, { 
    cat(file = stderr(), "stats_data_save clicked..." , "\n")
    
    stats_data_save_excel(session, input, output, params)
    
    cat(file = stderr(), "stats_data_save clicked...end" , "\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$stats_oneprotein_data_save, { 
    
    cat(file = stderr(), "stats saving OneProtein datatable to excel..." , "\n") 
    
    oneprotein_data_save_excel(session, input, output, params) 
    
    cat(file = stderr(), "stats saving OneProtein datatable to excel...end" , "\n") 
    
  })
  
#-------------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$stats_data_update, {
    
    if (length(dpmsr_set$y$stats$accession_stat) != 0) {  
      
      if(dpmsr_set$x$final_data_output == "Protein"){  
        cat(file = stderr(), str_c("accesion to mark as not stat signifigant...   ", dpmsr_set$y$stats$accession_stat) , "\n") 
        row_number <- which(dpmsr_set$data$stats[[input$stats_select_data_comp]]$Accession == dpmsr_set$y$stats$accession_stat)
        cat(file = stderr(), str_c("row to mark as not stat signifigant...   ", row_number) , "\n") 
        dpmsr_set$data$stats[[input$stats_select_data_comp]]$Stats[row_number] <<- ""
      }else{
        cat(file = stderr(), str_c("peptide to mark as not stat signifigant...   ", dpmsr_set$y$stats$sequence_stat) , "\n") 
        row_number <- which(dpmsr_set$data$stats[[input$stats_select_data_comp]]$Sequence   == dpmsr_set$y$stats$sequence_stat &
                              dpmsr_set$data$stats[[input$stats_select_data_comp]]$Modifications   == dpmsr_set$y$stats$modification_stat)
        cat(file = stderr(), str_c("row to mark as not stat signifigant...   ", row_number) , "\n") 
        dpmsr_set$data$stats[[input$stats_select_data_comp]]$Stats[row_number] <<- ""
      }
    }else{
      
      shinyalert("Oops!", "Cannot update stat", type = "error")
      
    }
    
  })
  
  
#-------------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$create_stats_oneprotein_plots, { 
    cat(file = stderr(), "stats_data_show clicked..." , "\n")
    
    create_stats_oneprotein_plots(session, input, output, params)
    
    cat(file = stderr(), "stats_data_show clicked...end" , "\n")
    
  })
  
  
  #-------------------------------------------------------------------------------------------------------------  
  
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$set_pathway, {
    
    showModal(modalDialog("Downloading and Setting up databases...", footer = NULL))  
    set_pathway(input, output, session)
    removeModal()
    
  }
  )
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$wiki_show, {
    showModal(modalDialog("Wiki Pathway Enrichment...", footer = NULL))  
    
    wiki_data <<- try(run_wiki(session, input, output), silent = TRUE)
    
    if ( class(wiki_data) != "try-error"){
      output$wiki_table<- renderRHandsontable({
        rhandsontable(wiki_data, rowHeaders = NULL, readOnly = TRUE) %>%
          hot_cols(colWidths = 80, halign = "htCenter" ) %>%
          hot_col(col = "ID", halign = "htCenter", colWidths = 120) %>%
          hot_col(col = "Description", halign = "htCenter", colWidths = 250) %>%
          hot_col(col = "pvalue", halign = "htCenter", colWidths = 150, format = '0.000000') %>% 
          hot_col(col = "p.adjust", halign = "htCenter", colWidths = 150, format = '0.000000') %>% 
          hot_col(col = "qvalue", halign = "htCenter", colWidths = 100, format = '0.00000') %>% 
          hot_col(col = "geneID", halign = "htCenter", colWidths = 400)
      })
    }else{
      shinyalert("Oops!", "Wiki Pathway enrichment failed due to insufficient gene IDs mapping to pathways", type = "error")
    }
    
    removeModal()
    
    
    fullName <- str_c(dpmsr_set$file$string, "Wiki_", input$select_data_comp_wiki, "_", 
                      input$select_ont_wiki,input$select_level_wiki, ".xlsx", collapse = " ")
    output$download_wiki_table <- downloadHandler(
      filename = function(){
        str_c("Wiki_", input$select_data_comp_wiki, "_", 
              input$select_ont_wiki,input$select_level_wiki, ".xlsx", collapse = " ")
      },
      content = function(file){
        file.copy(fullName, file)
      }
    )
    
    
    
  }
  )
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$profile_show, {
    showModal(modalDialog("Creating Go Profile...", footer = NULL))  
    
    profile_data <- try(run_profile(session, input, output), silent=TRUE)
    
    if ( class(profile_data) != "try-error"){
      
      go_profile_result <- profile_data@result[1:5]
      go_profile_result <- go_profile_result[order(-go_profile_result$Count),]
      
      
      output$go_profile_table <- renderRHandsontable({
        rhandsontable(go_profile_result, rowHeaders = NULL, readOnly = TRUE) %>%
          hot_cols(colWidths = 80, halign = "htCenter" ) %>%
          hot_col(col = "ID", halign = "htCenter", colWidths = 120) %>%
          hot_col(col = "Description", halign = "htCenter", colWidths = 250) %>%
          hot_col(col = "Count", halign = "htCenter", colWidths = 50) %>%
          hot_col(col = "GeneRatio", halign = "htCenter", colWidths = 100) %>%
          hot_col(col = "geneID", halign = "htCenter", colWidths = 150)
      })
      
      #diret plot of profile
      output$go_profile_plot <- renderPlot({
        barplot(profile_data, title = str_c("Go Profile ", input$select_ont_profile, " level=", input$select_level_profile), 
                drop=TRUE, showCategory=12, order=TRUE)
      })
      
    }else{
      shinyalert("Oops!", "Go Profile enrichment failed...", type = "error")
    }
    
    removeModal()
    
    
    table_fullName <- str_c(dpmsr_set$file$string, "GO_Profile_", input$select_data_comp_profile, "_", 
                            input$select_ont_profile,input$select_level_profile, ".xlsx", collapse = " ")
    output$download_go_profile_table <- downloadHandler(
      filename = function(){
        str_c("GO_Profile_", input$select_data_comp_profile, "_", 
              input$select_ont_profile,input$select_level_profile, ".xlsx", collapse = " ")
      },
      content = function(file){
        file.copy(table_fullName, file)
      }
    )
    
    plot_fullName <- str_c(dpmsr_set$file$string, "GO_Profile_", input$select_data_comp_profile, "_", 
                           input$select_ont_profile,input$select_level_profile, ".png", collapse = " ")
    output$download_go_profile_plot <- downloadHandler(
      filename = function(){
        str_c("GO_Profile_", input$select_data_comp_profile, "_", 
              input$select_ont_profile,input$select_level_profile, ".png", collapse = " ")
      },
      content = function(file){
        file.copy(plot_fullName, file)
      }
    )
    
    
    
    
  }
  )
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$go_show, {
    showModal(modalDialog("Go Enrichment Analysis...", footer = NULL))  
    
    go_data <- try(run_go_analysis(session, input, output), silent = TRUE)
    #go_data <- goresult
    
    if ( class(go_data) != "try-error") {
      dpmsr_set$data$pathway$mergedf <<- setup_go_volcano(session, input, output)
      #go_data <- try(go_data[order(go_data$pvalue),], silent = TRUE)
      
      output$go_table <- renderRHandsontable({
        rhandsontable(go_data, rowHeaders = NULL, readOnly = FALSE) %>%
          hot_cols(colWidths = 80, halign = "htCenter" ) %>%
          hot_col(col = "GO.ID", halign = "htCenter", colWidths = 100) %>%
          hot_col(col = "Term", halign = "htCenter", colWidths = 200) %>%
          hot_col(col = "Rank in classicFisher", halign = "htCenter", colWidths = 200)   %>%
          hot_col(col = "classicFisher", halign = "htCenter", colWidths = 100)  
      })
    }else{
      shinyalert("Oops!", "Go Analysis failed...", type = "error")
    } 
    
    fullName <- str_c(dpmsr_set$file$string, "GO_", input$select_data_comp_go, "_", 
                      input$select_ont_go, ".xlsx", collapse = " ")
    output$download_go_table <- downloadHandler(
      filename = function(){
        str_c("GO_", input$select_data_comp_go, "_", 
              input$select_ont_go, ".xlsx", collapse = " ")
      },
      content = function(file){
        file.copy(fullName, file)
      }
    )
    
    removeModal()
    
  }
  )   
  
  
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$go_volcano, {
    
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
  }
  )  
  
  
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$go_string, {
    
    if(length(dpmsr_set$string) > dpmsr_set$x$comp_number) {
      cat(file = stderr(), "go string triggered", "\n")
      showModal(modalDialog("String Analysis...", footer = NULL))  
      
      string_list <- run_string(session, input, output)
      
      cat(file = stderr(), "string list complete, create plot", "\n")
      
      output$string_plot <- renderImage({
        list(src=string_list$string_file_name,  
             contentType = 'image/png', width=1200, height=1200, alt="this is alt text")
      }, deleteFile = FALSE)
      
      
      fullName <- string_list$string_file_name
      output$download_string_plot <- downloadHandler(
        filename = function(){
          str_c(input$select_data_comp_string, ".png")
        },
        content = function(file){
          file.copy(fullName, file)
        }
      )
      
      # depreciated
      cat(file = stderr(), "create string link", "\n")
      #output$string_link <- renderText({dpmsr_set$string$link_network})
      #output$string_link <- renderText({string_list$linkthis})
      url <- a(dpmsr_set$string$link_network, href= dpmsr_set$string$link_network, target="_blank")
      output$string_link <- renderUI({
        tagList("URL link: ", url)
      })
      
      removeModal()
      
    }else{
      shinyalert("Oops!", "Need to run String Setup first...", type = "error")
    }    
  }
  ) 
  
  
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$go_string_enrich, {
    
    if(!is.null("dpmsr_set$string")) {
      showModal(modalDialog("Working...", footer = NULL))  
      string_result <- run_string_enrich(input, output)
      
      string_result <- dplyr::rename(string_result, genes = number_of_genes)
      string_result <- dplyr::rename(string_result, genes_background = number_of_genes_in_background)
      
      output$string_table<- renderRHandsontable({
        rhandsontable(string_result, rowHeaders = NULL, readOnly = TRUE) %>%
          hot_cols(colWidths = 80, halign = "htCenter" ) %>%
          hot_col(col = "term", halign = "htCenter", colWidths = 100) %>%
          hot_col(col = "genes_background", halign = "htCenter", colWidths = 100) %>%
          hot_col(col = "preferredNames",  halign = "htLeft", colWidths = 200) %>%
          hot_col(col = "inputGenes", halign = "htCenter", colWidths = 200) %>%
          hot_col(col = "p_value", halign = "htCenter", colWidths = 180, format = '0.0000000000000') %>% 
          hot_col(col = "fdr", halign = "htCenter", colWidths = 180, format = '0.0000000000000') %>% 
          hot_col(col = "description",  halign = "htLeft", colWidths = 400)
      })
      removeModal()
      
      
      
      fullName <- str_c(dpmsr_set$file$string, input$select_data_comp_string_enrich, "_", input$select_string_enrich, ".xlsx", collapse = " ")
      Simple_Excel_bg(string_result, "String", fullName)
      
      output$download_string_enrich_table <- downloadHandler(
        filename = function(){
          str_c(input$select_data_comp_string_enrich, "_", input$select_string_enrich, ".xlsx", collapse = " ")
        },
        content = function(file){
          file.copy(fullName, file)
        }
      )
      
      
    }else{
      
      shinyalert("Oops!", "Need to run String Setup first...", type = "error")
    }
    
  }
  )
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
      
#-------------------------------------------------------------------------------------------------------------      
#------------------------------------------------------------------------------------------------------   
 observeEvent(input$clean_environment, {
   cat(file = stderr(), "load clean environment", "\n")
   try(rm(list = ls(envir = .GlobalEnv), pos = .GlobalEnv, inherits = FALSE))
   gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
   source("Shiny_Libraries.R")
   source("Shiny_Source.R")
   set_user()
   session$reload()  
 })

 
 
 
     
})

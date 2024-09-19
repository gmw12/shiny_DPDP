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
  
  # observe({
  #   browser_info <<- (shinybrowser::get_all_info())
  #   height_factor <<- browser_info$dimensions$height / 1200
  # })
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
      load_data_file(session, input, output, params)
      
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
   prepare_data(session, input, output, params)
   
   # order columns and rename sample column names
   order_rename_columns()
   
   if (params$raw_data_format != "protein") {
     # gather info on raw data for ui
     meta_data("raw")
     
     # create graphs
     parameter_create_plots(sesion, input, output, params)
     
     #create histogram and calculate cutoff values
     filter_histogram_plot(sesion, input, output, params, "precursor_start", "Precursor_Start_Histogram")
  }
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
   #meta_data("filter")
   
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
    rollup_apply(session, input, output, params)
    
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
  observeEvent(input$set_pathway, {
    cat(file = stderr(), "set_pathway clicked..." , "\n")
    
    source("Shiny_Pathway.R")
    set_pathway(input, output, session, params)

    cat(file = stderr(), "set_pathway clicked...end" , "\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$wiki_show, {
    cat(file = stderr(), "wiki_show clicked..." , "\n") 
    
    source("Shiny_Wiki.R")
    run_wiki(session, input, output, params)
  
    cat(file = stderr(), "wiki_show clicked...end" , "\n") 
    
  })
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$wiki_data_save, { 
    cat(file = stderr(), "wiki_data_save clicked..." , "\n")
    
    db_table_to_Excel("wiki_table",  "wiki pathways", stringr::str_c(params$string_path, input$wiki_data_filename), params)
    
    cat(file = stderr(), "wiki_data_save clicked...end" , "\n")
  })
  #-------------------------------------------------------------------------------------------------------------

  observeEvent(input$profile_go_show, {
    cat(file = stderr(), "profile_go_show clicked..." , "\n")
    
    source("Shiny_Wiki.R")
    run_profile_go(session, input, output, params)
    
    cat(file = stderr(), "profile_go_show clicked...end" , "\n")
  })
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$profile_go_excel, { 
    cat(file = stderr(), "profile_go_excel clicked..." , "\n")
    
    db_table_to_Excel("go_profile_result",  "go profile", stringr::str_c(params$string_path, input$go_profile_filename), params)
    
    cat(file = stderr(), "profile_go_excel clicked...end" , "\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$go_show, {
    cat(file = stderr(), "profile_go_show clicked..." , "\n")
    
    source("Shiny_ViSEAGO.R")
    run_go_analysis(session, input, output, params)
    
    cat(file = stderr(), "profile_go_show clicked...end" , "\n")
  }
  )   
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$go_excel, { 
    cat(file = stderr(), "go_excel clicked..." , "\n")
    
    db_table_to_Excel("go_data",  "go analysis", stringr::str_c(params$string_path, input$go_filename), params)
    
    cat(file = stderr(), "go_excel clicked...end" , "\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$start_go_volcano, {
    cat(file = stderr(), "start_go_volcano clicked..." , "\n")
    
    source('Shiny_ViSEAGO.R')
    create_go_volcano(session, input, output, params)
    
    cat(file = stderr(), "start_go_volcano clicked...end" , "\n")
  })  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$go_volcano_excel, { 
    cat(file = stderr(), "go_volcano_excel clicked..." , "\n")
    
    db_table_to_Excel("go_volcano_data",  "go volcano data", stringr::str_c(params$string_path, input$go_volcano_filename), params)
    
    cat(file = stderr(), "go_volcano_excel clicked...end" , "\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$get_string, {
    cat(file = stderr(), "get_string clicked...", "\n")
    
    source('Shiny_String.R')
    run_string(session, input, output, params)
    
    cat(file = stderr(), "get_string clicked...end", "\n")
  }) 
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$get_string_enrich, {
    cat(file = stderr(), "get_string_enrich clicked...", "\n")
    
    source('Shiny_String.R')
    run_string_enrich(session, input, output, params)
    
    cat(file = stderr(), "get_string_enrich clicked...end", "\n")
  }) 
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$string_enrich_data_save, { 
    cat(file = stderr(), "string_enrich_data_save clicked..." , "\n")
    
    db_table_to_Excel("string_enrichment",  "string_enrich", stringr::str_c(params$string_path, input$string_enrich_data_filename), params)
    
    cat(file = stderr(), "string_enrich_data_save clicked...end" , "\n")
  })
  


  
  
  
      
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

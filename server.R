options(shiny.maxRequestSize = 4000*1024^2)
cat(file = stderr(), "server.R started", "\n")

#app_version <- '2024.01.05'

source("Shiny_Setup.R")
source("Shiny_Startup.R")

if (!exists('db_path')) {
  cat(file = stderr(), "initialize... db_path not found...", "\n")
  
  #clear memory
  rm(list = ls())
  gc()
  #   .rs.restartR()
  
  #set user
  set_user()

} else {
  cat(file = stderr(), "initialize... db_path found...", "\n")
}

shinyServer(function(session, input, output) {
  cat(file = stderr(), "\n\n", "Shiny Server started ...1", "\n")
  showModal(modalDialog("Loading app...", footer = NULL))
  
  source("Shiny_Source.R")
  
  #set file choosers
  set_file_choosers(session, input, output, volumes)
  
  if (exists('db_path')) {
    cat(file = stderr(), "db_path exists...", "\n\n")
    
    #update UI
    ui_render_load_design(session, input, output, db_path)
    
    if (get_param("data_path", db_path) != "")  {
      create_design_table(session, input, output, db_path)
      create_stats_design_table(session, input, output, db_path)
    }
    
    if (table_exists("summary_cv", db_path))  {create_cv_table(session, input, output, db_path)}
    
    #load menu
    load_menu(session, input, output, db_path)
    
    #app start conditions
    app_startup(session, input, output, db_path)
    
    hide_enable(session, input, output, db_path)
    
  }else {
    cat(file = stderr(), "db_path does NOT exist...", "\n\n")
    
    #fresh start, create default params
    #create_default_params(volumes, python_path) 
    #update_widgets(session, input, output, db_path)
    
    #load menu
    load_menu_start(session, input, output)
    
    #set file_prefix
    updateTextInput(session, "file_prefix", value = str_c("project_", strftime(Sys.time(), format = "%m%d%y")))
  }
  
  #set comp ui titles "Comparison 1"
  set_comp_names(session, input,output)
  
  #button observers
  observe_buttons(session, input, output)
  
  #create message for customer version
  if (site_user != "dpmsr") {
    shinyalert("Welcome!", "Please select 'Load' on the left panel.  Click 'Browse' to select file.  When the file is 100% uploaded then click 'Process File'.", type = "success")
  }
  
  
  removeModal()
  
  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_design_file, {
    
    cat(file = stderr(), "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_design_file)) {

      #load design from excel, create database
      load_design_file(session, input, output)
      
      #set file locations
      file_set(db_path)
      
      #update UI
      ui_render_load_design(session, input, output, db_path)
      
      #create design table, save to database
      create_design_table(session, input, output, db_path)
      create_stats_design_table(session, input, output, db_path)
      
      #backup design table
      design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
      save_data(design_sbf$datapath, db_path)
      
      #reset default dd for data
      set_file_choosers_data(session, input, output, volumes) 
      
    }
    cat(file = stderr(), "sfb_design_file button clicked...end", "\n\n\n")
  })
  

  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_data_file, {
    
    cat(file = stderr(), "sfb_data_file button clicked...", "\n")
    
    if (is.list(input$sfb_data_file)) {
      
      #read data files
      load_data_file(session, input, output, db_path)
      
      #output$data_file_name <- renderText({get_data_file_name()})
      
      #update UI
      ui_render_load_data(session, input, output, db_path)
      ui_render_load_design(session, input, output, db_path)
      
      #re-load menu
      load_menu(session, input, output, db_path)
      
      #update with auto detected params
      update_widgets_parameters(session, input, output, db_path)
    }
    
    cat(file = stderr(), "sfb_data_file button clicked...end", "\n\n\n")
  }) 

  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_archive_file, {
    
    cat(file = stderr(), "sfb_archive_file button clicked...", "\n")
    
    if (is.list(input$sfb_archive_file)) {
      cat(file = stderr(), "\n\n","sfb_archive_file button clicked...1", "\n")
      
      #copy zip file contents to database dir, load params
      archive_name <- load_archive_file(session, input, output)
      output$archive_file_name <- renderText({archive_name})
      
      #update UI
      ui_render_load_data(session, input, output, db_path)
      ui_render_load_design(session, input, output, db_path)
      app_startup(session, input, output, db_path)
      set_comp_names(session, input,output)
      create_design_table(session, input, output, db_path)
      create_stats_design_table(session, input, output, db_path)
      if (table_exists("summary_cv", db_path))  {create_cv_table(session, input, output, db_path)}
      
      #load menu
      load_menu(session, input, output, db_path)
      
      cat(file = stderr(), "sfb_archive_file button clicked...end", "\n\n\n")
    }
    
  }) 
  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$load_customer_archive_file, {
    
    cat(file = stderr(), "sfb_archive_customer_file button clicked...", "\n")
    showModal(modalDialog("Processing File...", footer = NULL))
    
    req(input$sfb_archive_customer_file)
    
    if (!is.null(input$sfb_archive_customer_file)) {fileUploaded <- TRUE  }  
    
    if (fileUploaded) {
      cat(file = stderr(), "sfb_archive_customer_file button clicked...1", "\n")
      
      #copy zip file contents to database dir, load params
      archive_name <- load_archive_file(session, input, output)
      params <- get_params()
      
      output$archive_file_name_customer <- renderText({archive_name})
      
      #update UI
      ui_render_load_data(session, input, output, db_path)
      ui_render_load_design(session, input, output, db_path)
      app_startup(session, input, output, db_path)
      set_comp_names(session, input,output)
      create_design_table(session, input, output, db_path)
      create_stats_design_table(session, input, output, db_path)
      if (table_exists("summary_cv", db_path))  {create_cv_table(session, input, output, db_path)}
      
      #load menu
      load_menu(session, input, output, db_path)
      
      removeModal()
      
      shinyalert("Success!", "File Processed.  Please select 'Stats' or 'Pathway' on the left panel to expand options", type = "success")  
      
      cat(file = stderr(), "sfb_archive_customer_file button clicked...end", "\n\n\n")
    }
    
  }) 
  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  # observeEvent(input$sfb_archive_customer_file, {
  #   
  #   cat(file = stderr(), "\n\n","sfb_archive_customer_file button clicked...", "\n")
  #   
  #   if (is.list(input$sfb_archive_customer_file)) {
  #     cat(file = stderr(), "\n\n","sfb_archive_customer_file button clicked...1", "\n")
  #     
  #     #copy zip file contents to database dir, load params
  #     archive_name <- load_archive_file(session, input, output)
  #     
  #     output$archive_file_name_customer <- renderText({archive_name})
  #     
  #     #update UI
  #     ui_render_load_data(session, input, output, db_path)
  #     ui_render_load_design(session, input, output)
  #     app_startup(session, input, output)
  #     set_comp_names(session, input,output)
  #     create_design_table(session, input, output, db_path)
  #     create_stats_design_table(session, input, output, db_path)
  #     if (table_exists("summary_cv", db_path))  {create_cv_table(session, input, output, db_path)}
  #     
  #     #load menu
  #     load_menu(session, input, output, db_path)
  #     
  #     cat(file = stderr(), "\n\n","sfb_archive_customer_file button clicked...end", "\n")
  #   }
  #   
  # }) 
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$motif_fasta_file, {
    
    cat(file = stderr(), "motif_fasta_file button clicked...", "\n")
    
    if (is.list(input$motif_fasta_file)) {
      cat(file = stderr(), "\n\n","motif_fasta_file button clicked...1", "\n")
      
      motif_path <- parseFilePaths(volumes, input$motif_fasta_file)
      output$fasta_file_name <- renderText({basename(motif_path$datapath)})

      
      cat(file = stderr(), "motif_fasta_file button clicked...end", "\n\n\n")
    }
    
  }) 
  
#------------------------------------------------------------------------------------------------------  
#accept parameters
 observeEvent(input$accept_parameters, {
   cat(file = stderr(), "accept parameters clicked...", "\n")

   #save inputs to params
   parameter_widget_save(session, input, output, db_path)
   
   #organize design table with parameter input
   set_sample_groups(db_path)

   # Organize raw data into selected columns and info column names
   prepare_data(session, input, output, db_path)
   
   # order columns and rename sample column names, check sample ID
   order_rename_columns(db_path)

   if (get_param('raw_data_format', db_path) != "protein") {
     # gather info on raw data for ui
     meta_data("start", db_path)
     
     # create graphs
     parameter_create_plots(sesion, input, output, db_path)
     
     #create histogram and calculate cutoff values
     filter_histogram_plot(sesion, input, output, db_path, "precursor_start", "Precursor_Start_Histogram")
   }else{
    # since skipping portion of app, need to load widgets for stats
     ui_render_parameters(session, input, output, db_path)
     update_stat_choices(session, input, output, db_path)
   }
   
   #update filter group choices
   update_widgets_filters(session, input, output, db_path) 
   
   #re-load menu
   load_menu(session, input, output, db_path)
   
   cat(file = stderr(), "accept parameters clicked...end", "\n\n\n")
 })

  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$noise_inflection_apply, {
    cat(file = stderr(), "noise inflection apply clicked", "\n")  
    
    #calculate noise inflection and create plot
    noise_inflection(session, input, output, db_path)
    
    render_noise_graphs(session, input, output, db_path)
    
    cat(file = stderr(), "noise inflection apply...end", "\n\n\n")  
  })
  
  
  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$noise_apply, {
    cat(file = stderr(), "noise apply clicked...", "\n")  

    #save noise inputs to params file
    noise_widget_save(session, input, output, db_path)
    
    #remove noise from df and save new df
    noise_remove(session, input, output, db_path)
      
    cat(file = stderr(), "noise apply...end", "\n\n\n")
  })
  
  
  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$filter_cutoff, {
    cat(file = stderr(), "filter cutoff clicked...", "\n")  
    
    #save filter inputs to params file
    filter_widget_save(session, input, output, db_path)
    
    #create histogram and calculate cutoff values
    filter_histogram_plot(sesion, input, output, db_path, "precursor_noise", "Precursor_NoiseFiltered_Histogram")
    
    cat(file = stderr(), "filter cutoff...end", "\n\n\n")  
  })
    
#------------------------------------------------------------------------------------------------------  
 
 observeEvent(input$filter_apply, {
   cat(file = stderr(), "filter apply clicked...", "\n")
   
   #save filter inputs to params file
   filter_widget_save(session, input, output, db_path)
   
   #apply data filters, save to new dataframe
   filter_data(session, input, output, db_path)
   
   #create plots
   filter_create_plots(sesion, input, output, db_path)
   
   #gather meta data for filtered dataframe
   #meta_data("filter")
   
   cat(file = stderr(), "filter apply clicked...end", "\n\n\n")
 })
 
  
#------------------------------------------------------------------------------------------------------   

 observeEvent(input$norm_parameters, {
   cat(file = stderr(), "norm parameters clicked...", "\n")
   
   norm_widget_save(session, input, output, db_path)
   
   norm_filter(db_path)
   
   render_norm_graphs(session, input, output, db_path)

   cat(file = stderr(), "norm parameters clicked...end", "\n\n\n")
 })
 
#------------------------------------------------------------------------------------------------------  
 
 observeEvent(input$norm_apply, {
   cat(file = stderr(), "norm apply clicked...", "\n")
   
   norm_apply_widget_save(session, input, output, db_path)
   
   norm_apply(session, input, ouput, db_path)
   
   render_norm_apply_graphs(session, input, output, db_path)
   
   cat(file = stderr(), "norm apply clicked...end", "\n\n\n")
 })

   
#------------------------------------------------------------------------------------------------------   
  
 observeEvent(input$impute_parameters, {
   cat(file = stderr(), "impute parameters clicked...", "\n")

   #save inputs
   impute_apply_widget_save(session, input, outpu, db_path)
   
   #gather info on raw data for ui, create random table for imputing
   impute_meta_data(db_path)
   
   #render text
   #render_impute_parameters(session, input, output) 
   
   #render plots
   create_impute_table(session, input, output, db_path)
   impute_create_plots(session, input, output, db_path)
   render_impute_graphs(session, input, output, db_path)
   
   cat(file = stderr(), "impute parameters clicked...end", "\n\n\n")
 })
 
  #------------------------------------------------------------------------------------------------------   
  
  observeEvent(input$impute_apply, {
    cat(file = stderr(), "impute_apply clicked...", "\n")
    
    impute_apply(session, input, output, db_path)
    
    #rollup/sum to peptide if PTM analysis (skipping rollup menuItem)
    if(get_param('data_output', db_path) == "Peptide") {
      cat(file = stderr(), "impute_apply clicked... output = Peptide", "\n")
      rollup_apply(session, input, output, db_path)
      #start qc stats
      qc_stats(session, input, output, db_path)
      create_cv_table(session, input, output, db_path)
      render_qc_graphs(session, input, output, db_path)
    }
    
    cat(file = stderr(), "impute_apply clicked...end", "\n\n\n")
  })
 
  #------------------------------------------------------------------------------------------------------   
  
  observeEvent(input$rollup_apply, {
    cat(file = stderr(), "rollup_apply clicked...", "\n")
    
    #save parameters
    rollup_widget_save(session, input, output, db_path) 
    
    #start rollup
    rollup_apply(session, input, output, db_path)
    
    #start qc stats
    qc_stats(session, input, output, db_path)
    create_cv_table(session, input, output, db_path)
    render_qc_graphs(session, input, output, db_path)
    
    cat(file = stderr(), "rollup_apply clicked...end", "\n\n\n")
  }) 
 
  
  #------------------------------------------------------------------------------------------------------   
  observeEvent(input$qc_protein_plot_apply, {
    cat(file = stderr(), "\n", "protein_plot_apply clicked...", "\n")
    
    qc_protein_plots(session, input, output, db_path)
    
    cat(file = stderr(), "\n", "protein_plot_apply clicked...end", "\n\n\n")
  }) 
  
  #------------------------------------------------------------------------------------------------------   
  observeEvent(input$qc_spike_plot_apply, {
    cat(file = stderr(), "protein_plot_apply clicked...", "\n")
    
    qc_spike_plots(session, input, output, db_path)
    
    cat(file = stderr(), "protein_plot_apply clicked...end", "\n\n\n")
  }) 
  
  #------------------------------------------------------------------------------------------------------   
  observeEvent(input$stat_options, {
    cat(file = stderr(), "stat_options clicked...", "\n")
    
    save_stat_options(session, input, output, db_path)
    
    cat(file = stderr(), "stat_options clicked... end", "\n\n\n")
  }) 
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$check_stats, {
    cat(file = stderr(), "check_stats clicked...", "\n")
    
    #save parameters
    stat_widget_save(session, input, output, db_path) 
    
    check_comp_names(session, input, output, db_path)

    update_widgets_stats(session, input, output, db_path)
    
    if (is.null(input$comp_spqc)) {
      shinyalert("SPQC", "No SPQC group was selected!", type = "warning")
    }

    cat(file = stderr(), "check_stats clicked...end", "\n\n\n")
  })  
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$start_stats, {
    cat(file = stderr(), "start_stats clicked...", "\n")
    
    stat_calc(session, input, output,db_path)

    cat(file = stderr(), "start_stats clicked...end", "\n\n\n")
  })   
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$save_stats, {
    cat(file = stderr(), "save_stats clicked...", "\n")
    
    stats_Final_Excel(session, input, output, db_path)
    
    cat(file = stderr(), "save_stats clicked...end", "\n\n\n")
  })   
  
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$create_stats_plots1, {
    cat(file = stderr(), "create_stats_plots1 clicked...", "\n")
    
    if (input$plot_type1 == "Volcano") {
      if (length(input$stats_plot_comp1) == 1)  {
        if(input$stats_plot_comp1 != get_param('comp_spqc', db_path)) {
          create_volcano(session, input, output, db_path, plot_number = 1)
        }
      }else{
        shinyalert("Oops!", "Cannot create volcano plot, pick only 1 comparison", type = "error")
      }
    }else{
      create_plot(session, input, output, db_path, plot_number = 1) 
    }
    
    cat(file = stderr(), "create_stats_plots1...end", "\n\n\n")
  })   
  
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$create_stats_plots2, {
    cat(file = stderr(), "create_stats_plots2 clicked...", "\n")
    
    if (input$plot_type2 == "Volcano") {
      if (length(input$stats_plot_comp2) == 1)  {
        if(input$stats_plot_comp2 != get_param('comp_spqc', db_path)) {
          create_volcano(session, input, output, db_path, plot_number = 2)
        }
      }
    }else{
      create_plot(session, input, output, db_path, plot_number = 2) 
    }
    
    cat(file = stderr(), "create_stats_plots2...end", "\n\n\n")
  })     

  
  #------------------------------------------------------------------------------------------------------------- 
  observeEvent(input$stats_data_show, { 
    cat(file = stderr(), "stats_data_show clicked..." , "\n")
    
    create_stats_data_table(session, input, output, db_path)
    
    cat(file = stderr(), "stats_data_show clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$stats_data_save, { 
    cat(file = stderr(), "stats_data_save clicked..." , "\n")
    
    stats_data_save_excel(session, input, output, db_path)
    
    cat(file = stderr(), "stats_data_save clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$stats_oneprotein_data_save, { 
    
    cat(file = stderr(), "stats saving OneProtein datatable to excel..." , "\n") 
    
    oneprotein_data_save_excel(session, input, output, db_path) 
    
    cat(file = stderr(), "stats saving OneProtein datatable to excel...end" , "\n\n\n") 
    
  })
  
#-------------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$stats_data_update, {
    cat(file = stderr(), "stats_data_update..." , "\n")
    
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
    cat(file = stderr(), "stats_data_update...end" , "\n\n\n") 
  })
  
#-------------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$create_stats_oneprotein_plots, { 
    cat(file = stderr(), "create_stats_oneprotein_plots..." , "\n")
    
    create_stats_oneprotein_plots(session, input, output, db_path)
    
    cat(file = stderr(), "create_stats_oneprotein_plots...end" , "\n\n\n")
    
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$create_stats_onepeptide_plots, { 
    cat(file = stderr(), "create_stats_onepeptide_plots..." , "\n")
    
    create_stats_onepeptide_plots(session, input, output, db_path)
    
    cat(file = stderr(), "create_stats_onepeptide_plots...end" , "\n\n\n")
    
  })
  
#---------------------------------------------------------------------------------------------------------
 
  
#-------------------------------------------------------------------------------------------------------------
  observeEvent(input$set_pathway, {
    cat(file = stderr(), "set_pathway clicked..." , "\n")
    
    source("Shiny_Pathway.R")
    set_pathway(input, output, session, db_path)

    cat(file = stderr(), "set_pathway clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$wiki_show, {
    cat(file = stderr(), "wiki_show clicked..." , "\n") 
    
    source("Shiny_Wiki.R")
    run_wiki(session, input, output, db_path)
    
    db_table_to_Excel("wiki_table",  "wiki pathways", stringr::str_c(get_param('string_path', db_path), input$wiki_data_filename), db_path)
    
    cat(file = stderr(), "wiki_show clicked...end" , "\n\n\n") 
    
  })
  #-------------------------------------------------------------------------------------------------------------  
  # observeEvent(input$wiki_data_save, { 
  #   cat(file = stderr(), "wiki_data_save clicked..." , "\n")
  #   
  #   db_table_to_Excel("wiki_table",  "wiki pathways", stringr::str_c(params$string_path, input$wiki_data_filename), db_path)
  #   
  #   cat(file = stderr(), "wiki_data_save clicked...end" , "\n")
  # })
  #-------------------------------------------------------------------------------------------------------------

  observeEvent(input$profile_go_show, {
    cat(file = stderr(), "profile_go_show clicked..." , "\n")
    
    source("Shiny_Wiki.R")
    run_profile_go(session, input, output, db_path)
    
    db_table_to_Excel("go_profile_result",  "go profile", stringr::str_c(get_param('string_path', db_path), input$go_profile_filename), db_path)
    
    cat(file = stderr(), "profile_go_show clicked...end" , "\n\n\n")
  })
  #-------------------------------------------------------------------------------------------------------------
  #-------------------------------------------------------------------------------------------------------------  
  # observeEvent(input$profile_go_excel, { 
  #   cat(file = stderr(), "profile_go_excel clicked..." , "\n")
  #   
  #   db_table_to_Excel("go_profile_result",  "go profile", stringr::str_c(params$string_path, input$go_profile_filename), db_path)
  #   
  #   cat(file = stderr(), "profile_go_excel clicked...end" , "\n")
  # })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$go_show, {
    cat(file = stderr(), "profile_go_show clicked..." , "\n")
    
    source("Shiny_ViSEAGO.R")
    run_go_analysis(session, input, output, db_path)
    
    db_table_to_Excel("go_data",  "go analysis", stringr::str_c(get_param('string_path', db_path), input$go_filename), db_path)
    
    cat(file = stderr(), "profile_go_show clicked...end" , "\n\n\n")
  }
  )   
  
  #-------------------------------------------------------------------------------------------------------------  
  # observeEvent(input$go_excel, { 
  #   cat(file = stderr(), "go_excel clicked..." , "\n")
  #   
  #   db_table_to_Excel("go_data",  "go analysis", stringr::str_c(params$string_path, input$go_filename), db_path)
  #   
  #   cat(file = stderr(), "go_excel clicked...end" , "\n")
  # })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$start_go_volcano, {
    cat(file = stderr(), "start_go_volcano clicked..." , "\n")
    
    source('Shiny_ViSEAGO.R')
    create_go_volcano(session, input, output, db_path)
    
    db_table_to_Excel("go_volcano_data",  "go volcano data", stringr::str_c(get_param('string_path', db_path), input$go_volcano_filename), db_path)
    
    cat(file = stderr(), "start_go_volcano clicked...end" , "\n\n\n")
  })  
  
  #-------------------------------------------------------------------------------------------------------------  
  # observeEvent(input$go_volcano_excel, { 
  #   cat(file = stderr(), "go_volcano_excel clicked..." , "\n")
  #   
  #   db_table_to_Excel("go_volcano_data",  "go volcano data", stringr::str_c(params$string_path, input$go_volcano_filename), db_path)
  #   
  #   cat(file = stderr(), "go_volcano_excel clicked...end" , "\n")
  # })
  
  #-------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$get_string, {
    cat(file = stderr(), "get_string clicked...", "\n")
    
    source('Shiny_String.R')
    run_string(session, input, output, db_path)
    
    cat(file = stderr(), "get_string clicked...end", "\n\n\n")
  }) 
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$get_string_enrich, {
    cat(file = stderr(), "get_string_enrich clicked...", "\n")
    
    source('Shiny_String.R')
    run_string_enrich(session, input, output, db_path)
    
    db_table_to_Excel("string_enrichment",  "string_enrich", stringr::str_c(get_param('string_path', db_path), input$string_enrich_data_filename), db_path)
    
    cat(file = stderr(), "get_string_enrich clicked...end", "\n\n\n")
  }) 
  
  #-------------------------------------------------------------------------------------------------------------  

  # observeEvent(input$string_enrich_data_save, { 
  #   cat(file = stderr(), "string_enrich_data_save clicked..." , "\n")
  #   
  #   db_table_to_Excel("string_enrichment",  "string_enrich", stringr::str_c(params$string_path, input$string_enrich_data_filename), db_path)
  #   
  #   cat(file = stderr(), "string_enrich_data_save clicked...end" , "\n")
  # })
  
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$parse_fasta, { 
    cat(file = stderr(), "parse_fasta clicked..." , "\n")
    
    source("Shiny_MotifX.R")
    create_phos_database(session, input, output, db_path)
    
    cat(file = stderr(), "parse_fasta clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$parse_fasta_customer, { 
    cat(file = stderr(), "parse_fasta_customer clicked..." , "\n")
    
    source("Shiny_MotifX.R")
    create_phos_database(session, input, output, db_path)
    
    cat(file = stderr(), "parse_fasta_customer clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$motif_show, {
    cat(file = stderr(), "motif_show clicked..." , "\n")
    
    source("Shiny_MotifX.R")
    run_motifx(session, input, output, db_path)
    
    cat(file = stderr(), "motif_show clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$motif_data_save, { 
    cat(file = stderr(), "motif_data_save clicked..." , "\n")
    
    motif_data_save_excel(session, input, output, db_path)
    
    cat(file = stderr(), "motif_data_save clicked...end" , "\n\n\n")
  })
  
  #-------------------------------------------------------------------------------------------------------------
  observeEvent(input$momo_create, {
    cat(file = stderr(), "momo_create clicked..." , "\n")
    
    source("Shiny_MoMo.R")
    run_momo(session, input, output, db_path)
    
    cat(file = stderr(), "momo_create clicked...end" , "\n\n\n")
  })
  
  
  
  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$archive_data, { 
    cat(file = stderr(), "archive_data clicked..." , "\n")
    
    zip_data_save(session, input, output, db_path)
    
    cat(file = stderr(), "archive_data clicked...end" , "\n\n\n")
  })

  #-------------------------------------------------------------------------------------------------------------  
  observeEvent(input$clean_slate, { 
    cat(file = stderr(), "clean_slate clicked..." , "\n\n\n")
    
    #clear memory
    rm(list = ls())
    gc()
    .rs.restartR()
    session$reload() 
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

 
  # This code will be run after the client has disconnected
  session$onSessionEnded(function() {
    if (site_user != "dpmsr") {
      cat(file = stderr(), "Running session end...", "\n")
      cat(file = stderr(), str_c("Database dir ", database_dir,  " exists? ", dir.exists(database_dir)), "\n")
      
      temp_files <- list.files(params$database_dir)
      if (length(temp_files) == 0) {
        temp_files <- ""
      }
      
      cat(file = stderr(), stringr::str_c("files in database_dir:  ", temp_files), "\n")
      
      #do.call(file.remove, list(list.files(database_dir, full.names = TRUE)))
      #dir_delete(database_dir)
      #system(str_c("rm -R ", database_dir))
      #clear_memory()
      #stopApp()
      #file_touch("restart.txt", access_time = Sys.time(), modification_time = Sys.time())
    }
  })
 
     
})

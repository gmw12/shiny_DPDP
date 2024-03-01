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
  
  source("Shiny_Source.R")
  hide_enable(session, input, output)
  
  #app start conditions
  source('Shiny_UI_Update.R')
  app_startup(session, input, output)

  #set file choosers
  set_file_choosers(session, input, output)
  
  #update UI
  ui_render_load_design(session, input, output)
  if (params$data_path != "")  {create_design_table(session, input, output)}
  
  
  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_design_file, {
    
    cat(file = stderr(), "\n\n", "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_design_file)) {
      
      showModal(modalDialog("Loading design file, creating database...", footer = NULL))
      
      #load design from excel, create database
      load_design_file(session, input, output)

      #set file locations
      file_set()
      
      #update UI
      ui_render_load_design(session, input, output)
      
      #create design table, save to database
      create_design_table(session, input, output)
      
      #backup design table
      design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
      save_data(design_sbf$datapath)
      
      removeModal()
    }

  })
  

  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_data_file, {
    
    cat(file = stderr(), "\n\n","sfb_data_file button clicked...", "\n")
    
    if (is.list(input$sfb_data_file)) {
      showModal(modalDialog("Loading data...", footer = NULL))
      
      #read data files
      load_data_file(session, input, output)
      output$data_file_name <- renderText({get_data_file_name()})
      
      #update UI
      ui_render_load_data(session, input, output)
      ui_render_load_design(session, input, output)
      
      removeModal()
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
   
   

 })

  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$noise_inflection_apply, {
    cat(file = stderr(), "\n", "noise inflection apply clicked", "\n")  
    
    #calculate noise inflection and create plot
    noise_inflection(session, input, output, params)
    render_noise_graphs(session, input, output)
    
    cat(file = stderr(), "\n", "noise inflection apply...end", "\n")  
  })
  
  
  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$noise_apply, {
    cat(file = stderr(), "\n", "noise apply clicked", "\n")  
    
    #save noise inputs to params file
    noise_widget_save(session, input, output)
    
    #remove noise from df and save new df
    noise_remove(session, input, output, params)
      
    cat(file = stderr(), "\n", "noise apply...end", "\n")
    
  })
  
  
  #------------------------------------------------------------------------------------------------------  
  
  observeEvent(input$filter_cutoff, {
    cat(file = stderr(), "\n", "filter cutoff clicked", "\n")  
    
    #save filter inputs to params file
    filter_widget_save(session, input, output)
    
    #create histogram and calculate cutoff values
    filter_histogram_plot(sesion, input, output, params)
    
    cat(file = stderr(), "\n", "filter cutoff...end", "\n")  
  })
    
#------------------------------------------------------------------------------------------------------  
 
 observeEvent(input$filter_apply, {
   cat(file = stderr(), "\n", "filter apply clicked", "\n")

   showModal(modalDialog("Applying data filters...", footer = NULL))
   
   #save filter inputs to params file
   filter_widget_save(session, input, output)
   
   #apply data filters, save to new dataframe
   filter_data(session, input, output)
   
   #create plots
   filter_create_plots(sesion, input, output, params)
   
   #gather meta data for filtered dataframe
   meta_data("filter")
   
   removeModal()
 })
 
  
#------------------------------------------------------------------------------------------------------   

 observeEvent(input$norm_parameters, {
   cat(file = stderr(), "\n", "norm parameters clicked", "\n")
   showModal(modalDialog("Setting normalization parameters...", footer = NULL))
   
   norm_widget_save(session, input, output)
   
   norm_filter()
   
   render_norm_graphs(session, input, output)

   removeModal()
   
 })
 
#------------------------------------------------------------------------------------------------------  
 
 observeEvent(input$norm_apply, {
   cat(file = stderr(), "\n", "norm apply clicked", "\n")
   showModal(modalDialog("Normalizing data...", footer = NULL))
   
   norm_apply_widget_save(session, input, output)
   
   norm_apply()
   
   render_norm_apply_graphs(session, input, output)
   
   removeModal()
 })

   
#------------------------------------------------------------------------------------------------------   
  
 observeEvent(input$impute_parameters, {
   cat(file = stderr(), "\n", "impute parameters clicked", "\n")

   #save inputs
   impute_apply_widget_save(session, input, output)
   
   #gather info on raw data for ui, create random table for imputing
   impute_meta_data()
   
   #render text
   #render_impute_parameters(session, input, output) 
   
   #render plots
   create_impute_table(session, input, output)
   impute_create_plots(session, input, output, params)
   render_impute_graphs(session, input, output)
   
 })
 
  #------------------------------------------------------------------------------------------------------   
  
  observeEvent(input$impute_apply, {
    cat(file = stderr(), "\n", "impute_apply clicked", "\n")
    
    impute_apply(session, input, output)
    
  })
 
  #------------------------------------------------------------------------------------------------------   
  
  observeEvent(input$rollup_apply, {
    cat(file = stderr(), "\n", "rollup_apply clicked", "\n")
    
    #save parameters
    rollup_widget_save(session, input, output) 
    
    #start rollup
    rollup_apply(session, input, output)
    
  }) 
 

 
  
  
  
  
  
  
 
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

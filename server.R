app_version <<- '2023.12.01'

options(shiny.maxRequestSize = 4000*1024^2)

cat(file = stderr(), "server.R started", "\n")


shinyServer(function(session, input, output) {
  
  #app start conditions
  app_startup(session, input, output)
  
  source("Shiny_Source.R")
  hide_enable(session, input, output)
  
  source("Shiny_Reactive.R", local = TRUE)
  
  
  
  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_design_file, {
    
    cat(file = stderr(), "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_design_file)) {
      
      showModal(modalDialog("Loading design file, creating database...", footer = NULL))
      
      #load design from excel, create database
      load_design_file(session, input, output)
  
      #save paramater table to database
      param_save()
      
      #set file locations
      file_set()
  
      #create design table
      create_design_table(session, input, output)
      
      #backup design table
      design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
      save_data(design_sbf$datapath)
      
      output$design_file_name <- renderText({get_design_file_name()})
      removeModal()
    }

  })
  

  
  #------------------------------------------------------------------------------------------------------  
  #Load data file
  observeEvent(input$sfb_data_file, {
    
    cat(file = stderr(), "sfb_data_file button clicked...", "\n")
    
    if (is.list(input$sfb_data_file)) {
      showModal(modalDialog("Loading data...", footer = NULL))
      load_data_file(session, input, output)
      output$data_file_name <- renderText({get_data_file_name()})
      render_data_info(session, input, output)
      removeModal()
    }

  }) 

   

 observeEvent(input$accept_parameters, {
   cat(file = stderr(), "accept parameters clicked", "\n")
   set_sample_groups(session, input, output)
   param_refresh()
 })
  

  
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

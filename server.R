app_version <<- '2023.11.15'

options(shiny.maxRequestSize = 4000*1024^2)

cat(file = stderr(), "server.R started", "\n")

#set user
set_user()


shinyServer(function(session, input, output) {
  
  #app start conditions
  app_startup(input, output, session)
  
  source("Shiny_Reactive.R", local = TRUE)
  source("Shiny_Render.R", local = TRUE)
  

  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_design_file, {
    
    cat(file = stderr(), "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_design_file)) {
      
      showModal(modalDialog("Loading design file, creating database...", footer = NULL))
      
      #load design from excel, create database
      load_design_file(session, input, output)
  
      #create paramater table
      create_parameter_table(session, input, output)
      
      #set file locations
      file_set()
  
      #create design table
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
    
    cat(file = stderr(), "sfb_data_file button clicked...", "\n")
    
    if (is.list(input$sfb_data_file)) {
      showModal(modalDialog("Loading data...", footer = NULL))
      load_data_file(session, input, output)
      removeModal()
    }

  }) 
  
  
    
})

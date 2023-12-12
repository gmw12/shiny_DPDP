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
   parameter_widget_save(session, input, output)
   set_sample_groups(session, input, output)
   param_refresh()
   bg_bar <- callr::r_bg(func = bar_plot, args = list("raw_precursor", "Raw_Precursor", params$qc_path, params), stderr = "error.txt", supervise = TRUE)
   bg_box <- callr::r_bg(func = box_plot, args = list("raw_precursor", "Raw_Precursor", params$qc_path, params), stderr = "error2.txt", supervise = TRUE)
   bg_meta <- callr::r_bg(func = raw_meta, args = list("raw_precursor", params), stderr = "error3.txt", supervise = TRUE)
   bg_bar$wait
   bg_box$wait
   bg_meta$wait
   param_refresh()
   
   wait_cycle <- 0
   while (!file.exists(str_c(params$qc_path,"Raw_Precursor_barplot.png"))) {
     if (wait_cycle < 10) {
       Sys.sleep(0.5)
       wait_cycle <- wait_cycle + 1
      }
     }
   
   render_parameters_graphs(session, input, output)
   
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

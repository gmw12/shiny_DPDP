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
  cat(file = stderr(), "Shiny Server started ...1", "\n")
  
  #app start conditions
  source('Shiny_UI_Update.R')
  app_startup(session, input, output)
  
  source("Shiny_Source.R")
  hide_enable(session, input, output)
  
  #set file choosers
  set_file_choosers(session, input, output)
  
  
  #------------------------------------------------------------------------------------------------------  
  #Load design file
  observeEvent(input$sfb_design_file, {
    
    cat(file = stderr(), "sfb_design_file button clicked...", "\n")
    
    if (is.list(input$sfb_design_file)) {
      
      showModal(modalDialog("Loading design file, creating database...", footer = NULL))
      
      #load design from excel, create database
      load_design_file(session, input, output)
  
      #save paramater table to database
      param_save_to_database()
      
      #set file locations
      file_set()
      
      #update UI
      ui_render_load_design(session, input, output)
      
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
      output$data_file_name <- renderText({get_data_file_name()})
      
      #update UI
      ui_render_load_data(session, input, output)
      
      removeModal()
    }

  }) 

   

 observeEvent(input$accept_parameters, {
   cat(file = stderr(), "accept parameters clicked", "\n")
   showModal(modalDialog("Applying parameters...", footer = NULL))
   
   parameter_widget_save(session, input, output)
   set_sample_groups(session, input, output, params)
   params <<- param_load_from_database()

   bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_raw", "Precursor_Raw", params$qc_path, params), stderr = "error_barplot.txt", supervise = TRUE)
   bg_box <- callr::r_bg(func = box_plot, args = list("precursor_raw", "Precursor_Raw", params$qc_path, params), stderr = "error_boxplot.txt", supervise = TRUE)
   bg_box$wait()
   bg_bar$wait()
   cat(file = stderr(), readLines("error_barplot.txt"), "\n")
   cat(file = stderr(), readLines("error_boxplot.txt"), "\n")
   

   bg_meta <- callr::r_bg(func = meta_data, args = list("precursor_raw", "raw", params), stderr = "error_rawmeta.txt", supervise = TRUE)
   bg_meta$wait()
   cat(file = stderr(), readLines("error_rawmeta.txt"), "\n")
   
   params <<- param_load_from_database()

   wait_cycle <- 0
   while (!file.exists(str_c(params$qc_path,"Precursor_Raw_barplot.png"))) {
     if (wait_cycle < 10) {
       Sys.sleep(0.5)
       wait_cycle <- wait_cycle + 1
      }
     }

   ui_render_parameters(session, input, output)
   
   # Organize raw data into selected columns and info column names
   removeModal()
   showModal(modalDialog("Prepare Data...", footer = NULL))
   prepare_data(session, input, output)
   
   # order columns and rename sample column names
   removeModal()
   showModal(modalDialog("Order and rename Data...", footer = NULL))
   order_rename_columns()
   removeModal()
   
 })
  
 
 
 observeEvent(input$filter_apply, {
   cat(file = stderr(), "filter apply clicked", "\n")

   showModal(modalDialog("Applying data filters...", footer = NULL))
   
   filter_data(session, input, output)
   filter_widget_save(session, input, output)
   
   bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_filter", "Precursor_Filter", params$qc_path, params), stderr = "error_filterbarplot.txt", supervise = TRUE)
   bg_box <- callr::r_bg(func = box_plot, args = list("precursor_filter", "Precursor_Filter", params$qc_path, params), stderr = "error_filterboxplot.txt", supervise = TRUE)
   bg_box$wait()
   bg_bar$wait()
   cat(file = stderr(), readLines("error_filterbarplot.txt"), "\n")
   cat(file = stderr(), readLines("error_filterboxplot.txt"), "\n")
   
   bg_meta <- callr::r_bg(func = meta_data, args = list("precursor_filter", "filter", params), stderr = "error_filtermeta.txt", supervise = TRUE)
   bg_meta$wait()
   cat(file = stderr(), readLines("error_filtermeta.txt"), "\n")
   
   params <<- param_load_from_database()
   
   wait_cycle <- 0
   while (!file.exists(str_c(params$qc_path,"Precursor_Raw_barplot.png"))) {
     if (wait_cycle < 10) {
       Sys.sleep(0.5)
       wait_cycle <- wait_cycle + 1
     }
   }
   
   ui_render_filter(session, input, output)
   
   removeModal()
 })
 
  
 
 
 
 observeEvent(input$norm_parameters, {
   cat(file = stderr(), "norm parameters clicked", "\n")
   showModal(modalDialog("Setting normalization parameters...", footer = NULL))
   
   norm_widget_save(session, input, output)
   
   norm_filter()
   
   render_norm_graphs(session, input, output)

   removeModal()
   
 })
 
 
 
 observeEvent(input$norm_apply, {
   cat(file = stderr(), "norm apply clicked", "\n")
   showModal(modalDialog("Normalizing data...", footer = NULL))
   
   norm_apply_widget_save(session, input, output)
   
   norm_apply()
   
   render_norm_apply_graphs(session, input, output)
   
   removeModal()
 })
 
 
 observeEvent(input$impute_parameters, {
   cat(file = stderr(), "impute apply clicked", "\n")
   showModal(modalDialog("Setting imputation parameters, creating histogram...", footer = NULL))

   impute_apply_widget_save(session, input, output)
   
   bg_impute <- callr::r_bg(func = histogram_plot, args = list("precursor_filter", "Precursor_Filtered_Histogram", params), stderr = "error_impute.txt", supervise = TRUE)
   bg_impute$wait()
   
   cat(file = stderr(), readLines("error_impute.txt"), "\n")
   
   render_impute_parameter_graphs(session, input, output)
   
   params <<- param_load_from_database()
   
   removeModal()
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

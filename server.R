app_version <<- '2023.12.01'

options(shiny.maxRequestSize = 4000*1024^2)

cat(file = stderr(), "server.R started", "\n")

#set user
set_user()


shinyServer(function(session, input, output) {
  
  #app start conditions
  app_startup(session, input, output)
  
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
      params <<- create_parameter_table(session, input, output)
      
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
  
 output$parameter_box <- renderUI({
  box(title = "Set analysis parameters...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 4, height = 800,
      selectInput("primary_group", label = "Full or primary group for filter and impute", 
                  choices = list("Full", "Primary"), selected = params$primary_group, width = 300),
      selectInput("data_output", label = ("Select Output Data Type"),
                   choices = list("Protein", "Peptide"),
                   selected = params$data_output, width = 300),
      checkboxInput("norm_ptm", label = "Normalize on PTM?", value = params$norm_ptm, width = 300),
      textInput("norm_ptm_grep", label = "Normalize PTM grep", value = params$norm_ptm_grep, width = 300),
      checkboxInput("impute_ptm", label = "Impute Distribution based on PTM?", value = params$input_ptm, width = 300),
      textInput("impute_ptm_grep", label = "Impute PTM grep", value = params$input_ptm_grep, width = 300),
      selectInput("peptide_select", label = h5("Peptides to Use"), 
                  choices = list("Razor", "Unique", "Shared"), 
                  selected = "Razor"),
      checkboxInput("multi_tmt", label = "SPQC Normalized TMT sets"),
      
      if (1 == 2) {
      checkboxInput("use_isoform", label = "Use Peptide Isoform?")
      },
      
      actionButton("save_parameters", label = "Save Parameters",
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton("load_parameters", label = "Load Parameters",
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
 })
  
  
 observeEvent(input$save_parameters, {
   cat(file = stderr(), "save parameters clicked", "\n")
   save_parameters(session, input, output)
 })
  
 observeEvent(input$load_parameters, {
   cat(file = stderr(), "load parameters clicked", "\n")
   load_parameters(session, input, output)
 })
  

    
})

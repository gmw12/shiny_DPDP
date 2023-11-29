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
  
 output$box_test <- renderUI({
  box(title = "Parameters", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 4, height = 800,
      tags$h3("Set analysis parameters..."),
      br(),
      checkboxInput("primary_group", label = "Use only primary group for filter and impute", value = FALSE),
      br(),
      radioButtons("data_output", label = h3("Select Output Data Type"),
                   choices = list("Protein" = 1, "Peptide" = 2),
                   selected = 2),
      checkboxInput("checkbox_norm_ptm", label = "Normalize on PTM?"),
      textInput("peptide_norm_grep", label = "Normalize PTM grep", value = "Enter value here"),
      checkboxInput("checkbox_impute_ptm", label = "Impute Distribution based on PTM?"),
      textInput("peptide_impute_grep", label = "Impute PTM grep", value = "Enter value here"),
      selectInput("razor", label = h5("Peptides to Use"), 
                  choices = list("Razor", "Unique", "Shared"), 
                  selected = "Razor"),
      checkboxInput("checkbox_tmt", label = "SPQC Normalized TMT sets"),
      if(1==2){
      checkboxInput("checkbox_isoform", label = "Use Peptide Isoform?")
      }
  )
 })
  
  
  
  
  
  
  
  
  
  
  
   
    
})

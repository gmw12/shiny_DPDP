cat(file = stderr(), "ui.R started", "\n")

source("Shiny_Libraries.R")
source("Shiny_Setup.R")
source("Shiny_Startup.R")

if (!exists('params')) {
  cat(file = stderr(), "params file does not exist...", "\n")
  create_default_params() 
  
  #set user
  set_user()
  
  }


  sidebar <- dashboardSidebar(width = 165,
    useShinyjs(),
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome"),
      menuItem("Load", tabName = "load"),
      menuItem("Parameters", tabName = "parameters"),
      menuItem("Filter", tabName = "filter"),
      menuItem("Normalize", tabName = "normalize"),
      menuItem("Impute"),
      menuItem("Rollup"),
      menuItem("Stats"),
      menuItem("Save")
    )
  )

  body <- dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "welcome",
        fluidRow(
          column(width = 12, align = "center",
            br(),
            br(),
            br(),
            br(),
            h1("Welcome to Duke Proteomics Data Processing", style = "font-size:60px"),
            br(),
            img(src = 'astral.png', align = "center", width = 400, height = 400 )
            )
          )
        ),
      
      # Design 
      tabItem(tabName = "load",
        fluidRow(
          column(width = 4,
            fluidRow(
              box(title = "Current State", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 150,
                  span(textOutput("data_source"), style = "color:blue; font-size:16px"),
                  span(textOutput("data_input"), style = "color:blue; font-size:16px"),
                  span(textOutput("data_table_format"), style = "color:blue; font-size:16px"),
                  span(textOutput("data_ptm"), style = "color:blue; font-size:16px")
              ),
              box(title = "Start from Scratch", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 375,
                tags$h4("1. Enter file prefix for data output"),
                fluidRow(align = "center", textInput("file_prefix", label = "", width = 300, value = "project_date")),
               
                tags$h4("2. Select and Load the study design file..."),
             
                fluidRow(align = "center", shinyFilesButton('sfb_design_file', label = 'Load Design File', title = 'Please select excel design file', multiple = FALSE,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; display:center")),
               
                span(textOutput("design_file_name"), style = "color:blue; font-size:16px"),
            
                tags$h4("3. Select data file(s).."),
        
                fluidRow(align = "center", shinyFilesButton('sfb_data_file', label = 'Select Data File(s)', title = 'Please select data file(s)', multiple = TRUE,
                                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                span(textOutput("data_file_name"), style = "color:blue; font-size:16px")
              ),
              
              box(title = "Start from Previous Analysis", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 12, height = 200,
                  tags$h3("Select database file"),
                  fluidRow(align = "center", shinyFilesButton('sfb_database_file', label = 'Select Database File', title = 'Please select database file', multiple = FALSE,
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
              ),
              
              
              
             )),
            
          
          column(width = 8,  
            box(title = "Study Design Table", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 725,
                DT::dataTableOutput("stats_design_table")
                #div(style = 'overflow-x: scroll; overflow-y: scroll;', DT::dataTableOutput("stats_design_table")) #), width = '100%'))
                
                ))
        )
      ),
      
      #Parameters
      tabItem(tabName = "parameters",
          fluidRow(
            column(width = 3,
            fluidRow(
              box(id = "param_box", title = "Set analysis parameters...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,

                selectInput("primary_group", label = "Full or primary group for filter and impute",
                                choices = list("Full", "Primary"), selected = params$primary_group, width = 300),
                selectInput("data_output", label = ("Select Output Data Type"),
                                choices = list("Protein", "Peptide"),
                                selected = params$data_output, width = 300),

                checkboxInput("ptm", label = "PTM Analysis?", value = params$ptm, width = 300),
                  
                checkboxInput("multi_tmt", label = "SPQC Normalized TMT sets"),
                  
                selectInput("peptide_select", label = h5("Peptides to Use"),
                              choices = list("Razor", "Unique", "Shared"),
                              selected = "Razor"),

                checkboxInput("use_isoform", label = "Use Peptide Isoform?"),

                fluidRow(align = "center", actionButton("accept_parameters", label = "Accept Parameters",
                             style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                ))),
        
        column(width = 9,  
            fluidRow(
               box(title = "Raw Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 625,
                   fluidRow(
                      column(width = 6, imageOutput("raw_bar")),
                      column(width = 6, imageOutput("raw_box"))
                   )
               )),
            fluidRow(
                box(title = "Raw Meta Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 100,
                    fluidRow(
                      column(width = 4, span(textOutput("meta_precursor_raw"), style = "color:blue; font-size:16px")),
                      column(width = 4, span(textOutput("meta_peptide_raw"), style = "color:blue; font-size:16px")),
                      column(width = 4, span(textOutput("meta_protein_raw"), style = "color:blue; font-size:16px"))
                    )
                ))
              )
          )
        
      ),
      
      #Filter
      tabItem(tabName = "filter",
              fluidRow(
                column(width = 3,
                       fluidRow(
                         box(id = "filter_box", title = "Set filter parameters...", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 750,
                             
                             numericInput("filter_min_measured_all", label = "Enter minimum # measured values (all samples)", value = 2),
                             hr(),
                             checkboxInput("filter_x_percent", label = "Require X% measured values in at least one group"),
                             numericInput("filter_x_percent_value", label = "Enter X% measured values (decimal)", value = 0.8),
                             hr(),
                             checkboxInput("filter_cv", label = "Filter on Specific Group CV"),
                             selectInput("filter_cv_group", label = "Enter group for CV filter", 
                                         choices = list("SPQC"), selected = "SPQC"),
                             numericInput("filter_cv_value", label = "Enter CV% for cutoff", value = "Enter value here"),
                             hr(),
                             br(),
                             
                             fluidRow(align = "center", actionButton("filter_apply", label = "Apply Filters",
                                                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                         ))),
                
                column(width = 9,  
                       fluidRow(
                         box(title = "Filter Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 625,
                             fluidRow(
                               column(width = 6, imageOutput("filter_bar")),
                               column(width = 6, imageOutput("filter_box"))
                             )
                         )),
                       fluidRow(
                         box(title = "Filter Meta Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 100,
                             fluidRow(
                               column(width = 4, span(textOutput("meta_precursor_filter"), style = "color:blue; font-size:16px")),
                               column(width = 4, span(textOutput("meta_peptide_filter"), style = "color:blue; font-size:16px")),
                               column(width = 4, span(textOutput("meta_protein_filter"), style = "color:blue; font-size:16px"))
                             )
                         ))
                )
              )
      ),
      
      
      #Normalize
      tabItem(tabName = "normalize",
              fluidRow(
                column(width = 8,
                  box(id = "norm_param_box", title = "Set normalization parameters...", status = "primary",
                          solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 230,
                      fluidRow(
                        column(width = 4,
                          checkboxInput("norm_include", label = "Include only grep for norm (trypsin|keratin)"),
                          textInput("include_norm_grep", label = "Filter Include grep", value = "trypsin|keratin"),
                        ),
                        column(width = 4,
                          checkboxInput("norm_exclude", label = "Exclude grep from norm (trypsin|keratin)"),
                          textInput("exclude_norm_grep", label = "Filter Exclude grep", value = "trypsin|keratin"),
                        ),
                        column(width = 4,
                          checkboxInput("norm_ptm", label = "Normalize on PTM?", value = params$norm_ptm, width = 300),
                          textInput("norm_ptm_grep", label = "Normalize PTM grep", value = params$norm_ptm_grep, width = 300),
                          #checkboxInput("impute_ptm", label = "Impute Distribution based on PTM?", value = params$input_ptm, width = 300),
                          #textInput("impute_ptm_grep", label = "Impute PTM grep", value = params$input_ptm_grep, width = 300),
                        ),
                       
                       fluidRow(align = "center", actionButton("norm_parameters", label = "Apply Normalizaton Parameters",
                                                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       
                       )
                      ) 
                    ),
                  column(width = 4,
                    box(id = "norm_box", title = "Normalization strategy...", status = "primary",
                          solidHeader = TRUE, collapsible = FALSE, align = "left", width = 12, height = 230, 
                      selectInput("norm_type", label = "Select normalization strategy",
                                         choices = list("Sample Loading - Total" = "sl",
                                                        "Trimmed Mean" = "tmm",
                                                        "Samle Loading Trimmed Mean" = "sltmm",
                                                        "Protein" = "protein",
                                                        "DirectLFQ" = "directlfq",
                                                        "Quantile" = "quantile",
                                                        "Linear Regression" = "lg",
                                                        "LOESS" = "loess",
                                                        "VSN" = "vsn",
                                                        "Median of Total Intensity" = "mti",
                                                        "Median Intensity" = "mi",
                                                        "Average Intensity" = "ai"), selected = "SLTMM"),
                      textInput("protein_norm_grep", label = "Protein Acession", value = ""),
                      fluidRow(align = "center", actionButton("norm_apply", label = "Apply Normalization",
                                                                     style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                         )
                    )
                
              ),
              
              fluidRow(
                box(title = "Normalized Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 500,
                    fluidRow(
                      column(width = 4, imageOutput("norm_data_start_bar")),
                      column(width = 4, imageOutput("norm_normdata_bar")),
                      column(width = 4, imageOutput("norm_bar"))
                    )
                )
              )
      )     
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    )
  )
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  dashboardPage(
    dashboardHeader(title = "Duke Proteomics Data Processing", titleWidth = 325),
    sidebar,
    body
  )
  
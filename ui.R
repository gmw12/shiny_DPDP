library(shinydashboard)

source("Shiny_Libraries.R")
source("Shiny_Source.R")
useShinyjs()

  sidebar <- dashboardSidebar(width = 165,
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome"),
      menuItem("Design", tabName = "design"),
      menuItem("Data", tabName = "data"),
      menuItem("Overview"),
      menuItem("Filter"),
      menuItem("Normalize"),
      menuItem("Impute"),
      menuItem("Rollup"),
      menuItem("Stats"),
      menuItem("Save")
    )
  )

  body <- dashboardBody(
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
      tabItem(tabName = "design",
        fluidRow(
          column(width = 4,
            fluidRow(
              box(title = "Create New Study Design", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 12, height = 350,
                tags$h3("Enter file prefix for data output"),
                textInput("file_prefix", label = "", width = 300, value = "project_date"),
                br(),
                tags$h3("Choose and Load the study design file..."),
                br(),
                shinyFilesButton('sfb_design_file', label = 'Load Design File', title = 'Please select excel design file', multiple = FALSE,
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                br(),
                span(textOutput("design_file_name"), style = "color:blue; font-size:16px"),
                br()
                )),
            
            fluidRow(
              box(title = "Load Database", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 12, height = 250,
                  tags$h3("Choose database file.."),
                  br(),
                  shinyFilesButton('sfb_database_file', label = 'Load Database File', title = 'Please select database file', multiple = FALSE,
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  )
            )
            ),
          
          column(width = 8,  
            box(title = "Study Design Table", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = 700,
                DT::dataTableOutput("stats_design_table")
                #div(style = 'overflow-x: scroll; overflow-y: scroll;', DT::dataTableOutput("stats_design_table")) #), width = '100%'))
                
                ))
        )
      ),
      
      
      #Data
      tabItem(tabName = "data",
              hr(),
              box(title = "Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, align = "center", width = 4, height = 300,
                  tags$h3("Choose and Load the data file(s)..."),
                  br(),
                  shinyFilesButton('sfb_data_file', label = 'Choose Data File(s)', title = 'Please select data file(s)', multiple = TRUE,
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  span(textOutput("data_file_name"), style = "color:blue; font-size:16px"),
              )
      )      
      
      
      
      
      
      
    )
  )
    
  
  dashboardPage(
    dashboardHeader(title = "Duke Proteomics Data Processing", titleWidth = 350),
    sidebar,
    body
  )
  
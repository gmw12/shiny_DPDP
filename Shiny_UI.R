cat(file = stderr(), "Shiny_UI.R", "\n")

#-------------------------------------------------------------------------

set_comp_names <- function(session, input, output) {
  output$comp1_text <- renderText({ "Comparison 1" })
  output$comp2_text <- renderText({ "Comparison 2" })
  output$comp3_text <- renderText({ "Comparison 3" })
  output$comp4_text <- renderText({ "Comparison 4" })
  output$comp5_text <- renderText({ "Comparison 5" })
  output$comp6_text <- renderText({ "Comparison 6" })
  output$comp7_text <- renderText({ "Comparison 7" })
  output$comp8_text <- renderText({ "Comparison 8" })
  output$comp9_text <- renderText({ "Comparison 9" })
}

#-------------------------------------------------------------------------

create_comp <- function(i) {
  fluidRow(
    column(width = 2,
           span(textOutput(str_c("comp", i, "_text")), style = "color:blue; font-size:20px"),
    ),
    column(width = 3,
           pickerInput(inputId = str_c("comp_", i, "N"), label = "Numerator",  choices = "None", 
                       options = list(`actions-box` = TRUE,size = 10,
                                      `selected-text-format` = "count > 7"),  multiple = TRUE)
    ),
    column(width = 3,
           pickerInput(inputId = str_c("comp_", i, "D"), label = "Denominator",  choices = "None", 
                       options = list(`actions-box` = TRUE,size = 10,
                                      `selected-text-format` = "count > 7"),  multiple = TRUE)
    ),
    column(width = 3,
           textInput(str_c("comp", i, "_name"), label = "Description", value = "")
    )
  )
}




#-------------------------------------------------------------------------
create_stats_bar_ui <- function(plot_number) {
  fluidRow(
    column(width = 6, offset = 0,
           dropdownButton(
             textInput(str_c(plot_number, "_stats_barplot_y_axis_label"), label = "y axis label", value = "Intensity", width = 200),
             textInput(str_c(plot_number, "_stats_barplot_title"), label = "plot title", value = "Total Summed Intensity", width = 200),
             sliderInput(str_c(plot_number, "_stats_barplot_label_size"), label = h5("Label Size"), min = 1, 
                         max = 50, value = 11),
             sliderInput(str_c(plot_number, "_stats_barplot_title_size"), label = h5("Title Size"), min = 10, 
                         max = 50, value = 20),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           ),
           div(
             style = "position:relative",
             plotOutput(str_c(plot_number, "_stats_barplot"), width = 800, height = 550)
           ),
           downloadButton(str_c(plot_number, '_download_stats_barplot'))
    )  
  )  
  
}

#-------------------------------------------------------------------------
create_stats_box_ui <- function(plot_number) {
  fluidRow(
    column(width = 6, offset = 0,
           dropdownButton(
             textInput(str_c(plot_number, "_stats_boxplot_y_axis_label"), label = "y axis label", value = "Intensity", width = 200),
             textInput(str_c(plot_number, "_stats_boxplot_title"), label = "plot title", value = "Total Summed Intensity", width = 200),
             sliderInput(str_c(plot_number, "_stats_boxplot_label_size"), label = h5("Label Size"), min = 1, 
                         max = 50, value = 11),
             sliderInput(str_c(plot_number, "_stats_boxplot_title_size"), label = h5("Title Size"), min = 10, 
                         max = 50, value = 20),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           ),
           div(
             style = "position:relative",
             plotOutput(str_c(plot_number, "_stats_boxplot"), width = 800, height = 550)
           ),
           downloadButton(str_c(plot_number, '_download_stats_boxplot'))
    )  
  )  
  
}

#-------------------------------------------------------------------------
create_stats_pca2d_ui <- function(plot_number) {
  fluidRow(
    column(width = 6, offset = 0,
           dropdownButton(
             selectInput(str_c(plot_number, "_stats_pca2d_x"), label = "pca xaxis", choices = list("PC1", "PC2", "PC3", "PC4", "PC5"), 
                         selected = "PC1"),
             selectInput(str_c(plot_number, "_stats_pca2d_y"), label = "pca yaxis", choices = list("PC1", "PC2", "PC3", "PC4", "PC5"), 
                         selected = "PC2"),
             textInput(str_c(plot_number, "_stats_pca2d_title"), label = "plot title", value = "pca2d", width = 200),
             sliderInput(str_c(plot_number, "_stats_pca2d_label_size"), label = h5("Label Size"), min = 1, 
                         max = 50, value = 11),
             sliderInput(str_c(plot_number, "_stats_pca2d_title_size"), label = h5("Title Size"), min = 10, 
                         max = 50, value = 20),
             sliderInput(str_c(plot_number, "_stats_pca2d_dot_size"), label = h5("Point Size"), min = 1, 
                         max = 20, value = 4),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           ),
           div(
             style = "position:relative",
             plotOutput(str_c(plot_number, "_stats_pca2d"), width = 800, height = 550,
                        hover = hoverOpts("plot_pca2d_hover", delay = 100, delayType = "debounce")),
             uiOutput("hover_pca2d_info")
           ),
           downloadButton(str_c(plot_number, '_download_stats_pca2d'))
    )  
  )  
  
}

#-------------------------------------------------------------------------
create_stats_pca3d_ui <- function(plot_number) {
  fluidRow(
    column(width = 6, offset = 0,
           dropdownButton(
             textInput(str_c(plot_number, "_stats_pca3d_title"), label = "plot title", value = "pca3d", width = 200),
             sliderInput(str_c(plot_number, "_stats_pca3d_label_size"), label = h5("Label Size"), min = 1, 
                         max = 50, value = 11),
             sliderInput(str_c(plot_number, "_stats_pca3d_title_size"), label = h5("Title Size"), min = 10, 
                         max = 50, value = 20),
             sliderInput(str_c(plot_number, "_stats_pca3d_dot_size"), label = h5("Point Size"), min = 1, 
                         max = 10, value = 2),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           ),
           div(
             style = "position:relative",
             rglwidgetOutput(str_c(plot_number, "_stats_pca3d"), width = 800, height = 550)
           ),
           downloadButton(str_c(plot_number, '_download_stats_pca3d'))
    )  
  )  
  
}
#-------------------------------------------------------------------------
create_stats_cluster_ui <- function(plot_number) {
  fluidRow(
    column(width = 6, offset = 0,
           dropdownButton(
             textInput(str_c(plot_number, "_stats_cluster_title"), label = "plot title", value = "cluster", width = 200),
             sliderInput(str_c(plot_number, "_stats_cluster_label_size"), label = h5("Label Size"), min = 1, 
                         max = 50, value = 11),
             sliderInput(str_c(plot_number, "_stats_cluster_title_size"), label = h5("Title Size"), min = 10, 
                         max = 50, value = 20),
             colourpicker::colourInput(str_c(plot_number, "_cluster_high_color"), "Select High Color", "#FF3366"),
             colourpicker::colourInput(str_c(plot_number, "_cluster_low_color"), "Select Low Color", "#009933"),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           ),
           div(
             style = "position:relative",
             plotOutput(str_c(plot_number, "_stats_cluster"), width = 800, height = 550)
           ),
           downloadButton(str_c(plot_number, '_download_stats_cluster'))
    )  
  )  
  
}
create_stats_heatmap_ui <- function(plot_number) {
  fluidRow(
    column(width = 6, offset = 0,
           dropdownButton(
             textInput(str_c(plot_number, "_stats_heatmap_title"), label = "plot title", value = "heatmap", width = 200),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           ),
           div(
             style = "position:relative",
             plotOutput(str_c(plot_number, "_stats_heatmap"), width = 800, height = 550)
           ),
           downloadButton(str_c(plot_number, '_download_stats_heatmap'))
    )  
  )  
  
}
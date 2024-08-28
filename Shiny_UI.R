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
             plotOutput(str_c(plot_number, "_stats_barplot"), width = "40vw", height = "60vh")
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
             plotOutput(str_c(plot_number, "_stats_boxplot"), width = "40vw", height = "60vh")
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
             plotOutput(str_c(plot_number, "_stats_pca2d"), width = "40vw", height = "60vh",
                        hover = hoverOpts(str_c(plot_number, "_plot_pca2d_hover"), delay = 100, delayType = "debounce")),
             uiOutput(str_c(plot_number, "_hover_pca2d_info"))
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
             rglwidgetOutput(str_c(plot_number, "_stats_pca3d"), width = "40vw", height = "60vh")
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
             plotOutput(str_c(plot_number, "_stats_cluster"), width = "40vw", height = "60vh")
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
             plotOutput(str_c(plot_number, "_stats_heatmap"), width = "40vw", height = "60vh")
           ),
           downloadButton(str_c(plot_number, '_download_stats_heatmap'))
    )  
  )  
}

#---------------------------------------------------------------------------
create_stats_volcano_ui <- function(plot_number){
  fluidRow(
    column(width = 6, offset = 0,
      uiOutput("create_stats_volcano_ui_1"),  
      uiOutput("create_stats_volcano_ui_2")  
    )
    )

}


#---------------------------------------------------------------------------
create_stats_volcano_ui_part1 <- function(plot_number){
  fluidRow(
    column(width = 3, offset = 0,
           dropdownButton(
             column(width = 6, offset = 0,
               textInput(str_c(plot_number, "_volcano_stats_plot_title"), label = "plot title", 
                         value = "Volcano", width = 200),
               textInput(str_c(plot_number, "_volcano_stats_plot_y_axis_label"), label = "y axis label", value = "-log_pvalue", width = 200),
               textInput(str_c(plot_number, "_volcano_stats_plot_x_axis_label"), label = "x axis label", value = "log_FC", width = 200),
               colourpicker::colourInput(str_c(plot_number, "_volcano_stats_dot_color"), "Select Color", "blue"),
             ),
             column(width = 6, offset = 0 ,
               sliderInput(str_c(plot_number, "_volcano_stats_plot_dot_size"), label = h5("Point Size"), min = 1, 
                           max = 10, value = 2),
               sliderInput(str_c(plot_number, "_volcano_stats_plot_label_size"), label = h5("Label Size"), min = 1, 
                           max = 50, value = 20),
               sliderInput(str_c(plot_number, "_volcano_stats_plot_title_size"), label = h5("Title Size"), min = 10, 
                           max = 50, value = 20)
             ),
               circle = TRUE, status = "danger", icon = icon("cogs"), width = "600px", size = "sm",
               tooltip = tooltipOptions(title = "Click to see inputs !")
             ),
            dropdownButton(
             column(width=6, offset = 0,
               h3('Highlight Proteins'),
               h5('Enter list of strings to search in protein descriptions. Not case sensitive. Beware of trailing spaces.'),
               textInput(str_c(plot_number, "_volcano_highlight"), label = "Highlight search term (description)"),
               colourpicker::colourInput(str_c(plot_number, "_volcano_highlight_color"), "Description Highlight Color", "red"),
               checkboxInput(str_c(plot_number, "_stats_volcano_highlight_up"), label = "Highlight stat signif up?", value = TRUE),
               checkboxInput(str_c(plot_number, "_stats_volcano_highlight_down"), label = "Highlight stat signif down?", value = TRUE),
               colourpicker::colourInput(str_c(plot_number, "_volcano_highlight_color_up"), "Stat 'Down' Color", "red"),
               colourpicker::colourInput(str_c(plot_number, "_volcano_highlight_color_down"), "Stat 'Up' Color", "red"),
             ),
             column(width = 6, offset = 0,

               sliderInput(str_c(plot_number, "_volcano_highlight_dot_size"), label = h5("Point Size"), min = 1, 
                           max = 10, value = 3),
               sliderInput(str_c(plot_number, "_volcano_highlight_alpha"), label = h5("Transparency"), min = 0.1, 
                           max = 1, value = 0.5),
               checkboxInput(str_c(plot_number, "_stats_volcano_fixed_axis"), label = "Fix x and y axis for all plots?"),
               numericInput(str_c(plot_number, "_stats_volcano_y_axis"), label = "y axis", value = 10),
               numericInput(str_c(plot_number, "_stats_volcano_x_axis"), label = "x axis", value = 5)
             ),
             circle = TRUE, status = "danger", icon = icon("cogs"), width = "600px", size = "sm",
             tooltip = tooltipOptions(title = "Click to see inputs !")
           )
    )
  )

}

#---------------------------------------------------------------------------
create_stats_volcano_ui_part2 <- function(plot_number){

  fluidRow(
    column(width = 6, offset = 0,
      div(
        style = "position:relative",
        plotOutput(str_c(plot_number, "_volcano_stats_plot"), width = "40vw", height = "60vh",
                   hover = hoverOpts(str_c(plot_number, "_volcano_stats_hover"), delay = 100, delayType = "debounce")),
        uiOutput(str_c(plot_number, "_volcano_stats_hover_info"))
      ),
      br(),
      downloadButton(str_c(plot_number, "_download_stats_volcano"))
    )
  )
  
}


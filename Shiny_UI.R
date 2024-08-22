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
create_stats_bar_ui <- function() {
  fluidRow(
    column(width = 6, offet = 0,
      dropdownButton(
        textInput("stats_boxplot_y_axis_label", label = "y axis label", value = "Intensity", width = 200),
        textInput("stats_boxplot_title", label = "plot title", value = "Total Summed Intensity", width = 200),
        sliderInput("stats_boxplot_label_size", label = h5("Label Size"), min = 1, 
                    max = 50, value = 11),
        sliderInput("stats_boxplot_title_size", label = h5("Title Size"), min = 10, 
                    max = 50, value = 20),
        circle = TRUE, status = "danger", icon = icon("cogs"), width = "300px", size = "sm",
        tooltip = tooltipOptions(title = "Click to see inputs !")
      ),
      div(
        style = "position:relative",
        plotOutput("stats_boxplot", width = 600, height = 400)
      ),
      downloadButton('download_stats_boxplot')
    )
  )  
  
}



#-------------------------------------------------------------------------
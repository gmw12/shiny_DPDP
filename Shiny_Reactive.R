cat(file = stderr(), "load Shiny_Reactive.R", "\n")

get_design_file_name <- reactive({
  design_file_info <- parseFilePaths(volumes, input$sfb_design_file)
  design_file_path <- design_file_info$datapath
})

get_data_file_name <- reactive({
  data_file_info <- parseFilePaths(volumes, input$sfb_data_file)
  data_file_path <- basename(data_file_info$datapath)
})  


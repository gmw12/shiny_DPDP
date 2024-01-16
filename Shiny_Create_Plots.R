cat(file = stderr(), "Shiny_Create_Plots.R", "\n")

#------------------------------------------------------------------------------------------------------
parameter_create_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function parameter_create_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_start", "Precursor_Start", params$qc_path, params), stderr = str_c(params$error_path, "//error_barplot.txt"), supervise = TRUE)
  bg_box <- callr::r_bg(func = box_plot, args = list("precursor_start", "Precursor_Start", params$qc_path, params), stderr = str_c(params$error_path, "//error_boxplot.txt"), supervise = TRUE)
  bg_box$wait()
  bg_bar$wait()
  print_stderr("error_barplot.txt")
  print_stderr("error_boxplot.txt")

  wait_cycle <- 0
  while (!file.exists(str_c(params$qc_path,"Precursor_Start_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_parameters(session, input, output)
  
  filter_histogram_plot(session, input, output, params)
  
  cat(file = stderr(), "create parameter plots end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
filter_histogram_plot <- function(sesion, input, output, params){
  cat(file = stderr(), "Function filter histogram plot", "\n")
  showModal(modalDialog("Creating Histogram...", footer = NULL))
  
  bg_histogram <- callr::r_bg(func = histogram_plot, args = list("precursor_start", "Precursor_Start_Histogram", params), stderr = str_c(params$error_path, "//error_histogram.txt"), supervise = TRUE)
  bg_histogram$wait()
  print_stderr("error_histogram.txt")

  render_filter_histogram_graphs(session, input, output)
  
  params <<- param_load_from_database()
  cat(file = stderr(), "create histogram plot end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
filter_create_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function filter_create_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_filter", "Precursor_Filter", params$qc_path, params), stderr = str_c(params$error_path,  "//error_filterbarplot.txt"), supervise = TRUE)
  bg_box <- callr::r_bg(func = box_plot, args = list("precursor_filter", "Precursor_Filter", params$qc_path, params), stderr = str_c(params$error_path, "//error_filterboxplot.txt"), supervise = TRUE)
  bg_box$wait()
  bg_bar$wait()
  print_stderr("error_filterbarplot.txt")
  print_stderr("error_filterboxplot.txt")
  
  wait_cycle <- 0
  while (!file.exists(str_c(params$qc_path,"Precursor_Filter_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_filter(session, input, output)
  
  cat(file = stderr(), "create filter plots end", "\n")
  removeModal()
}









cat(file = stderr(), "Shiny_Create_Plots.R", "\n")

#------------------------------------------------------------------------------------------------------
parameter_create_plots <- function(sesion, input, output, params){
  cat(file = stderr(), "Function parameter_create_plots", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_start", "Precursor_Start", params$qc_path, params), stderr = "error_barplot.txt", supervise = TRUE)
  bg_box <- callr::r_bg(func = box_plot, args = list("precursor_start", "Precursor_Start", params$qc_path, params), stderr = "error_boxplot.txt", supervise = TRUE)
  bg_box$wait()
  bg_bar$wait()
  cat(file = stderr(), readLines("error_barplot.txt"), "\n")
  cat(file = stderr(), readLines("error_boxplot.txt"), "\n")
  
  bg_histogram <- callr::r_bg(func = histogram_plot, args = list("precursor_start", "Precursor_Start_Histogram", params), stderr = "error_histogram.txt", supervise = TRUE)
  bg_histogram$wait()
  cat(file = stderr(), readLines("error_histogram.txt"), "\n")
  
  wait_cycle <- 0
  while (!file.exists(str_c(params$qc_path,"Precursor_Start_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_parameters(session, input, output)
  
  render_impute_parameter_graphs(session, input, output)
  
  cat(file = stderr(), "create parameter plots end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------









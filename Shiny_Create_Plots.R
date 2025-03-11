cat(file = stderr(), "Shiny_Create_Plots.R", "\n")

#------------------------------------------------------------------------------------------------------
parameter_create_plots <- function(sesion, input, output, db_path){
  cat(file = stderr(), "Function parameter_create_plots...", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  qc_path <- get_param("qc_path", db_path)
  error_path <- get_param("error_path", db_path)
  
  bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_start", "Precursor_Start", qc_path, db_path), stderr = str_c(error_path, "//error_barplot.txt"), supervise = TRUE)
  bg_box <- callr::r_bg(func = box_plot, args = list("precursor_start", "Precursor_Start", qc_path, db_path), stderr = str_c(error_path, "//error_boxplot.txt"), supervise = TRUE)
  bg_box$wait()
  bg_bar$wait()
  print_stderr("error_barplot.txt", db_path)
  print_stderr("error_boxplot.txt", db_path)

  wait_cycle <- 0
  while (!file.exists(str_c(qc_path, "Precursor_Start_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_parameters(session, input, output, db_path)
  
  cat(file = stderr(), "create parameter plots... end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------
filter_histogram_plot <- function(sesion, input, output, db_path, table_name, plot_title){
  cat(file = stderr(), "Function filter histogram plot...", "\n")
  showModal(modalDialog("Creating Histogram...", footer = NULL))
  
  bg_histogram <- callr::r_bg(func = histogram_plot, args = list(table_name, plot_title, db_path), stderr = str_c(get_param('error_path', db_path), "//error_histogram.txt"), supervise = TRUE)
  bg_histogram$wait()
  print_stderr("error_histogram.txt", db_path)

  render_filter_histogram_graphs(session, input, output, db_path)
  
  cat(file = stderr(), "create histogram plot... end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
filter_create_plots <- function(sesion, input, output, db_path){
  cat(file = stderr(), "Function filter_create_plots...", "\n")
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  qc_path <- get_param("qc_path", db_path)
  error_path <- get_param("error_path", db_path)
  
  bg_bar <- callr::r_bg(func = bar_plot, args = list("precursor_filter", "Precursor_Filter", qc_path, db_path), stderr = str_c(error_path,  "//error_filterbarplot.txt"), supervise = TRUE)
  bg_box <- callr::r_bg(func = box_plot, args = list("precursor_filter", "Precursor_Filter", qc_path, db_path), stderr = str_c(error_path, "//error_filterboxplot.txt"), supervise = TRUE)
  bg_box$wait()
  bg_bar$wait()
  print_stderr("error_filterbarplot.txt", db_path)
  print_stderr("error_filterboxplot.txt", db_path)
  
  wait_cycle <- 0
  while (!file.exists(str_c(qc_path,"Precursor_Filter_barplot.png"))) {
    if (wait_cycle < 10) {
      Sys.sleep(0.5)
      wait_cycle <- wait_cycle + 1
    }
  }
  
  ui_render_filter(session, input, output, db_path)
  
  cat(file = stderr(), "create filter plots... end", "\n")
  removeModal()
}

#------------------------------------------------------------------------------------------------------
impute_create_plots <- function(sesion, input, output, db_path){
  cat(file = stderr(), "Function impute_create_plots...", "\n")
  
  showModal(modalDialog("Creating Plots...", footer = NULL))
  
  error_path <- get_param("error_path", db_path)
  
  bg_missing_bar <- callr::r_bg(func = missing_bar_plot, args = list(db_path), stderr = str_c(error_path, "//error_missing_bar.txt"), supervise = TRUE)
  bg_missing_percent <- callr::r_bg(func = missing_percent_plot, args = list(db_path), stderr = str_c(error_path, "//error_missing_percent.txt"), supervise = TRUE)
  
  bg_missing_bar$wait()
  print_stderr("error_missing_bar.txt", db_path)
  
  bg_missing_percent$wait()
  print_stderr("error_missing_percent.txt", db_path)
  
  cat(file = stderr(), "Function impute_create_plots... end", "\n")
  removeModal()
}







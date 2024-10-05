cat(file = stderr(), "Shiny_Startup.R", "\n")

set_user <- function() {
  cat(file = stderr(), "Function - set_user", "\n")
  
  #set user to unkown to force app to find correct usr
  site_user <<- "unknown"
  volumes <<- "unknown"
  database_dir <<- stringr::str_c(getwd(), "/database")
  
  while (site_user == "unknown") {
    if (Sys.info()["nodename"] == "oldmac") {
      volumes <<- c(dd = '/Users/gregwaitt/Documents/Data', wd = '.', Home = fs::path_home(),  getVolumes()())
      #version determines website content
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "titanshinyu20") {
      #for titan_black VM
      volumes <<- c(dd = '/home/dpmsr/shared/h_drive', dd2 = '/home/dpmsr/shared/other_black', RawData = '/home/dpmsr/shared/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "greg-GS63VR-7RF") {
      #for greg linux laptop
      volumes <<- c(dd = '/home/dpmsr/shared', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "greg-ThinkPad-W550s") {
      #for greg linux laptop
      volumes <<- c(dd = '/home/dpmsr/shared', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
    }else if (Sys.info()["nodename"] == "bob") {
      volumes <<- c(dd = '/home/dpmsr/mnt/h_black2', h1 = '/home/dpmsr/mnt/h_black1', h2 = '/home/dpmsr/mnt/h_black2', dc = 'home/dpmsr/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/dpmsr/anaconda3/envs/PDP/bin/python3"
    }else if (Sys.info()["nodename"] == "waittblack") {
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/dpmsr/anaconda3/envs/PDP/bin/python3"
    }else if (Sys.info()["nodename"] == "shiny-titan") {
      volumes <<- c(dd = '/mnt/h_black2', h1 = '/mnt/h_black1', h2 = '/mnt/h_black2', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/user/anaconda3/envs/python38/bin/python3"
    }else if (Sys.info()["nodename"] == "gregorys-mbp.lan" | Sys.info()["nodename"] == "Gregorys-MacBook-Pro.local" ) {
      volumes <<- c(dd = '/Users/gregwaitt/Data', dd2 = '/Users/gregwaitt/Cloud-Drive/R', dc = '/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      python_path <<- "/home/user/anaconda3/envs/python38/bin/python3"
    }else{
      #for public website
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "not_dpmsr"
      database_dir <<- "/data/database"
    }
  }
  
  cat(file = stderr(), str_c("site_user set to -->  ", site_user), "\n")
  cat(file = stderr(), str_c("volumes --> ", volumes), "\n")
  
  cat(file = stderr(), "Function - set_user...end", "\n\n")
  return()
}

#---------------------------------------------------------------------------------------------------------

create_default_params <- function(volumes, python_path) {
  cat(file = stderr(), "Function - create_default_params...", "\n")
  
  params <<- data.frame(
    "volumes" = toString(volumes),
    "volumes_name" = toString(names(volumes)),
    "database_dir" = stringr::str_c(getwd(), "/database"),
    "python_path" = python_path,
    "database_path" = "", #stringr::str_c(getwd(), "/database"),
    "design_path" = "",
    "design_file" = "",
    "data_source" = "",
    "file_prefix" = str_c("project_", strftime(Sys.time(), format = "%m%d%y")),
    "data_path" = "",
    "data_file" = "",
    "backup_path" = "",
    "extra_path" = "",
    "error_path" = "",
    "qc_path" = "",
    "string_path" = "",
    "phos_path" = "",
    "app_path" = "",
    "python_path" = "",
    "raw_data_format" = "", 
    "current_data_format" = "",
    "ptm" = FALSE, 
    "data_table_format" = "", 
    "primary_group" = "Full", 
    "data_output" = "Protein", 
    "norm_ptm" = FALSE, 
    "norm_ptm_grep" = "Phospho",
    "peptide_select" = 'razor', 
    "multi_tmt" = FALSE, 
    "use_isoform" = FALSE,
    "sample_number" = 0,
    "group_number" = 0,
    "unique_groups" = "na",
    "meta_precursor_raw" = 0,
    "meta_peptide_raw" = 0,
    "meta_protein_raw" = 0,
    "meta_precursor_filter" = 0,
    "meta_peptide_filter" = 0,
    "meta_protein_filter" = 0,
    "noise_type" = "none",
    "noise_baseline_value" = 1,
    "noise_inflection" = 0,
    "noise_count" = 0,
    "noise_total" = 0,
    "filter_min_measured_all" = 2,
    "filter_x_percent" = TRUE,
    "filter_x_percent_value" = 50,
    "filter_cv" = FALSE,
    "filter_cv_group" = "SPQC",
    "filter_cv_value" = 99,
    "info_col_precursor" = 0,
    "info_col_peptide" = 0,
    "info_col_protein" = 0,
    "intensity_cutoff" = 0,
    "intensity_mean" = 1,
    "intensity_cutoff_sd" = 0.5,
    "missing_cutoff" = 50,
    "checkbox_misaligned" = FALSE,
    "misaligned_cutoff" = 50,
    "misaligned_target" = "dataset",
    "total_na" = 0,
    "total_misaligned" = 0,
    "precursor_quality" = FALSE,
    "precursor_quality_sd" = 50,
    "precursor_quality_intensity" = 500,
    "precursor_quality_min" = 300,
    "precursor_spqc_ratio" = FALSE,
    "precursor_spqc_accuracy" = 50,
    "precursor_spqc_intensity" = 500,
    "norm_include" = FALSE,
    "include_norm_grep" = "trypsin|keratin|casein", 
    "norm_exclude" = FALSE,
    "exclude_norm_grep" = "trypsin|keratin|casein",
    "norm_type" = "impute",
    "protein_norm_grep" = "",
    "protein_norm_search_field" = "",
    "impute_type" = "duke",
    "bottom_x" = 2,
    "impute_ptm" = FALSE, 
    "impute_ptm_grep" = "Phospho",
    "meta_impute_na" = "",
    "rollup_method" = "sum",
    "rollup_topn" = 3,
    "maxlfq_scale" = 0,
    "stat_norm" = "impute",
    "comp_spqc" =  "",
    "comp_number" = 1,
    "tax_choice" = "",
    "string_species" = 0
    
  )
  
  cat(file = stderr(), "Function - create_default_params...end", "\n\n")
}


#---------------------------------------------------------------------------------------------------------


set_file_choosers <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_design_file', session = session, roots = volumes, filetypes = c('', 'xlsx'))
  #shinyFileChoose(input, 'sfb_data_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  shinyFileChoose(input, 'sfb_archive_file', session = session, roots = volumes, filetypes = c('', 'zip'))
  
  cat(file = stderr(), "Function - set_file_choosers...end", "\n\n")
}

#---------------------------------------------------------------------------------------------------------


set_file_choosers_data <- function(session, input, output, volumes) {
  cat(file = stderr(), "Function - set_file_choosers_reset...", "\n")
  cat(file = stderr(), stringr::str_c("Volumes ---> ", volumes), "\n")
  
  shinyFileChoose(input, 'sfb_data_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  cat(file = stderr(), "Function - set_file_choosers_reset...end", "\n")
}

#---------------------------------------------------------------------------------------------------------


app_startup <- function(session, input, output) {
  cat(file = stderr(), "Function - app_startup", "\n")
  source('Shiny_UI_Update.R')
  
  #Check if database file present
  if (params$database_path != "") {
    cat(file = stderr(), "params exists", "\n")
    loaded_database <- basename(params$database_path)
    loaded_prefix <- params$file_prefix
    
    #updateUI
    ui_render_load_design(session, input, output)
    ui_render_load_data(session, input, output)
    ui_render_parameters(session, input, output)
    render_parameters_graphs(session, input, output)
    ui_render_filter(session, input, output)
    render_noise_graphs(session, input, output)
    render_filter_graphs(session, input, output)
    render_norm_graphs(session, input, output)
    render_norm_apply_graphs(session, input, output)
    render_filter_histogram_graphs(session, input, output)
    render_impute_graphs(session, input, output) 
    #create_impute_table(session, input, output) 
    render_qc_graphs(session, input, output) 
 
      
    #update Widgets
    update_widgets(session, input, output, params)

    
  }else{
    loaded_database <- "none"
    loaded_prefix <- "none"
  }
  

  output$loaded_database <- renderText(str_c("Loaded Database:  ", loaded_database))
  output$loaded_prefix <- renderText(str_c("Loaded File Prefix:  ", loaded_prefix))
  
  #observers
  observe_comp_names(session, input, output)
  observe_plot_type1(session, input, output)   
  observe_plot_type2(session, input, output)  
  
  
  cat(file = stderr(), "Function - app_startup...end", "\n\n")
}

#-------------------------------------------------------------------
named_list <- function(input_string) {
  cat(file = stderr(), "Function named_list...", "\n")
  
  input_string <- params$norm_type
  named_list <- strsplit(input_string, ", ")
  list_names <- named_list
  named_list <- as.list(named_list)
  test <- c(named_list)
  test <- unlist(test)
  
  names(named_list) <- c(test)
  cat(file = stderr(), "Function named_list...end", "\n")
  return(named_list)
}
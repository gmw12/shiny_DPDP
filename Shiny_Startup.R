cat(file = stderr(), "Shiny_Startup.R", "\n")

set_user <- function() {
  cat(file = stderr(), "Function - set_user", "\n")
  
  #set user to unkown to force app to find correct usr
  site_user <<- "unknown"
  volumes <<- "unknown"
  
  while (site_user == "unknown") {
    if (Sys.info()["sysname"] == "Darwin" ) {
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
      volumes <<- c(h1 = '/home/dpmsr/mnt/h_black1', h2 = '/home/dpmsr/mnt/h_black2', dc = 'home/dpmsr/mnt/RawData', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "dpmsr"
      params$python_path <<- "/home/dpmsr/anaconda3/envs/PDP/bin/python3"
    }else{
      #for public website
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "not_dpmsr"
    }
  }
  
  cat(file = stderr(), str_c("site_user set to -->  ", site_user), "\n")
  cat(file = stderr(), str_c("volumes --> ", volumes), "\n")
  
  params$volumes <<- toString(volumes)
  params$volumes_name <<- toString(names(volumes))
  
  cat(file = stderr(), "Function - set_user...end", "\n")
  return(site_user)
}


#---------------------------------------------------------------------------------------------------------


app_startup <- function(session, input, output) {
  cat(file = stderr(), "Function - app_startup", "\n")
  
  #update_widgets(session, input, output)
  
  shinyFileChoose(input, 'sfb_design_file', session = session, roots = volumes, filetypes = c('', 'xlsx'))
  
  shinyFileChoose(input, 'sfb_data_file', session = session, roots = volumes, filetypes = c('', 'tsv', 'txt'))
  
  shinyFileChoose(input, 'sfb_database_file', session = session, roots = volumes, filetypes = c('', 'db'))
  
  #Check if database file present
  if (params$database_path != "na") {
    cat(file = stderr(), "params exists", "\n")
    loaded_database <- basename(params$database_path)
    loaded_prefix <- params$file_prefix
    
    #updateUI
    ui_render_load_design(session, input, output)
    ui_render_load_data(session, input, output)
    ui_render_parameters(session, input, output)
    render_parameters_graphs(session, input, output)
    ui_render_filter(session, input, output)
    render_filter_graphs(session, input, output)
    render_norm_graphs(session, input, output)
    
    #update Widgets
    update_widgets(session, input, output)
    
  }else{
    loaded_database <- "none"
    loaded_prefix <- "none"
  }
  

  output$loaded_database <- renderText(str_c("Loaded Database:  ", loaded_database))
  output$loaded_prefix <- renderText(str_c("Loaded File Prefix:  ", loaded_prefix))
  
  cat(file = stderr(), "Function - app_startup...end", "\n")
}

#---------------------------------------------------------------------------------------------------------
create_default_params <- function() {
  cat(file = stderr(), "Function - create_default_params", "\n")
  
  params <<- data.frame(
              "database_path" = "na",
              "design_path" = "na",
              "design_file" = "na",
              "data_source" = "SP",
              "file_prefix" = "project_date",
              "data_path" = "na",
              "data_file" = "na",
              "volumes" = "na",
              "volumes_name" = "na",
              "backup_path" = "na",
              "extra_path" = "na",
              "qc_path" = "na",
              "string_path" = "na",
              "phos_path" = "na",
              "app_path" = "na",
              "python_path" = "na",
              "raw_data_format" = "precursor", 
              "current_data_format" = "",
              "ptm" = FALSE, 
              "data_table_format" = "short", 
              "primary_group" = "Full", 
              "data_output" = "Protein", 
              "norm_ptm" = FALSE, 
              "norm_ptm_grep" = "Phospho",
              "impute_ptm" = FALSE, 
              "impute_ptm_grep" = "Phospho", 
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
              "filter_min_measured_all" = 2,
              "filter_x_percent" = FALSE,
              "filter_x_percent_value" = 0.5,
              "filter_cv" = FALSE,
              "filter_cv_group" = "SPQC",
              "filter_cv_value" = 99,
              "info_col_precursor" = 0,
              "info_col_peptide" = 0,
              "info_col_protein" = 0,
              "norm_include" = FALSE,
              "include_norm_grep" = "trypsin|keratin", 
              "norm_exclude" = FALSE,
              "exclude_norm_grep" = "trypsin|keratin",
              "norm_type" = "sl"
              
              
  )
}
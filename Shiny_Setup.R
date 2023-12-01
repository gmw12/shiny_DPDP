cat(file = stderr(), "Shiny_Setup.R", "\n")

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
      python_path <<- "/home/dpmsr/anaconda3/envs/PDP/bin/python3"
    }else{
      #for public website
      volumes <<- c(dd = '/data', wd = '.', Home = fs::path_home(), getVolumes()())
      site_user <<- "not_dpmsr"
    }
  }
  
  cat(file = stderr(), str_c("site_user set to -->  ", site_user), "\n")
  cat(file = stderr(), str_c("volumes --> ", volumes), "\n")

  cat(file = stderr(), "Function - set_user...end", "\n")
  return(site_user)
}



#---------------------------------------------------------------------
create_parameter_table <- function(session, input, output){
  cat(file = stderr(), "Function create_parameter_table", "\n")

  columns <- c("file_prefix", "data_path", "backup_path", "extra_path", 
               "qc_path", "string_path", "phos_path", "app_path", "raw_data_input", 
               "ptm", "data_format", "primary_group", "data_output", "norm_ptm", "norm_ptm_grep",
               "input_ptm", "imput_ptm_grep", "peptide_select", "multi_tmt", "use_isoform"
               )
  
  df = data.frame(matrix(nrow = 1, ncol = length(columns))) 
  colnames(df) <- columns
  
  df$file_prefix <- input$file_prefix
  
  #set defaults
  df$primary_group <- 1
  df$data_output <- 1
  df$norm_ptm <- FALSE
  df$norm_ptm_grep <- "Phospho"
  df$input_ptm <- FALSE
  df$input_ptm_grep <- "Phospho"
  df$peptide_select <- "razor"
  df$multi_tmt <- FALSE
  df$use_isoform <- FALSE
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  RSQLite::dbWriteTable(conn, "parameters", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)

  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function create_parameter_table...end", "\n")
  
  return(df)
}


#---------------------------------------------------------------------
save_parameters <- function(session, input, output){
  
  cat(file = stderr(), "Function save_parameter_table...", "\n")
  
  params$primary_group <<- input$primary_group
  params$data_output <<- input$data_output
  params$norm_ptm <<- input$norm_ptm
  params$norm_ptm_grep <<- input$norm_ptm_grep
  params$impute_ptm <<- input$impute_ptm
  params$impute_ptm_grep <<- input$impute_ptm_grep
  params$peptide_select <<- input$peptide_select
  params$multi_tmt <<- input$multi_tmt
  params$use_isoform <<- input$use_isoform

  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  RSQLite::dbWriteTable(conn, "parameters", params, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function save_parameter_table...end", "\n")
  
}


#---------------------------------------------------------------------
load_parameters <- function(session, input, output){
  
  cat(file = stderr(), "Function load_parameter_table...", "\n")
  
  updateCheckboxInput(session, "primary_group", value = params$primary_group)

  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function load_parameter_table...end", "\n")
  
}
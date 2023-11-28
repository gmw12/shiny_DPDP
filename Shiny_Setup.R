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

  columns <- c("fileprefix", "data_path", "backup_path", "extra_path", 
               "qc_path", "string_path", "phos_path", "app_path")
  
  df = data.frame(matrix(nrow = 1, ncol = length(columns))) 
  colnames(df) <- columns
  df$file_prefix <- input$file_prefix
  
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  RSQLite::dbWriteTable(conn, "parameters", df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)

  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  cat(file = stderr(), "Function create_parameter_table...end", "\n")
}

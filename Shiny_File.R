cat(file = stderr(), "Shiny_File.R", "\n")


#----------------------------------------------------------
Simple_fread <- function(file) {
  data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
} 


#----------------------------------------------------------------------------------------
file_set <- function(){
  cat(file = stderr(), "Function file_set", "\n")
  
  #set paths
  params$data_path <<- data_path
  params$backup_path <<- create_dir(str_c(data_path, "Backup"))
  params$extra_path <<- create_dir(str_c(data_path, "Extra"))
  params$qc_path <<- create_dir(str_c(data_path, "QC"))
  params$string_path <<- create_dir(str_c(data_path, "String"))
  params$phos_path <<- create_dir(str_c(data_path, "Phos"))
  params$app_path <<- create_dir(str_c(data_path, "Backup/App"))
  
  conn <- dbConnect(RSQLite::SQLite(), database_path)
  dbWriteTable(conn, "parameters", params, overwrite = TRUE)
  dbDisconnect(conn)
  
  #archive code with data
  cat(file = stderr(), "Function file_set... archive R files", "\n")
  r_files <- list.files()
  for (i in 1:length(r_files)) {
    if (grepl(".R$", r_files[i])) {
      file.copy(r_files[i], str_c(params$app_path, r_files[i]))
    }
  }
  
  cat(file = stderr(), "Function file_set...end", "\n")
}


#----------------------------------------------------------------------------------------
create_dir <- function(name){
  cat(file = stderr(), "Function create_dir...", "\n")
  if (is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists, deleting...", "\n")
    do.call(file.remove, list(list.files(name, full.names = TRUE)))
    dir_delete(name)
    dir_create(name)
  }else{
    dir_create(name)
  }
  name <- str_replace_all(name, "/", "//")
  name <- str_c(name, "//")
  cat(file = stderr(), str_c(name, " created...", "\n"))
  
  cat(file = stderr(), "Function create_dir...end", "\n")
  return(name)
}

#--------------------------------------------------------------

excel_to_db <- function(excel_path, table_name, database_path){
  df <- readxl::read_excel(excel_path)
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}


#----------------------------------------------------------------------------------------
save_data <- function(data_file){
  backup_path <- param_query("backup_path")
  file.copy(data_file, str_c(backup_path, basename(data_file)))
}

#----------------------------------------------------------------------------------------
save_data_bg <- function(file1, dir1){
  file2 <- paste(dir1, basename(file1))
  file.copy(file1, file2)
}

#----------------------------------------------------------------------------------------
param_query <- function(param){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  query_str <- str_c("SELECT ", param, " FROM Parameters")
  df <- RSQLite::dbGetQuery(conn, query_str)
  RSQLite::dbDisconnect(conn)
  return(df[1,1])
}

#----------------------------------------------------------------------------------------
param_update <- function(param, value){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  query_str <<- str_c("UPDATE Parameters SET ", param, "=", value)
  RSQLite::dbExecute(conn, query_str)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
param_refresh <- function(){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  params <<- RSQLite::dbReadTable(conn, "parameters")
  RSQLite::dbDisconnect(conn)
}
cat(file = stderr(), "Shiny_File.R", "\n")


#----------------------------------------------------------
Simple_fread <- function(file) {
  data.table::fread(file = file, header = TRUE, stringsAsFactors = FALSE, sep = "\t")
} 

#----------------------------------------------
Simple_Excel <- function(df, sheetname, filename) {
  cat(file=stderr(), stringr::str_c("Simple_Excel -> ", filename), "\n")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet=1, df)  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
}

#----------------------------------------------
db_table_to_Excel <- function(table_name, sheetname, filename, db_path) {
  cat(file=stderr(), stringr::str_c("Function db_table_to_Excel -> ", table_name, " to", filename), "\n")
  
  params <- get_params(db_path)
  
  arg_list <- list(table_name, sheetname, filename, params, db_path)
  bg_db_table_to_Excel <- callr::r_bg(func = db_table_to_Excel_bg , args = arg_list, stderr = stringr::str_c(params$error_path, "//error_db_table_to_Excel.txt"), supervise = TRUE)
  bg_db_table_to_Excel$wait()
  print_stderr("error_db_table_to_Excel.txt", db_path)
  cat(file=stderr(), stringr::str_c("db_table_to_Excel...end"), "\n")
}

#----------------------------------------------
db_table_to_Excel_bg <- function(table_name, sheetname, filename, params, db_path) {
  cat(file=stderr(), stringr::str_c("db_table_to_Excel_bg..."), "\n")
  
  source("Shiny_File.R")
  df <- read_table_try(table_name, db_path)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeData(wb, sheet=1, df)  
  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
  cat(file=stderr(), stringr::str_c("db_table_to_Excel_bg...end"), "\n")
}

#----------------------------------------------------------------------------------------
file_set <- function(db_path){
  cat(file = stderr(), "Function file_set", "\n")
  
  params <- get_params(db_path)
  
  #set paths
  params$backup_path <- create_dir(str_c(params$data_path, "Backup"))
  params$extra_path <- create_dir(str_c(params$data_path, "Extra"))
  params$qc_path <- create_dir(str_c(params$data_path, "QC"))
  params$string_path <- create_dir(str_c(params$data_path, "String"))
  params$phos_path <- create_dir(str_c(params$data_path, "Phos"))
  params$app_path <- create_dir(str_c(params$data_path, "Backup/App"))

  write_params(params, db_path)
  
  #archive code with data
  cat(file = stderr(), "Function file_set... archive R files", "\n")
  r_files <- list.files()
  for (i in 1:length(r_files)) {
    if (grepl(".R$", r_files[i])) {
      file.copy(r_files[i], stringr::str_c(params$app_path, r_files[i]))
    }
  }
  
  cat(file = stderr(), "Function file_set...end", "\n\n")
}

# name="/Users/gregwaitt/Data/project_022525/" 
#----------------------------------------------------------------------------------------
create_dir <- function(name){
  cat(file = stderr(), "Function create_dir...", "\n")
  if (fs::is_dir(name)) {
    cat(file = stderr(), stringr::str_c("dir exists, deleting -->", name), "\n")
    #added file delete, dir delete not working on customer shiny server
    unlink(name, recursive = TRUE)
    if (fs::is_dir(name)) {
      do.call(file.remove, list(list.files(name, full.names = TRUE)))
      fs::dir_delete(name)
    }
    cat(file = stderr(), "dir deleted, re-creating...", "\n")
  }else{
    cat(file = stderr(), stringr::str_c("dir does NOT exist, creating...", name), "\n")
    
  }
  
  fs::dir_create(name, mode="u=rwx,go=rwx", recurse=TRUE)
  
  
  if (fs::is_dir(name)) {
    name <- stringr::str_replace_all(name, "/", "//")
    name <- stringr::str_c(name, "//")
    cat(file = stderr(), stringr::str_c(name, " confirmed created...", "\n"))
  }else{
    cat(file = stderr(), stringr::str_c(name, " NOT created...", "\n"))
  }  
  
  cat(file = stderr(), "Function create_dir...end", "\n\n")
  return(name)
}
#----------------------------------------------------------------------------------------
create_dir_only <- function(name){
  cat(file = stderr(), "Function create_dir_only...", "\n")
  if (fs::is_dir(name)) {
    #added file delete, dir delete not working on customer shiny server
    cat(file = stderr(), "dir exists...", "\n")
  }else{
    dir_create(name)
    name <- str_replace_all(name, "/", "//")
    name <- stringr::str_c(name, "//")
    cat(file = stderr(), stringr::str_c(name, " created...", "\n"))
  }

  cat(file = stderr(), "Function create_dir_only...end", "\n\n")
  return(name)
}

#----------------------------------------------------------------------------------------
# Function to delete and recreate a directory using system commands
manage_directory2 <- function(dir_path) {
  # Convert to absolute path for safety
  dir_path <- normalizePath(dir_path, mustWork = FALSE)
  
  # Check if the directory exists
  if (dir.exists(dir_path)) {
    cat("Directory exists:", dir_path, "\n")
    cat("Removing directory and all its contents...\n")
    
    # Use system commands to delete the directory and all its contents
    if (.Platform$OS.type == "windows") {
      # Windows command
      system2("cmd", args = c("/c", paste0("rmdir /s /q \"", dir_path, "\"")), invisible = FALSE)
    } else {
      # Unix/Linux/Mac command
      system2("rm", args = c("-rf", shQuote(dir_path)), invisible = FALSE)
    }
    
    if (dir.exists(dir_path)) {
      stop("Failed to delete directory:", dir_path)
    } else {
      cat("Directory successfully deleted.\n")
    }
  } else {
    cat("Directory does not exist:", dir_path, "\n")
  }
  
  # Create the directory using system commands
  cat("Creating directory:", dir_path, "\n")
  
  if (.Platform$OS.type == "windows") {
    # Windows command
    system2("cmd", args = c("/c", paste0("mkdir \"", dir_path, "\"")), invisible = FALSE)
  } else {
    # Unix/Linux/Mac command
    system2("mkdir", args = c("-p", shQuote(dir_path)), invisible = FALSE)
  }
  
  if (dir.exists(dir_path)) {
    cat("Directory successfully created.\n")
  } else {
    stop("Failed to create directory:", dir_path)
  }
  
  return(invisible(dir_path))
}


#----------------------------------------------------------------------------------------
# Function to delete and recreate a directory
manage_directory <- function(dir_path) {
  cat(file = stderr(), "Function manage_directory...", "\n")
  # Convert to absolute path for safety
  dir_path <- normalizePath(dir_path, mustWork = FALSE)
  
  # Check if the directory exists
  if (dir.exists(dir_path)) {
    cat("Directory exists:", dir_path, "\n")
    cat("Removing directory and all its contents...\n")
    
    # Use unlink with recursive=TRUE to delete the directory and all its contents
    unlink(dir_path, recursive = TRUE)
    
    if (dir.exists(dir_path)) {
      stop("Failed to delete directory:", dir_path)
    } else {
      cat("Directory successfully deleted.\n")
    }
  } else {
    cat("Directory does not exist:", dir_path, "\n")
  }
  
  # Create the directory
  cat("Creating directory:", dir_path, "\n")
  dir.create(dir_path, recursive = TRUE)
  
  if (dir.exists(dir_path)) {
    cat("Directory successfully created.\n")
  } else {
    stop("Failed to create directory:", dir_path)
  }
  
  cat(file = stderr(), "Function manage_directory...end", "\n\n")
  return(invisible(dir_path))
}



#--------------------------------------------------------------
excel_to_db <- function(excel_path, table_name, db_path){
  df <- readxl::read_excel(excel_path)
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
save_data <- function(data_file, db_path){
  cat(file = stderr(), "Function - save_data...", "\n")
  backup_path <- get_param("backup_path", db_path)
  file.copy(data_file, stringr::str_c(backup_path, basename(data_file)))
  cat(file = stderr(), "Function - save_data...end", "\n\n")
}

#----------------------------------------------------------------------------------------
save_data_bg <- function(file1, dir1){
  file2 <- paste(dir1, basename(file1))
  file.copy(file1, file2)
}


#----------------------------------------------------------------------------------------
get_param <- function(param, db_path){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  query_str <- stringr::str_c("SELECT ", param, " FROM params")
  result <- RSQLite::dbGetQuery(conn, query_str)
  RSQLite::dbDisconnect(conn)
  return(result[1,1])
}

#----------------------------------------------------------------------------------------
param_update <- function(param, value, db_path){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
  query_str <- stringr::str_c("UPDATE params SET ", param, "=", value)
  RSQLite::dbExecute(conn, query_str)
  RSQLite::dbDisconnect(conn)
}


#----------------------------------------------------------------------------------------
table_exists <- function(table_name, db_path){
  cat(file = stderr(), "Function - table exists...", "\n")
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  tables_list <- dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  cat(file = stderr(), stringr::str_c("Function - table exists...end, ", table_name, "  ", table_name %in% tables_list), "\n")
  return(table_name %in% tables_list)
}
#-----------------------------------------------------------------------------------------
filter_db <- function(table_name, column_name, key_word, params) {
  require('RSQLite')
  conn <- dbConnect(RSQLite::SQLite(), params$database_path) 
  query <- stringr::str_c("SELECT * FROM ", table_name, " WHERE ", column_name, " LIKE '", key_word,"'") 
  df <- dbGetQuery(conn, query)
  RSQLite::dbDisconnect(conn)
  return(df)
}
#----------------------------------------------------------------------------------------
up <- function(packed_string){
  return(strsplit(packed_string, ","))
}


#----------------------------------------------------------------------------------------
print_stderr <- function(file_name, db_path){
  cat(file = stderr(), "Function - print_stderr...", "\n")
  error_list = readLines(stringr::str_c(get_param('error_path', db_path), "//", file_name))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }
  cat(file = stderr(), "Function - print_stderr...end", "\n")
}

#----------------------------------------------------------------------------------------
print_stderr2 <- function(file_name, db_path){
  cat(file = stderr(), "Function - print_stderr2...", "\n")
  error_list = readLines(stringr::str_c(get_param('error_path', db_path), "//", file_name))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }
  cat(file = stderr(), "Function - print_stderr2...end", "\n")
}
#----------------------------------------------------------------------------------------
print_temp_stderr <- function(file_name, error_path){
  error_list = readLines(stringr::str_c(error_path, "//", file_name))
  for (i in 1:length(error_list)) {
    cat(file = stderr(), error_list[i], "\n")
  }
}
#----------------------------------------------------------------------------------------
read_table <- function(table_name, db_path){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path) 
  df <- RSQLite::dbReadTable(conn, table_name)
  RSQLite::dbDisconnect(conn)
  return(df)
}

#----------------------------------------------------------------------------------------
read_table_try <- function(table_name, db_path){
  for (i in 1:10) {
    df <- try(read_table(table_name, db_path))
    if (class(df) != "try-error"){
      i <- 10
      return(df)
    }else{
      cat(file = stderr(), "Read table error...", "\n")
      Sys.sleep(0.5)
    }
  }
}

#----------------------------------------------------------------------------------------
get_params <- function(db_path){
  cat(file = stderr(), "function get_params...", "\n")
  for (i in 1:10) {
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path) 
    df <- RSQLite::dbReadTable(conn, "params")
    RSQLite::dbDisconnect(conn)
    if (class(df) != "try-error"){
      i <- 10
      return(df)
    }else{
      cat(file = stderr(), "Read table error...", "\n")
      Sys.sleep(0.5)
    }
  }
  cat(file = stderr(), "function get_params...end", "\n\n")
  return(df)
}

#----------------------------------------------------------------------------------------
write_table <- function(table_name, df, db_path){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path) 
  RSQLite::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  RSQLite::dbDisconnect(conn)
}

#----------------------------------------------------------------------------------------
write_table_try <- function(table_name, df, db_path){
  cat(file = stderr(), "function write_table_try...", "\n")
  for (i in 1:10) {
    test <- try(write_table(table_name, df, db_path))
    if (class(test) != "write-error"){
      i <- 10
      return(test)
    }else{
      cat(file = stderr(), "Write table error...", "\n")
      Sys.sleep(0.5)
    }
  }
  cat(file = stderr(), "function write_table_try...end", "\n")
}

#----------------------------------------------------------------------------------------
write_params <- function(params, db_path){
  cat(file = stderr(), "function write_params...", "\n")
  for (i in 1:10) {
    conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path) 
    test <- RSQLite::dbWriteTable(conn, "params", params, overwrite = TRUE)
    RSQLite::dbDisconnect(conn)
    if (class(test) != "write-error"){
      i <- 10
      return(test)
    }else{
      cat(file = stderr(), "Write table error...", "\n")
      Sys.sleep(0.5)
    }
  }
  cat(file = stderr(), "function write_params...end", "\n")
}

#----------------------------------------------------------------------------------------
list_tables <- function(db_path){
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path) 
  table_list <- RSQLite::dbListTables(conn)
  RSQLite::dbDisconnect(conn)
  return(table_list)
}

#----------------------------------------------------------------------------------------
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#----------------------------------------------------------------------------------------
zip_data_save <- function(session, input, output, db_path){
  cat(file = stderr(), "Function - zip_data_save...", "\n")
  source('Shiny_File.R')
  showModal(modalDialog("Saving data to zip file...", footer = NULL))
  
  params <- get_params(db_path)
  
  arg_list <- list(input$archive_data_filename, params, db_path)
  archive_data <- callr::r_bg(func = zip_data_save_bg, args = arg_list, stderr = stringr::str_c(params$error_path, "//error_archive_data.txt"), supervise = TRUE)
  archive_data$wait()
  print_stderr("error_archive_data.txt", db_path)
  
  removeModal()
  cat(file = stderr(), "Function - zip_data_save...end", "\n")
}

#----------------------------------------------------------------------------------------
zip_data_save_bg <- function(input_archive_data_filename, params, db_path){
  cat(file = stderr(), "Function - zip_data_save_bg...", "\n")
  #save(params, file="z1")
  
  filename <- stringr::str_c(params$data_path, input_archive_data_filename)
  files2zip <- dir(stringr::str_c(getwd(),"/database"), full.names = TRUE)
  
  utils::zip(zipfile = filename, files = files2zip, extras = '-j')
  
  cat(file = stderr(), "Function - zip_data_save_bg...end", "\n")
}

#----------------------------------------------------------------------------------------
getExtension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 
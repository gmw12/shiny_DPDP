cat(file = stderr(), "Shiny_Design.R", "\n")

#---------------------------------------------------------------------
load_design_file <- function(session, input, output){
  cat(file = stderr(), "Function load_design_file", "\n")
  
  design_sbf <- parseFilePaths(volumes, input$sfb_design_file)
  design_path <- str_extract(design_sbf$datapath, "^/.*/")
  
  #Global set of data and database paths
  data_path <<- str_c(design_path, input$file_prefix, "/")
  database_path <<- str_c(getwd(), "/database/", input$file_prefix, ".db")
  
  #create working directory for 
  create_dir(data_path)
  
  cat(file = stderr(), str_c("loading design file from ", design_path), "\n")
  
  test <- callr::r_bg(excel_to_db, args = list(design_sbf$datapath, "design", database_path), stderr = "error.txt", supervise = TRUE)
  test$wait()
  
  gc(verbose = getOption("verbose"), reset = FALSE, full = TRUE)
  
  cat(file = stderr(), "Function load_design_file...end", "\n")
}


#------------------------

check_comp_name_length <- function(){
  cat(file = stderr(), "check_comp_name_length...", "\n")
  names <- unique(dpmsr_set$design$Group)
  names_len <- sort(nchar(names), decreasing = TRUE)
  if (length(names) > 1) {
    longest_comp <- names_len[1] + names_len[2]
  }else{
    longest_comp <- names_len[1] + names_len[1]
  }
  if (longest_comp > 27) {
    return(TRUE)
  }else {
    return(FALSE)
  }
}

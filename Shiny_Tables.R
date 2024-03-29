cat(file = stderr(), "Shiny_Tables.R", "\n")

#------------------------------------------------------------------------
#load design table
create_design_table <- function(session, input, output){
  cat(file = stderr(), "Function create_design_table", "\n")
  
  bg_designtable <- callr::r_bg(create_design_table_bg, args = list(params$database_path), stderr = str_c(params$error_path, "//error_designtable.txt"), supervise = TRUE)
  bg_designtable$wait()
  print_stderr("error_designtable.txt")
  
  design_DT <- bg_designtable$get_result()
  output$stats_design_table <-  renderRHandsontable(design_DT)
  
  cat(file = stderr(), "Function create_design_table...end", "\n")
}

#--------------------------------

create_design_table_bg <- function(database_path){
  cat(file = stderr(), "Function create_design_table_bg", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  design <- RSQLite::dbReadTable(conn, "design", design)
  RSQLite::dbDisconnect(conn)
    
  design <- design |> dplyr::mutate_all(as.character)
  
  design_DT <- rhandsontable::rhandsontable(design, readOnly = TRUE, rowHeaders = NULL, digits = 0) |> 
    rhandsontable::hot_col(col = 'ID', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Replicate', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Label', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Group', halign = 'htCenter')
  
  cat(file = stderr(), "Function create_design_table_bg...end", "\n")
  return(design_DT)   
}

#------------------------------------------------------------------------
#load design table
create_stats_design_table <- function(session, input, output){
  cat(file = stderr(), "Function create_stats_design_table", "\n")
  
  bg_designtable <- callr::r_bg(create_stats_design_table_bg, args = list(params$database_path), stderr = str_c(params$error_path, "//error_statsdesigntable.txt"), supervise = TRUE)
  bg_designtable$wait()
  print_stderr("error_statsdesigntable.txt")
  
  design_DT <- bg_designtable$get_result()
  output$stats_design_table2 <-  renderRHandsontable(design_DT)

  cat(file = stderr(), "Function create_stats_design_table...end", "\n")
}

#--------------------------------

create_stats_design_table_bg <- function(database_path){
  cat(file = stderr(), "Function build_design_table_bg", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  design <- RSQLite::dbReadTable(conn, "design", design)
  RSQLite::dbDisconnect(conn)

  design <- design[, c("ID", "Replicate", "Label", "Group")] |> dplyr::mutate_all(as.character)
  colnames(design) <- c("ID", "Replicate", "Label", "Group")
  
  design_DT <- rhandsontable::rhandsontable(design, readOnly = TRUE, rowHeaders = NULL, digits = 0) |> 
    rhandsontable::hot_col(col = 'ID', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Replicate', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Label', halign = 'htCenter') |>
    rhandsontable::hot_col(col = 'Group', halign = 'htCenter')
  
  cat(file = stderr(), "Function create_stats_design_table_bg...end", "\n")
  return(design_DT)   
}

#--------------------------------------------------------------------------------------------------------------------


#load design table
create_impute_table <- function(session, input, output){
  cat(file = stderr(), "Function create_impute_table", "\n")
  
  bg_imputetable <- callr::r_bg(create_impute_table_bg, args = list(params$database_path), stderr = str_c(params$error_path, "//error_imputetable.txt"), supervise = TRUE)
  bg_imputetable$wait()
  print_stderr("error_imputetable.txt")
  
  impute_DT <- bg_imputetable$get_result()
  output$impute_meta_table <-  DT::renderDataTable(impute_DT)
  
  cat(file = stderr(), "Function create_impute_table...end", "\n")
}

#--------------------------------

create_impute_table_bg <- function(database_path){
  cat(file = stderr(), "Function build_design_table", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  df <- RSQLite::dbReadTable(conn, "missing_values")
  RSQLite::dbDisconnect(conn)
  
  impute_DT <-  DT::datatable(df,
                              rownames = FALSE,
                              options = list(
                                autoWidth = TRUE,
                                columnDefs = list(list(targets = c(0), visibile = TRUE, "width" = '5', className = 'dt-center'),
                                                  list(targets = c(1), visibile = TRUE, "width" = '5', className = 'dt-center')
                                ),
                                pageLength = 12, 
                                lengthMenu = c(12,20,100,500)
                              ))
  cat(file = stderr(), "Function build_impute_table...end", "\n")
  return(impute_DT)   
}

#--------------------------------------------------------------------------
#load design table
create_cv_table <- function(session, input, output, params){
  cat(file = stderr(), "Function create_cv_table", "\n")
  
  bg_cvtable <- callr::r_bg(create_cv_table_bg, args = list(params), stderr = str_c(params$error_path, "//error_cvtable.txt"), supervise = TRUE)
  bg_cvtable$wait()
  print_stderr("error_cvtable.txt")
  
  cv_DT <- bg_cvtable$get_result()
  output$qc_cv_table <-  renderRHandsontable(cv_DT)
  
  cat(file = stderr(), "Function create_cv_table...end", "\n")
}

#--------------------------------

create_cv_table_bg <- function(params){
  cat(file = stderr(), "Function create_cv_table_bg", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), params$database_path)
  cv_table <- RSQLite::dbReadTable(conn, "summary_cv")
  RSQLite::dbDisconnect(conn)
  
  cv_table[,-1] <- round(cv_table[,-1], digits = 1)
  cv_table <- cv_table |> dplyr::mutate_all(as.character)
  
  data_CV <- rhandsontable::rhandsontable(cv_table, readOnly = TRUE, rowHeaders = NULL, digits = 0)

  cat(file = stderr(), "Function create_cv_table_bg...end", "\n")
  
  return(data_CV)   
}

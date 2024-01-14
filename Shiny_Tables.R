cat(file = stderr(), "Shiny_Tables.R", "\n")

#load design table
create_design_table <- function(session, input, output){
  cat(file = stderr(), "Function create_design_table", "\n")
  
  bg_designtable <- callr::r_bg(create_design_table_bg, args = list(params$database_path), stderr = str_c(params$error_path, "//error_designtable.txt"), supervise = TRUE)
  bg_designtable$wait()
  print_stderr("error_designtable.txt")
  
  design_DT <- bg_designtable$get_result()
  output$stats_design_table <-  DT::renderDataTable(design_DT)
  
  cat(file = stderr(), "Function create_design_table...end", "\n")
}

#--------------------------------

create_design_table_bg <- function(database_path){
  cat(file = stderr(), "Function build_design_table", "\n")
  
  #get design data
  conn <- RSQLite::dbConnect(RSQLite::SQLite(), database_path)
  design <- RSQLite::dbReadTable(conn, "design", design)
  RSQLite::dbDisconnect(conn)
    
  #stats_design <- design[, c("ID", "Replicate", "Label", "Group")]
  design_DT <-  DT::datatable(design,
                                    rownames = FALSE,
                                    options = list(
                                      autoWidth = TRUE,
                                      columnDefs = list(list(targets = c(0), visibile = TRUE, "width" = '5', className = 'dt-center'),
                                                        list(targets = c(1), visibile = TRUE, "width" = '5', className = 'dt-center'),
                                                        list(targets = c(2), visibile = TRUE, "width" = '5', className = 'dt-center'),
                                                        list(targets = c(3), visibile = TRUE, "width" = '5', className = 'dt-center'),
                                                        list(targets = c(4), visibile = TRUE, "width" = '10', className = 'dt-center'),
                                                        list(targets = c(5), visibile = TRUE, "width" = '20', className = 'dt-center'),
                                                        list(targets = c(6), visibile = TRUE, "width" = '20', className = 'dt-center')
                                      ),
                                      pageLength = 12, 
                                      lengthMenu = c(12,20,100,500)
                                    ))
  cat(file = stderr(), "Function build_design_table...end", "\n")
  return(design_DT)   
}
cat(file = stderr(), "Shiny_Observers.R", "\n")

#----------------------------------------------------------------------------------------- 
observe_comp_names <- function(session, input, output){
  
  #---comparison info
  observe({
      
      compname1 <- input$comp1_name
      compname2 <- input$comp2_name
      compname3 <- input$comp3_name
      compname4 <- input$comp4_name
      compname5 <- input$comp5_name
      compname6 <- input$comp6_name
      compname7 <- input$comp7_name
      compname8 <- input$comp8_name
      compname9 <- input$comp9_name
    
      
      if (params$database_path != "na") {
        conn <- dbConnect(RSQLite::SQLite(), params$database_path)
        tables <- dbListTables(conn)
        if ("stats_comp" %in% tables) {
          stats_comp <- RSQLite::dbReadTable(conn, "stats_comp")
          if (nrow(stats_comp) > 0) {
            for (i in (1:nrow(stats_comp))) {
              stats_comp$Name[i] <- get(str_c("compname",i))
            }
            RSQLite::dbWriteTable(conn, "stats_comp", stats_comp, overwrite = TRUE)
          }
        }
        RSQLite::dbDisconnect(conn)
      }
      update_widgets_stats(session, input, output)
      #update_stat_comparisons(session, input, output)
  
  })

}



#-------------------------------------------------------------------------------------------------------------
observe_plot_type <- function(session, input, output){

  observe({
    cat(file = stderr(), "Observe plot_type1...", "\n")
  
    if (input$plot_type1 == "PCA_2D") {
  
      output$stats_plots1 <- renderUI({
        create_stats_pca2d_ui()
      })
  
    }
  
    cat(file = stderr(), "Observe plot_type1...end", "\n")
  })
  
}
cat(file = stderr(), "Shiny_Observers.R", "\n")
#---------------------------------------------------------
observe_buttons <- function(session, input , output) {
  cat(file = stderr(), "observe buttons loaded...", "\n")
  
  observeEvent(input$load_customer_archive_file,{
    runjs('document.getElementById("load_customer_archive_file").style.backgroundColor = "green";')
  })
  
  observeEvent(input$accept_parameters,{
    runjs('document.getElementById("accept_parameters").style.backgroundColor = "green";')
  })
 
  observeEvent(input$noise_apply,{
    runjs('document.getElementById("noise_apply").style.backgroundColor = "green";')
  })
  
  observeEvent(input$noise_inflection_apply,{
    runjs('document.getElementById("noise_inflection_apply").style.backgroundColor = "green";')
  })
  
  observeEvent(input$filter_cutoff,{
    runjs('document.getElementById("filter_cutoff").style.backgroundColor = "green";')
  })
  
  observeEvent(input$filter_apply,{
    runjs('document.getElementById("filter_apply").style.backgroundColor = "green";')
  })
  
  observeEvent(input$norm_parameters,{
    runjs('document.getElementById("norm_parameters").style.backgroundColor = "green";')
  })
  
  observeEvent(input$norm_apply,{
    runjs('document.getElementById("norm_apply").style.backgroundColor = "green";')
  })
  
  observeEvent(input$impute_parameters,{
    runjs('document.getElementById("impute_parameters").style.backgroundColor = "green";')
  })
  
  observeEvent(input$impute_apply,{
    runjs('document.getElementById("impute_apply").style.backgroundColor = "green";')
  })
  
  observeEvent(input$rollup_apply,{
    runjs('document.getElementById("rollup_apply").style.backgroundColor = "green";')
  })
  
  observeEvent(input$stat_options,{
    runjs('document.getElementById("stat_options").style.backgroundColor = "green";')
  })
  
  observeEvent(input$check_stats,{
    runjs('document.getElementById("check_stats").style.backgroundColor = "green";')
  })
  
  observeEvent(input$start_stats,{
    runjs('document.getElementById("start_stats").style.backgroundColor = "green";')
  })
  
  observeEvent(input$save_stats,{
    runjs('document.getElementById("save_stats").style.backgroundColor = "green";')
  })
  
  observeEvent(input$set_pathway,{
    runjs('document.getElementById("set_pathway").style.backgroundColor = "green";')
  })
  
  observeEvent(input$wiki_show,{
    runjs('document.getElementById("wiki_show").style.backgroundColor = "green";')
  })
  
  observeEvent(input$profile_go_show,{
    runjs('document.getElementById("profile_go_show").style.backgroundColor = "green";')
  })
  
  observeEvent(input$go_show,{
    runjs('document.getElementById("go_show").style.backgroundColor = "green";')
  }) 
  
  observeEvent(input$start_go_volcano,{
    runjs('document.getElementById("start_go_volcano").style.backgroundColor = "green";')
  })  
  
  observeEvent(input$get_string,{
    runjs('document.getElementById("get_string").style.backgroundColor = "green";')
  })  
  
  observeEvent(input$get_string_enrich,{
    runjs('document.getElementById("get_string_enrich").style.backgroundColor = "green";')
  })  
  
  observeEvent(input$parse_fasta,{
    runjs('document.getElementById("parse_fasta").style.backgroundColor = "green";')
  })  
  
  observeEvent(input$motif_show,{
    runjs('document.getElementById("motif_show").style.backgroundColor = "green";')
  })
  
}

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
      update_widgets_stats(session, input, output, params)
      #update_stat_comparisons(session, input, output)
  
  })

}



#-------------------------------------------------------------------------------------------------------------
observe_plot_type1 <- function(session, input, output){

  observe({
    cat(file = stderr(), "Observe plot_type1...", "\n")
  
    if (input$plot_type1 == "Bar") {
      cat(file = stderr(), stringr::str_c("Observed: Bar plot") , "\n")
      output$stats_plots1 <- renderUI({
        create_stats_bar_ui(plot_number=1)
      })
    }
  
    if (input$plot_type1 == "Box") {
      output$stats_plots1 <- renderUI({
        create_stats_box_ui(plot_number=1)
      })
    }
    
    if (input$plot_type1 == "PCA_2D") {
      output$stats_plots1 <- renderUI({
        create_stats_pca2d_ui(plot_number=1)
      })
    }
    
    if (input$plot_type1 == "PCA_3D") {
      output$stats_plots1 <- renderUI({
        create_stats_pca3d_ui(plot_number=1)
      })
    }
    
    if (input$plot_type1 == "Cluster") {
      output$stats_plots1 <- renderUI({
        create_stats_cluster_ui(plot_number=1)
      })
    }
    
    if (input$plot_type1 == "Heatmap") {
      output$stats_plots1 <- renderUI({
        create_stats_heatmap_ui(plot_number=1)
      })
    }
    
    if (input$plot_type1 == "Volcano") {
      
      output$stats_plots1 <- renderUI({
        create_stats_volcano_ui(plot_number=1)
      })
      
      output$create_stats_volcano_ui_1 <- renderUI({
        create_stats_volcano_ui_part1(plot_number=1)
      })
      
      output$create_stats_volcano_ui_2 <- renderUI({
        create_stats_volcano_ui_part2(plot_number=1)
      })
    }
    
    
    
    cat(file = stderr(), "Observe plot_type1...end", "\n")
  })
  
}

#-------------------------------
observe_plot_type2 <- function(session, input, output){

  observe({
    cat(file = stderr(), "Observe plot_type2...", "\n")
    
    if (input$plot_type2 == "Bar") {
      output$stats_plots2 <- renderUI({
        create_stats_bar_ui(plot_number=2)
      })
    }
    
    if (input$plot_type2 == "Box") {
      output$stats_plots2 <- renderUI({
        create_stats_box_ui(plot_number=2)
      })
    }
    
    if (input$plot_type2 == "PCA_2D") {
      output$stats_plots2 <- renderUI({
        create_stats_pca2d_ui(plot_number=2)
      })
    }
    
    if (input$plot_type2 == "PCA_3D") {
      output$stats_plots2 <- renderUI({
        create_stats_pca3d_ui(plot_number=2)
      })
    }
    
    if (input$plot_type2 == "Cluster") {
      output$stats_plots2 <- renderUI({
        create_stats_cluster_ui(plot_number=2)
      })
    }
    
    if (input$plot_type2 == "Heatmap") {
      output$stats_plots2 <- renderUI({
        create_stats_heatmap_ui(plot_number=2)
      })
    }
        
    if (input$plot_type2 == "Volcano") {
      output$stats_plots2 <- renderUI({
        create_stats_volcano_ui(plot_number=2)
      })
      
      output$create_stats_volcano_ui_1 <- renderUI({
        create_stats_volcano_ui_part1(plot_number=2)
      })
      
      output$create_stats_volcano_ui_2 <- renderUI({
        create_stats_volcano_ui_part2(plot_number=2)
      })
    }
      
    cat(file = stderr(), "Observe plot_type2...end", "\n")
  })

}
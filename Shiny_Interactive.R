cat(file = stderr(), "Shiny_Interactive.R", "\n")
#----------------------------------------------------------

interactive_barplot <- function(session, input, output, df, namex, color_list, output_name, comp_name, plot_number)
{
  cat(file = stderr(), "Function interactive_barplot...", "\n")
  # df <<- df
  # namex <<- namex 
  # color_list <<- color_list
  # output_name <<- output_name
  # comp_name <<- comp_name
  cat(file = stderr(), "interactive_barplot" , "\n")
  
  datay <- colSums(df, na.rm = TRUE)
  df2 <- data.frame(namex)
  df2$Total_Intensity <- datay
  colnames(df2) <- c("Sample", "Total_Intensity")
  df2$Sample <- factor(df2$Sample, levels = df2$Sample)
  ymax <- max(datay)
  
  cat(file = stderr(), "interactive_barplot...1", "\n")
  create_stats_barplot <- reactive({
    ggplot(data = df2, aes(x = Sample, y = Total_Intensity)) +
      geom_bar(stat = "identity", fill = color_list) + theme_classic() + 
      ggtitle(input[[str_c(plot_number, "_",output_name, "_title")]]) + 
      ylab(input[[str_c(plot_number, "_",output_name, "_y_axis_label")]]) +
      xlab(NULL) +
      #scale_y_discrete(labels = NULL) +
      coord_cartesian(ylim = NULL, expand = TRUE) +
      theme(plot.title = element_text(hjust = 0.5, size = input[[str_c(output_name, "_title_size")]]), 
            axis.title = element_text(size = input[[str_c(plot_number, "_",output_name, "_label_size")]], color = "black"),
            axis.text.x = element_text(size = input[[str_c(plot_number, "_",output_name, "_label_size")]], angle = 90,  color = "black"),
            axis.text.y = element_text(size = input[[str_c(plot_number, "_",output_name, "_label_size")]],  color = "black"),
      ) 
  })
  
  output[[str_c(plot_number, "_",output_name)]] <- renderPlot({
    req(create_stats_barplot())
    create_stats_barplot()
  })
  
  cat(file = stderr(), "interactive_barplot...2", "\n")
  output[[str_c(plot_number, "_download_", output_name)]] <- downloadHandler(
    filename = function(){
      str_c(plot_number, "_Stats_Barplot_", comp_name,  ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_barplot())
      ggsave(file, plot = create_stats_barplot(), device = 'png')
    }
  )
  cat(file = stderr(), "Function interactive_barplot...end", "\n")
}

#------------------------------------------------------------------------------------------------------------------------


interactive_boxplot <- function(session, input, output, df, namex, color_list, comp_string, plot_number)
{
  cat(file = stderr(), "interactive_boxplot" , "\n")
  
  colnames(df) <- namex
  df3 <- log2(df) %>% gather(Sample, Intensity, colnames(df))
  df3$Sample <- factor(df3$Sample, levels = rev(namex))
  
  create_stats_boxplot <- reactive({
    ggplot2::ggplot(data = df3, ggplot2::aes(x = Sample, y = Intensity)) +
      ggplot2::geom_boxplot(notch = TRUE, outlier.colour = "red", outlier.shape = 1,
                            outlier.size = 1, fill = rev(color_list)) + ggplot2::theme_classic() + 
      ggplot2::coord_flip() +
      ggplot2::xlab(input[[str_c(plot_number, "stats_boxplot_x_axis_label")]]) +
      ggplot2::ggtitle(input[[str_c(plot_number, "_stats_boxplot_title")]]) + 
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = input[[str_c(plot_number, "_stats_boxplot_title_size")]]), 
                     axis.title = ggplot2::element_text(size = input[[str_c(plot_number, "_stats_boxplot_label_size")]], color = "black"),
                     axis.text.x = ggplot2::element_text(size = input[[str_c(plot_number, "_stats_boxplot_label_size")]], angle = 90,  color = "black"),
                     axis.text.y = ggplot2::element_text(size = input[[str_c(plot_number, "_stats_boxplot_label_size")]],  color = "black"),
      ) 
  })
  
  output[[str_c(plot_number, "_stats_boxplot")]] <- renderPlot({
    req(create_stats_boxplot())
    #callr::r_bg(create_stats_boxplot, args = list(df3=df3), supervise = TRUE)
    create_stats_boxplot()
  })
  
  output[[str_c(plot_number, "_download_stats_boxplot")]]  <- downloadHandler(
    filename = function(){
      str_c(plot_number, "_stats_Boxplot_", comp_string, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_boxplot())
      ggsave(file, plot = create_stats_boxplot(), device = 'png')
    }
  )
  
}
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_pca2d <- function(session, input, output, df, namex, color_list, groupx, comp_name, plot_number)
{
  #test_df <<- df
  #test_groupx <<- groupx
  #df<-test_df
  #groupx <- test_groupx
  
  cat(file = stderr(), "interactive_pca2d" , "\n")
  x_transpose <- t(df)
  x_transpose <- data.frame(x_transpose)
  cat(file = stderr(), "interactive_pca2d...1" , "\n")
  row.names(x_transpose) <- NULL
  x_transpose <- cbind(groupx, x_transpose)
  
  x_pca <- prcomp(x_transpose[,-1], scale = TRUE)
  
  test_this <- x_transpose[,1]
  x_gr <- factor(unlist(test_this))
  cat(file = stderr(), "interactive_pca2d...2" , "\n")
  summary(x_gr)
  df_out <- as.data.frame(x_pca$x)
  #df_out_test <<- df_out
  df_xgr <- data.frame(x_gr)
  #df_xgr_test <<- df_xgr
  #df_xgr$x_gr <- as.character(df_xgr$x_gr)
  
  cat(file = stderr(), "interactive_pca2d...3" , "\n")
  
  hover_data <- data.frame(cbind(namex, df_out[[ input[[str_c(plot_number, "_stats_pca2d_x")]] ]], df_out[[input[[str_c(plot_number, "_stats_pca2d_y")]]]]), stringsAsFactors = FALSE  )
  colnames(hover_data) <- c("Sample", "get(input$stats_pca2d_x)", "get(input$stats_pca2d_y)")
  hover_data$`get(input[[str_c(plot_number, "_stats_pca2d_x")]])` <- as.numeric(hover_data$`get(input[[str_c(plot_number, "_stats_pca2d_x")]])`)
  hover_data$`get(input[[str_c(plot_number, "_stats_pca2d_y")]])` <- as.numeric(hover_data$`get(input[[str_c(plot_number, "_stats_pca2d_y")]])`)
  
  #hover_data_test <<- hover_data
  cat(file = stderr(), "interactive_pca2d...4" , "\n")
  create_stats_pca2d <- reactive({
    ggplot(df_out, aes(x = get(input[[str_c(plot_number, "_stats_pca2d_x")]]), y = get(input[[str_c(plot_number, "_stats_pca2d_y")]]), color = x_gr )) +
      geom_point(alpha = 0.8, size = input[[str_c(plot_number, "_stats_pca2d_dot_size")]] ) +
      theme(legend.title = element_blank()) +
      ggtitle(input[[str_c(plot_number, "_stats_pca2d_title")]]) + 
      ylab(input[[str_c(plot_number, "_stats_pca2d_y")]]) +
      xlab(input[[str_c(plot_number, "_stats_pca2d_x")]]) +
      scale_color_manual(values = rev(unique(color_list))) +
      theme(plot.title = element_text(hjust = 0.5, size = input[[str_c(plot_number, "_stats_pca2d_title_size")]]), 
            axis.title = element_text(size = input[[str_c(plot_number, "_stats_pca2d_label_size")]], color = "black"),
            axis.text.x = element_text(size =  input[[str_c(plot_number, "_stats_pca2d_label_size")]], angle = 90,  color = "black"),
            axis.text.y = element_text(size =  input[[str_c(plot_number, "_stats_pca2d_label_size")]],  color = "black"),
      ) 
  })
  
  cat(file = stderr(), "interactive_pca2d...5" , "\n")
  output[[str_c(plot_number, "_stats_pca2d")]] <- renderPlot({
    req(create_stats_pca2d())
    create_stats_pca2d()
  })
  
  output[[str_c(plot_number, "_download_stats_pca2d")]] <- downloadHandler(
    filename = function(){
      str_c(plot_number, "_stats_pca2d_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_pca2d())
      ggsave(file, plot = create_stats_pca2d(), device = 'png')
    }
  )
  
  cat(file = stderr(), "interactive_pca2d...6" , "\n")
  output$hover_pca2d_info <- renderUI({
    hover <- input$plot_pca2d_hover
    point <- nearPoints(hover_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- left_pct * (hover$range$right - hover$range$left)
    top_px <- top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    
    cat(file = stderr(), "interactive_pca2d...7" , "\n")
    if (top_pct > 0.3) {
      top_custom <- 10
    }else{
      top_custom <- 200
    }
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", 10, "px; top:", top_custom, "px;")
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Sample: </b>", point$Sample, "<br/>")))
    )
  })
  cat(file = stderr(), "interactive_pca2d...end" , "\n")
}

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_pca3d <- function(session, input, output, df, namex, color_list, groupx, comp_name, plot_number)
{
  cat(file = stderr(), "interactive_pca3d" , "\n")
  x_transpose <- t(df)
  x_transpose <- data.frame(x_transpose)
  row.names(x_transpose) <- NULL
  x_transpose <- cbind(groupx, x_transpose)
  
  x_pca <- prcomp(x_transpose[,-1], scale = TRUE)
  
  test_this <- x_transpose[,1]
  x_gr <- factor(unlist(test_this))
  summary(x_gr)
  
  create_stats_pca3d <- reactive({
    pca3d(x_pca, 
          group = x_gr,
          new = FALSE,
          legend = "right",
          palette = rev(unique(color_list)), 
          radius = input[[str_c(plot_number, "_stats_pca3d_dot_size")]],
          title = input[[str_c(plot_number, "_stats_pca3d_title")]]
    )
  })
  
  output[[str_c(plot_number, "_stats_pca3d")]] <- renderRglwidget({
    try(rgl.close())
    req(create_stats_pca3d())
    create_stats_pca3d()
    rglwidget()
  })
  
  output[[str_c(plot_number, "_download_stats_pca3d")]] <- downloadHandler(
    filename = function(){
      str_c(plot_number, "_stats_pca3d_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_pca3d())
      snapshotPCA3d(file)
      #ggsave(file, plot = create_stats_pca3d(), device = 'png')
    }
  )
}

#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_cluster <- function(session, input, output, df, namex, comp_name, plot_number)
{
  cat(file = stderr(), "interactive_cluster" , "\n")
  require('factoextra')
  
  colnames(df) <- namex
  
  df <- t(df)
  df <- data.frame(df)
  #row.names(df) <- NULL
  #df <- na.omit(df)
  df[] <- lapply(df, as.numeric)
  df <- scale(df)
  
  
  create_stats_cluster <- reactive({
    distance <- get_dist(df, method = "euclidean")
    fviz_dist(distance,  show_labels = TRUE, gradient = list(low = input[[str_c(plot_number, "_cluster_low_color")]], mid = "white", high = input[[str_c(plot_number, "_cluster_high_color")]] )) +
      ggtitle(input[[str_c(plot_number, "_stats_cluster_title")]]) +
      theme(plot.title = element_text(hjust = 0.5, size = input[[str_c(plot_number, "_stats_cluster_title_size")]] ), 
            axis.text.x = element_text(size = input[[str_c(plot_number, "_stats_cluster_label_size")]], angle = 90,  color = "black"),
            axis.text.y = element_text(size = input[[str_c(plot_number, "_stats_cluster_label_size")]],  color = "black"))
  })
  
  output[[str_c(plot_number, "_stats_cluster")]] <- renderPlot({
    req(create_stats_cluster())
    create_stats_cluster()
  })
  
  output[[str_c(plot_number, "_download_stats_cluster")]] <- downloadHandler(
    filename = function(){
      str_c(plot_number, "_stats_cluster_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      req(create_stats_cluster())
      ggsave(file, plot = create_stats_cluster(), device = 'png')
    }
  )
  
  
}


#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------

interactive_heatmap <- function(session, input, output, df, namex, groupx, comp_name, params)
{
  cat(file = stderr(), "interactive_heatmap..." , "\n")
  if (site_user == "dpmsr") {
    heatmap_filename <- "erasemyheatmap.png"
  }else{
    heatmap_filename <- str_c(params$data_path, "/erasemyheatmap.png")
  }
  
  #if norm by protein one protein will have 0 stdev - which will crash the calc...
  if (params$data_output == "Protein") {
    df$stdev <-  apply(df[1:ncol(df)], 1, sd) 
    df <- df[df$stdev > 0.1,]
    df <- df[,1:ncol(df) - 1] 
  }
  
  cat(file = stderr(), "interactive_heatmap...1" , "\n")
  colnames(df) <- namex
  df <- log2(df)
  df <- data.matrix(df)
  
  ## Row- and column-wise clustering 
  hr <- hclust(as.dist(1 - cor(t(df), method = "pearson")), method = "complete")
  hc <- hclust(as.dist(1 - cor(df, method = "spearman")), method = "complete") 
  ## Tree cutting
  mycl <- cutree(hr, h = max(hr$height)/1.5); mycolhc <- rainbow(length(unique(mycl)), start = 0.1, end = 0.9); mycolhc <- mycolhc[as.vector(mycl)] 
  ## Plot heatmap 
  mycol <- redgreen(75)
  
  
  create_stats_heatmap <- reactive({
    heatmap.2(df, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = mycol, labCol = groupx, 
              scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc, main = input[[str_c(plot_number, "_stats_heatmap_title")]],
              margins = c(10,10))
    #heatmap_filename <- str_c(dpmsr_set$file$output_dir, dpmsr_set$data$stats$final_comp, "//", "erasemyheatmap.png")
    png(filename = str_c(plot_number, "_", heatmap_filename), units = "px", width = 1776, height = 1776)  
    heatmap.2(df, Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = mycol, labCol = groupx, 
              scale = "row", density.info = "none", trace = "none", RowSideColors = mycolhc, main = input[[str_c(plot_number, "_stats_heatmap_title")]],
              margins = c(20,20))
    dev.off()
  })
  
  cat(file = stderr(), "interactive_heatmap...2" , "\n")
  
  output[[str_c(plot_number, "_stats_heatmap")]] <- renderPlot({
    req(create_stats_heatmap())
    create_stats_heatmap()
  })
  
  output[[str_c(plot_number, "_download_stats_heatmap")]] <- downloadHandler(
    filename = function(){
      str_c(plot_number, "_stats_heatmap_", comp_name, ".png", collapse = " ")
    },
    content = function(file){
      #heatmap_filename <- str_c(dpmsr_set$file$output_dir, dpmsr_set$data$stats$final_comp, "//", "erasemyheatmap.png")
      file.copy(heatmap_filename, file)
    }
  )
  
  cat(file = stderr(), str_c("heatmap file exists? ", heatmap_filename, "   ", file.exists(heatmap_filename)), "\n")
  try(file_delete(heatmap_filename), silent = TRUE)
  cat(file = stderr(), str_c("deleting temp heatmap file ", file.exists(heatmap_filename)), "\n")
  
}

#------------------------------------------------------------------------------------------------------------------------



















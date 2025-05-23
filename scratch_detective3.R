
set_name <- "10658_Precur_F234N73_Human_LFQ"
source('Shiny_File.R')

stats_df <- read_table_try('stats_comp', db_path)

ecoli_N = NULL
human_N = NULL
yeast_N = NULL
ecoli_D = NULL
human_D = NULL
yeast_D = NULL


for (i in (1:nrow(stats_df))){

  stats_N <- stats_df$FactorsN[i]
  stats_D <- stats_df$FactorsD[i]

  find_E <- str_locate(pattern = "E", stats_N)
  find_H <- str_locate(pattern = "H", stats_N)
  find_Y <- str_locate(pattern = "Y", stats_N)
  end_N <- nchar(stats_N)
  
  find_E <- as.numeric(substr(stats_N, find_E[1]+1, find_H[1]-1))
  find_H <- as.numeric(substr(stats_N, find_H[1]+1, find_Y[1]-1))
  find_Y <- as.numeric(substr(stats_N, find_Y[1]+1, end_N[1]))
  
  ecoli_N = c(ecoli_N, find_E)
  human_N = c(human_N, find_H)
  yeast_N = c(yeast_N, find_Y)
  
  find_E <- str_locate(pattern = "E", stats_D)
  find_H <- str_locate(pattern = "H", stats_D)
  find_Y <- str_locate(pattern = "Y", stats_D)
  end_N <- nchar(stats_D)
  
  find_E <- as.numeric(substr(stats_D, find_E[1]+1, find_H[1]-1))
  find_H <- as.numeric(substr(stats_D, find_H[1]+1, find_Y[1]-1))
  find_Y <- as.numeric(substr(stats_D, find_Y[1]+1, end_N[1]))
  
  ecoli_D = c(ecoli_D, find_E)
  human_D = c(human_D, find_H)
  yeast_D = c(yeast_D, find_Y)
}

ecoli_FC <- log(ecoli_N/ecoli_D,2) 
yeast_FC <- log(yeast_N/yeast_D,2)
human_FC <- log(human_N/human_D,2)

result_ecoli <- data.frame(matrix(ncol=12, nrow = nrow(stats_df)))
result_human <- data.frame(matrix(ncol=12, nrow = nrow(stats_df)))
result_yeast <- data.frame(matrix(ncol=12, nrow = nrow(stats_df)))

col_names <- c("Name", "Organism", "Count", "FC_target", "AvgFC", "Stdev", "Acc10", "Acc10_pval", "Acc20", "Acc20_pval", "Acc30", "Acc30_pval")
colnames(result_ecoli) <- col_names
colnames(result_human) <- col_names
colnames(result_yeast) <- col_names

x <- 0
for (i in stats_df$Final_Table_Name){
  cat(file = stderr(), str_c("data = ", i), "\n")
  x <- x + 1
  
  df <- read_table_try(i, db_path)
  df_ecoli <- df[df$Name %like% "ECOLI", ]
  df_human <- df[df$Name %like% "HUMAN", ]
  df_yeast <- df[df$Name %like% "YEAST", ]
  
  ecoli_fc <- df_ecoli |> dplyr::select(contains('FC2'))
  ecoli_fc <- log(ecoli_fc,2)
  ecoli_fc$acc <- ecoli_fc[,1]/ecoli_FC[x]
  ecoli_pval <- df_ecoli |> dplyr::select(contains('pval'))
  
  human_fc <- df_human |> dplyr::select(contains('FC2'))
  human_fc <- log(human_fc, 2)
  human_fc$acc <- human_fc[,1]/human_FC[x]
  human_pval <- df_human |> dplyr::select(contains('pval'))
  
  yeast_fc <- df_yeast |> dplyr::select(contains('FC2'))
  yeast_fc <- log(yeast_fc,2)
  yeast_fc$acc <- yeast_fc[,1]/yeast_FC[x]
  yeast_pval <- df_yeast |> dplyr::select(contains('pval'))
  
  result_ecoli$Organism[x] <- "Ecoli"
  result_ecoli$Name[x] <- set_name
  result_ecoli$Count[x] <- nrow(ecoli_fc)
  result_ecoli$FC_target[x] <- ecoli_FC[x]
  result_ecoli$AvgFC[x] <- colMeans(ecoli_fc)[1]
  result_ecoli$Stdev[x] <- round(sd(unlist(ecoli_fc[,1])) ,3)
  find_pval <- which(ecoli_pval[,1] <= 0.05)
  
  result_human$Organism[x] <- "Human"
  result_human$Name[x] <- set_name
  result_human$Count[x] <- nrow(human_fc)
  result_human$FC_target[x] <- human_FC[x]
  result_human$AvgFC[x] <- colMeans(human_fc)[1]
  result_human$Stdev[x] <- round(sd(unlist(human_fc[,1])), 3)
  find_pval <- which(human_pval[,1] <= 0.05)
  
  result_yeast$Organism[x] <- "Yeast"
  result_yeast$Name[x] <- set_name
  result_yeast$Count[x] <- nrow(yeast_fc)
  result_yeast$FC_target[x] <- yeast_FC[x]
  result_yeast$AvgFC[x] <- colMeans(yeast_fc)[1]
  result_yeast$Stdev[x] <- round(sd(unlist(yeast_fc[,1])),2)
  find_pval <- which(yeast_pval[,1] <= 0.05)
  
  
  find_fc <- which(ecoli_fc$acc > 0.9 & ecoli_fc$acc < 1.1 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_ecoli$Acc10[x] <- length(find_fc)
  result_ecoli$Acc10_pval[x] <- length(find_fc_pval)
  find_fc <- which(ecoli_fc$acc > 0.8 & ecoli_fc$acc < 1.2 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_ecoli$Acc20[x] <- length(find_fc)
  result_ecoli$Acc20_pval[x] <- length(find_fc_pval)
  find_fc <- which(ecoli_fc$acc > 0.7 & ecoli_fc$acc < 1.3 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_ecoli$Acc30[x] <- length(find_fc)
  result_ecoli$Acc30_pval[x] <- length(find_fc_pval)
  
  find_fc <- which(human_fc$acc > 0.9 & human_fc$acc < 1.1 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_human$Acc10[x] <- length(find_fc)
  result_human$Acc10_pval[x] <- length(find_fc_pval)
  find_fc <- which(human_fc$acc > 0.8 & human_fc$acc < 1.2 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_human$Acc20[x] <- length(find_fc)
  result_human$Acc20_pval[x] <- length(find_fc_pval)
  find_fc <- which(human_fc$acc > 0.7 & human_fc$acc < 1.3 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_human$Acc30[x] <- length(find_fc)
  result_human$Acc30_pval[x] <- length(find_fc_pval)
  
  find_fc <- which(yeast_fc$acc > 0.9 & yeast_fc$acc < 1.1 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_yeast$Acc10[x] <- length(find_fc)
  result_yeast$Acc10_pval[x] <- length(find_fc_pval)
  find_fc <- which(yeast_fc$acc > 0.8 & yeast_fc$acc < 1.2 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_yeast$Acc20[x] <- length(find_fc)
  result_yeast$Acc20_pval[x] <- length(find_fc_pval)
  find_fc <- which(yeast_fc$acc > 0.7 & yeast_fc$acc < 1.3 )
  find_fc_pval <- Reduce(intersect, list(find_pval, find_fc))
  result_yeast$Acc30[x] <- length(find_fc)
  result_yeast$Acc30_pval[x] <- length(find_fc_pval)


  
  hist_title <- stringr::str_c("Ecoli: ", ecoli_N[x]," / ", ecoli_D[x])
  df1 <- data.frame(ecoli_fc[,1])
  df1$type <- "Ecoli"
  colnames(df1) <- c("fc", "type")

  # p = ggplot(df1, aes(x=fc)) + 
  #   geom_histogram(aes(y=after_stat(density)), binwidth = 200, colour="black", fill="white")+
  #   geom_density(alpha=.2, fill="#FF6666") +
  #   labs(title=hist_title, x=i, y = "Count") + xlim(ecoli_FC[x]-4, ecoli_FC[x]+4)
  # print(p)
  
  hist_title <- stringr::str_c("Human: ", human_N[x]," / ", human_D[x])
  df2 <- data.frame(human_fc[,1])
  df2$type <- "Human"
  colnames(df2) <- c("fc", "type")

  # p= ggplot(df2, aes(x=fc)) + 
  #   geom_histogram(aes(y=after_stat(density)), binwidth = 200, colour="black", fill="white")+
  #   geom_density(alpha=.2, fill="#FF6666") +
  #   labs(title=hist_title, x=i, y = "Count") + xlim(human_FC[x]-4, human_FC[x]+4)
  # print(p)
  
  hist_title <- stringr::str_c("Combined: ", yeast_N[x]," / ", yeast_D[x])
  df3 <- data.frame(yeast_fc[,1])
  df3$type <- "Yeast"
  colnames(df3) <- c("fc", "type")
  
  # p= ggplot(df3, aes(x=fc)) + 
  #   geom_histogram(aes(y=after_stat(density)), binwidth = 200, colour="black", fill="white")+
  #   geom_density(alpha=.2, fill="#FF6666") +
  #   labs(title=hist_title, x=i, y = "Count") + xlim(yeast_FC[x]-4, yeast_FC[x]+4)
  # print(p)
  
  
  d1 <- density(df1[,1])
  d2 <- density(df2[,1])
  d3 <- density(df3[,1])
  
  peak_x1 <- round(d1$x[which.max(d1$y)],2) #Gives you the first highest Peak
  intensity1 <- data.frame(d1[c("x", "y")])[c(F, diff(diff(d1$y)>=0)<0),]
  peak_y1 <- max(intensity1$y)
  
  peak_x2 <- round(d2$x[which.max(d2$y)],2) #Gives you the first highest Peak
  intensity2 <- data.frame(d2[c("x", "y")])[c(F, diff(diff(d2$y)>=0)<0),]
  peak_y2 <- max(intensity2$y)
  
  peak_x3 <- round(d3$x[which.max(d3$y)],2) #Gives you the first highest Peak
  intensity3 <- data.frame(d3[c("x", "y")])[c(F, diff(diff(d3$y)>=0)<0),]
  peak_y3 <- max(intensity3$y)
  
  df_all <- rbind(df1, df2, df3)
  
  hist_title <- stringr::str_c("Combined: ", stats_df$FactorsN[x], " v ", stats_df$FactorsD[x])
  p= ggplot(df_all, aes(x=fc, color=type, fill=type)) + 
    geom_density(alpha=.2) + #, fill="lightgreen") +
    labs(title=hist_title, x=i, y = "Density") + xlim(-5, 5) + ylim(0,5) +
    geom_vline(xintercept=result_ecoli$FC_target[x], linetype="dotted") +
    geom_vline(xintercept=result_human$FC_target[x], linetype="dotted") +
    geom_vline(xintercept=result_yeast$FC_target[x], linetype="dotted") +
    annotate("text", x=peak_x1, y=peak_y1+0.5, label=result_ecoli$Stdev[x], size=5, color="black")+ 
    annotate("text", x=peak_x2, y=peak_y2+0.5, label=result_human$Stdev[x], size=5, color="black")+ 
    annotate("text", x=peak_x3, y=peak_y3+0.5, label=result_yeast$Stdev[x], size=5, color="black")+ 
    coord_flip()
  print(p)
  
}

final_result <- rbind(result_ecoli, result_human, result_yeast)
save(params, file=stringr::str_c(params$data_path, "params"))
Simple_Excel(final_result, set_name, stringr::str_c(params$data_path, set_name, "_final_result.xlsx"))


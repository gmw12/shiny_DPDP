
set_name <- "10658_Precur_F234N73_Human_LFQ"
source('Shiny_File.R')

stats_df <- read_table_try('stats_comp', params)

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
  
  df <- read_table_try(i, params)
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
  result_ecoli$Stdev[x] <- sd(unlist(ecoli_fc[,1]))
  find_pval <- which(ecoli_pval[,1] <= 0.05)
  
  result_human$Organism[x] <- "Human"
  result_human$Name[x] <- set_name
  result_human$Count[x] <- nrow(human_fc)
  result_human$FC_target[x] <- human_FC[x]
  result_human$AvgFC[x] <- colMeans(human_fc)[1]
  result_human$Stdev[x] <- sd(unlist(human_fc[,1]))
  find_pval <- which(human_pval[,1] <= 0.05)
  
  result_yeast$Organism[x] <- "Yeast"
  result_yeast$Name[x] <- set_name
  result_yeast$Count[x] <- nrow(yeast_fc)
  result_yeast$FC_target[x] <- yeast_FC[x]
  result_yeast$AvgFC[x] <- colMeans(yeast_fc)[1]
  result_yeast$Stdev[x] <- sd(unlist(yeast_fc[,1]))
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


  
  hist_title <- stringr::str_c("Ecoli: ", ecoli_spk[x]," / ", ecoli_comp[x])
  df <- data.frame(ecoli_fc[,1])
  colnames(df) <- c("fc")
  p = ggplot(df, aes(x=fc)) + 
    geom_histogram(aes(y=after_stat(density)), binwidth = 200, colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title=hist_title, x=i, y = "Count") + xlim(ecoli_FC[x]-4, ecoli_FC[x]+4)
  print(p)
  
  hist_title <- stringr::str_c("Human: ", human_spk[x]," / ", human_comp[x])
  df <- data.frame(human_fc[,1])
  colnames(df) <- c("fc")
  p= ggplot(df, aes(x=fc)) + 
    geom_histogram(aes(y=after_stat(density)), binwidth = 200, colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title=hist_title, x=i, y = "Count") + xlim(human_FC[x]-4, human_FC[x]+4)
  print(p)
  
  
  hist_title <- stringr::str_c("Yeast: ", yeast_spk[x]," / ", yeast_comp[x])
  df <- data.frame(yeast_fc[,1])
  colnames(df) <- c("fc")
  p= ggplot(df, aes(x=fc)) + 
    geom_histogram(aes(y=after_stat(density)), binwidth = 200, colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") +
    labs(title=hist_title, x=i, y = "Count") + xlim(yeast_FC[x]-4, yeast_FC[x]+4)
  print(p)
  
}

final_result <- rbind(result_ecoli, result_human, result_yeast)
save(params, file=stringr::str_c(params$data_path, "params"))
Simple_Excel(final_result, set_name, stringr::str_c(params$data_path, set_name, "_final_result.xlsx"))


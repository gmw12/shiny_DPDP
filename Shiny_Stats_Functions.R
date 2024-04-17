cat(file = stderr(), "Shiny_Stats_Functions.R", "\n")

#----------------------------------------------------------------------------------------- 

#Percent CV ---------------------------------
percentCV_gw <- function(x) {
  cat(file = stderr(), "function percentCV_gw...", "\n")
  
  ave <- rowMeans(x)
  n <- ncol(x)
  sd <- apply(x[1:n], 1, sd)
  cv <- (100 * sd / ave)
  
  cat(file = stderr(), "function percentCV_gw...end", "\n")
  return(signif(cv, digits = 3))
}



#cohensD ---------------------------------
cohend_gw <- function(x, y, hedges) {
  cat(file = stderr(), "function cohend_gw...", "\n")
  
  cohend <- function(x, y) {
    cohend_est = try(cohen.d(
      as.numeric(x),
      as.numeric(y),
      na.rm = TRUE,
      pooled = FALSE,
      paired = FALSE,
      hedges.correction = hedges
    ))
    if (is(cohend_est, "try-error"))
      return(NA)
    else
      return(signif((cohend_est$estimate), digits = 3))
  }
  
  x <- data.frame(t(x))
  y <- data.frame(t(y))
  
  cd <- mapply(function(x,y) cohend(x,y), x, y)
  
  cat(file = stderr(), "function cohend_gw...end", "\n")
  return(cd)
}

#missing factor ---------------------------------
missing_factor_gw <- function(df_N_missing, df_D_missing) {
  cat(file = stderr(), "function missing_factor_gw...", "\n")
  
  #if protein level set to max of 1
  df_N_missing[df_N_missing > 1 ] <- 1
  df_D_missing[df_D_missing > 1 ] <- 1  
  
  #calc mf and return highest of sample groups
  n <- df_N_missing |> dplyr::mutate_all(as.numeric)
  d <- df_D_missing |> dplyr::mutate_all(as.numeric)
  mf_n <- rowSums(n) / ncol(n)
  mf_d <- rowSums(d) / ncol(d)
  df_mf <- data.frame(cbind(mf_n, mf_d), stringsAsFactors = FALSE)
  df_mf$max <-
    apply(
      df_mf,
      1,
      FUN = function(x) {
        max(x, na.rm = TRUE)
      }
    )
  cat(file = stderr(), "function missing_factor_gw...end", "\n")
  return(signif(df_mf$max, digits = 3))
}



#fold change ---------------------------------
foldchange_gw <- function(x, y, params) {
  cat(file = stderr(), "function foldchange_gw...", "\n")
  
  if (!as.logical(params$pair_comp)) {
    ave_x = rowMeans(x)
    ave_y = rowMeans(y)
    test = ave_x / ave_y
  } else{
    sn <- ncol(x)
    indiv_fc <- x
    for (i in 1:sn) {
      indiv_fc[i] <- (x[i] / y[i])
    }
    test <- rowMeans(indiv_fc)
  }
  fc <- ifelse((test >= 1), test, -1 / test)
  
  cat(file = stderr(), "function foldchange_gw...end", "\n")
  return(signif(fc, digits = 7))
}

#fold change pair---------------------------------
foldchange_pair_gw <- function(x, y) {
  cat(file = stderr(), "function foldchange_pair_gw...", "\n")
  sn <- ncol(x)
  indiv_fc <- x
  for (i in 1:sn) {
    indiv_fc[i] <- (x[i] / y[i])
  }
  test <- rowMeans(indiv_fc)
  fc <- ifelse((test >= 1), test,-1 / test)
  
  cat(file = stderr(), "function foldchange_pair_gw...end", "\n")
  return(signif(fc, digits = 7))
}



#fold change decimal ---------------------------------
foldchange_decimal_gw <- function(x, y, params) {
  cat(file = stderr(), "function foldchange_decimal_gw...", "\n")
  
  if (!as.logical(params$pair_comp)) {
    ave_x = rowMeans(x)
    ave_y = rowMeans(y)
    test = ave_x / ave_y
  } else{
    sn <- ncol(x)
    indiv_fc <- x
    for (i in 1:sn) {
      indiv_fc[i] <- (x[i] / y[i])
    }
    test <- rowMeans(indiv_fc)
  }
  fc <- test
  
  cat(file = stderr(), "function foldchange_decimal_gw...end", "\n")
  return(signif(fc, digits = 7))
}

#fold change pair decimal---------------------------------
foldchange_pair_decimal_gw <- function(x, y) {
  cat(file = stderr(), "function foldchange_pair_decimal_gw...", "\n")
  
  sn <- ncol(x)
  indiv_fc <- x
  for (i in 1:sn) {
    indiv_fc[i] <- (x[i] / y[i])
  }
  test <- rowMeans(indiv_fc)
  fc <- test
  
  cat(file = stderr(), "function foldchange_pair_decimal_gw...end", "\n")
  return(signif(fc, digits = 7))
}





#t.test ---------------------------------
ttest_gw <- function(x, y, params) {
  if (!as.logical(params$pair_comp)) {
    ttest_pvalue = try(t.test(
      x,
      y,
      alternative = "two.sided",
      var.equal = FALSE,
      paired = FALSE
    ),
    silent = TRUE)
  } else{
    ttest_pvalue = t.test(x, y, paired = TRUE)
  }
  if (is(ttest_pvalue, "try-error"))
    return(NA)
  else
    return(signif((ttest_pvalue$p.value), digits = 7))
}


pvalue_gw <- function(x, y, params) {
  cat(file = stderr(), "function pvalue_gw...", "\n")
  
  x <- log2(x)
  y <- log2(y)
  temp_pval <- rep(NA, nrow(x))
  for (i in 1:nrow(x))
  {
    temp_pval[i] <- ttest_gw(as.numeric(x[i, ]), as.numeric(y[i, ]), params)
  }
  
  cat(file = stderr(), "function pvalue_gw...end", "\n")
  return(temp_pval)
}



exactTest_gw <- function(x, y) {
  #x <- log2(x)
  #y <- log2(y)
  et <- exactTestDoubleTail(x, y)
  return(et)
}



#x<-comp_N_data
#y<-comp_D_data
#comp_name <- comp_groups$comp_name[1]
limma_gw <- function(x, y) {   #, comp_name, plot_dir) {
  cat(file = stderr(), "function limma_gw...", "\n")
  
  xy <- cbind(x, y)
  xy <- log2(xy)
  n <- ncol(x)
  d <- ncol(y)
  design <- model.matrix( ~ 0 + factor(c(rep(1, n), rep(0, d))))
  colnames(design) <- c("group1", "group2")
  contrast.matrix <- makeContrasts(group2 - group1, levels = design)
  fit <- lmFit(xy, design)
  fit2 <- contrasts.fit(fit, contrast.matrix)
  fit2 <- eBayes(fit2)
  topfit <- topTable(fit2,
                     coef = 1,
                     sort = "none",
                     number = Inf)
  data_out <- topfit$P.Value
  #try(limma_qq(fit2$t, comp_name, plot_dir), silent = TRUE)
  #try(limma_ma(topfit, comp_name, plot_dir), silent = TRUE)
  #try(limma_volcano(fit2, comp_name, plot_dir), silent = TRUE)
  
  cat(file = stderr(), "function limma_gw...end", "\n")
  return(data_out)
}


old_limma_gw <- function(x, y) {
  xy <- cbind(x, y)
  xy <- log2(xy)
  n <- ncol(x)
  d <- ncol(y)
  design <- cbind(Grp1 = 1, Grp2vs1 = c(rep(1, n), rep(0, d)))
  #design <- c(0,0,0,1,1,1)
  # Ordinary fit
  fit <- lmFit(xy, design)
  fit <- eBayes(fit)
  topfit <-
    topTable(
      fit,
      coef = 2,
      adjust.method = "BH",
      sort = "none",
      number = Inf
    )
  data_out <- topfit$P.Value
  return(data_out)
}

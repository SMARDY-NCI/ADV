lsdAnalysis <- function(){
  # To begin, the complete matrix will be used to calibrate the ground truth of 
  # the PCA model. This model estimation step is typically referred as 
  # PCA-Model Building (PCA-MB). 
  
  # Then, the imputed versions will be used for PCA-MB using the TSR algorithm, 
  # which has proved to work well with a wide variety of datasets. 
  
  # Nonetheless, the Mean Squared Prediction Error (MSPE) will be kept to measure 
  # the estimation error due to the missing data imputation effect.
  
  # The following figure shows the plots with the average values of each 
  # parameter and the associated uncertainty calculated as the Least Squared 
  # Significant intervals, using the mean squared error after performing an 
  # ANOVA on each parameter, considering the simple effect of the factor 
  # "RW out (#)" and the nested random factor "Repetition". 
  # The dashed and solid orange lines are the percentile 2.5#, 50# and 97.5# 
  # of the distribution of each parameter obtained by resampling with k=200 
  # repetitions in which only a 1# of observations are removed. This is 
  # intended to give an idea of the expected uncertainty of the parameters 
  # caused just by a minimum removal of observations to fit the model. 
  # It is important to remark that these percentiles do not vary with 
  # "RW out (#)", and are just plotted as a reference level of results.
  # As it can be seen in most cases, the increase in the percentage of 
  # removed rows leads to significant changes on the model estimates. 
  # This can be appreciated by the fact that LSD intervals do not overlap as 
  # the "RW out (#)" values increase. However, these differences don't seem 
  # to be relevant in practice since the paremeter values are still within 
  # the expected values obtained with different samples of the 99#-rows database.
  lsdAnalysis <- list()
  orange_rgb <- c(0.8500, 0.3250, 0.0980)
  i.metrics <- which((grepl("*radius", names(para_test)) | grepl("*corr", names(para_test))))
  para_test$RWoutpctge <- as.factor(para_test$RWoutpctge)
  para_test$Repetition <- as.factor(para_test$Repetition)
  t = tiledlayout(2,2)
  for (j_metric in i.metrics){
    metric_name <- (colnames(para_test)[j_metric])
    lsdAnalysis[[metric_name]] <- aov(para_test[[metric_name]] ~ RWoutpctge/Repetition, data = para_test)
    a.tbl <- lsdAnalysis[[metric_name]]
    n.rep <- unique(para_test$Repetition)
    mse.lsd <- sum(a.tbl$residuals^2)/(nrow(para_test)-2)
    lsd.width <- sqrt(mse.lsd*2/n.rep)*(qt(1-0.025,stats.dfe)); 
    
    lsdfig()
    legend('off')
  }
  title(t,'Case II Rows deletion')
  xlabel(t,'RW removed(#)')
}

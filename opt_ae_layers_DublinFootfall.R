## ----setup, include=FALSE-----------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, 
                      out.width='50%', fig.align = "center",
                      message=FALSE)

library(dplyr)
library(magrittr)
library(factoextra)
library(qqplotr)
library(tictoc)
library(caret)
library(keras)
library(ggplot2)
library(tidyr)
library(tensorflow)
source(here::here("R","pcals.R"))
source(here::here("R","autoencoder.R"))
source(here::here("R","pca_functions.R"))
source(here::here("R","AEoptlayers.R"))
source(here::here("R","pcambtsrR.R"))
source(here::here("R","lsdAnalysis.R"))
source(here::here("R","lsdfig.R"))


## ----load data----------------------------------------------------------------------------------------------------
dat <- read.csv(here::here("data","pedestrian-count-dcc-2022.csv")) %>%
  select_if(is.numeric) %>%
  filter(complete.cases(.))


## ----delnullvar, out.width='70%'----------------------------------------------------------------------------------
print("These variables show null variance. Consider their deletion before the PCA")
summary(dat[,(apply(dat,2,var)==0),drop=F])
dat.or <- dat
dat <- dat[,!(apply(dat,2,var)==0),drop=F]
dat <- dat[,seq(1,ncol(dat),by=3)]
dat %>% gather() %>% head()
opt.AElay.results <- AEoptlayers(dat, A=4, tr = NULL, kcv = 10,act.fun = "relu", n.epochs = 50)

save("opt.AElay.results",file="optAElayers.RData")

load("optAElayers.RData")
df.anova.test <- as.data.frame(c(t(opt.AElay.results$losstscv)))
colnames(df.anova.test) <- "MSE_testcv"
df.anova.test$MSE_test <- c(t(opt.AElay.results$lossts))
df.anova.test$KFold <- paste0("Fold ",c(1:10))
df.anova.test$Method <- "Autoencoder"
df.anova.test$NHLayers <- as.factor(rep(c(0,1,2),each = 10))
df.anova.test$Repetition <- as.factor(rep(c(1:10), times=3))

aov.mse.testcv <- summary(aov(MSE_testcv ~ NHLayers + Error(Repetition), data = df.anova.test))
mse.aov.testcv <- aov.mse.testcv[["Error: Within"]][[1]]["Residuals","Mean Sq"]

aov.mse.test <- summary(aov(MSE_test ~ NHLayers + Error(Repetition), data = df.anova.test))
mse.aov.test <- aov.mse.test[["Error: Within"]][[1]]["Residuals","Mean Sq"]

lsd.testcv.width <- sqrt(mse.aov.testcv*2/length(unique(df.anova.test$Repetition)))*
  (qt(1-0.025,aov.mse.testcv[["Error: Within"]][[1]]["Residuals","Df"]))
lsd.test.width <- sqrt(mse.aov.test*2/length(unique(df.anova.test$Repetition)))*
	(qt(1-0.025,aov.mse.test[["Error: Within"]][[1]]["Residuals","Df"]))

lsd.DFopt.cv <- df.anova.test[,c(-2)]
colnames(lsd.DFopt.cv)[1] <- "MSE"
lsd.DFopt.cv$Test <- "Cross-Validation"
lsd.DFopt.cv$ylo <- lsd.DFopt.cv$MSE - lsd.testcv.width
lsd.DFopt.cv$yup <- lsd.DFopt.cv$MSE + lsd.testcv.width
lsd.DFopt.ev <- df.anova.test[,-1]
colnames(lsd.DFopt.ev)[1] <- "MSE"
lsd.DFopt.ev$Test <- "External-Validation"
lsd.DFopt.ev$ylo <- lsd.DFopt.ev$MSE - lsd.test.width
lsd.DFopt.ev$yup <- lsd.DFopt.ev$MSE + lsd.test.width
lsd.DFopt <- rbind(lsd.DFopt.cv,lsd.DFopt.ev)
lsd.DFopt$Test <- as.factor(lsd.DFopt$Test)
g.name <- names(lsd.DFopt[,"Test"])
g.levels <- levels(lsd.DFopt[,"Test"])
lsd.DFopt.plot <- aggregate(lsd.DFopt[,"MSE"] ~ lsd.DFopt[,"NHLayers"] + lsd.DFopt[,"Test"], 
														data=lsd.DFopt, mean)
colnames(lsd.DFopt.plot) <- c("NHLayers", "Test", "MSE")
lsd.DFopt.plot$yup <- lsd.DFopt.plot[,"MSE"] + c(rep(lsd.testcv.width,times=3),
																								 rep(lsd.test.width, times=3))
lsd.DFopt.plot$ylo <- lsd.DFopt.plot[,"MSE"] - c(rep(lsd.testcv.width,times=3),
																								 rep(lsd.test.width, times=3))

lsd_plot <- ggplot(lsd.DFopt.plot, aes(x = lsd.DFopt.plot[,"NHLayers"], 
																			 color = lsd.DFopt.plot[,"Test"])) + 
	geom_errorbar(aes(ymin=ylo, ymax=yup), width=0.3, size = 1,position=position_dodge(width=0.5)) +
	guides(color = guide_legend("Test set", title.position = "top", title.hjust = 0.5),
				 linetype = "none", fill = "none") +
	theme_minimal(base_size = 12) + ylab("MSE") + theme(legend.position = "top") +
	scale_fill_manual("Test",values=g.levels) +
	xlab("Number of Hidden Layers") + 
	ggtitle("Autoencoder architecture (Dublin Footfall data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))


lsdfig(df.anova.test,vy.name = "MSE_testcv",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.testcv.width, col=rgb(0,1,0,0.5),graph.out = "errorbar", 
			 ytext="MSE (cross validation)", xtext="Number of Hidden Layers",
       tittext = "Autoencoder architecture (Dublin Footfall data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

lsdfig(df.anova.test,vy.name = "MSE_test",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.test.width, col=rgb(0,1,0,0.5),graph.out = "errorbar", 
			 ytext="MSE (external validation)", xtext="Number of Hidden Layers",
			 tittext = "Autoencoder architecture (Dublin Footfall data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

df.anova.test.relu$logMSE <- log10(df.anova.test.relu$MSE)
lsd.logMSE <- aov(logMSE ~ NHLayers + Repetition, data = df.anova.test.relu)
summary(lsd.logMSE)
logmse.lsd <- sum(lsd.logMSE$residuals^2)/(lsd.logMSE$df.residual)
loglsd.width <- sqrt(logmse.lsd*2/length(unique(df.anova.test.relu$Repetition)))*
	(qt(1-0.025,lsd.logMSE$df.residual))
lsdfig(df.anova.test.relu,vy.name = "logMSE",vx.name ="NHLayers",vg.name = NULL,
			 yw=loglsd.width, col=rgb(0,1,0,0.5),graph.out = "errorbar",
			 ytext=bquote(log[10]~MSE), xtext="Number of Hidden Layers",
			 tittext = "Autoencoder architecture (Air Quality data)")

df.anova.test.log <- df.anova.test
df.anova.test.log$MSE <- log10(df.anova.test$MSE)
lsd.log.MSE <- aov(MSE ~ NLayers + Repetition, data = df.anova.test.log)
summary(lsd.log.MSE)

mse.lsd.log <- sum(lsd.log.MSE$residuals^2)/(lsd.log.MSE$df.residual)
exp2.lsd.results <- lsdAnalysis(exp2.data, "Method", hier = F)
lsd.width.log <- sqrt(mse.lsd.log*2/length(unique(df.anova.test.log$Repetition)))*
  (qt(1-0.025,lsd.log.MSE$df.residual))
lsdfig(df.anova.test.log,"MSE","NLayers","", lsd.width.log, col=rgb(0,1,0,0.5), 
       graph.out = "errorbar", ytext="log10 (MSE)", xtext="N.Layers", 
       tittext = "Autoencoder architecture")

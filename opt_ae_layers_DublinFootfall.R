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
colnames(df.anova.test) <- "MSE"
df.anova.test$KFold <- paste0("Fold ",c(1:10))
df.anova.test$Method <- "Autoencoder"
df.anova.test$NLayers <- as.factor(rep(c(1,2,3),each = 10))
df.anova.test$Repetition <- as.factor(rep(c(1:10), times=3))
lsd.MSE <- aov(MSE ~ NLayers + Repetition, data = df.anova.test)
summary(lsd.MSE)
mse.lsd <- sum(lsd.MSE$residuals^2)/(lsd.MSE$df.residual)
lsd.width <- sqrt(mse.lsd*2/length(unique(df.anova.test$Repetition)))*
  (qt(1-0.025,lsd.MSE$df.residual))
lsdfig(df.anova.test,vy.name = "MSE",vx.name ="NLayers",vg.name = NULL,
			 yw=lsd.width, col=rgb(0,1,0,0.5),graph.out = "errorbar", 
			 ytext="MSE", xtext="N.Layers", 
       tittext = "Autoencoder architecture (Dublin Footfall data")

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

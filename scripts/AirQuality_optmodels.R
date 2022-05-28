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
source(here::here("R","autoencoder_opt.R"))
source(here::here("R","autoenc_opt_layers.R"))
source(here::here("R","pca_functions.R"))
source(here::here("R","pcambtsrR.R"))
source(here::here("R","lsdAnalysis.R"))
source(here::here("R","lsdfig.R"))


## ----load data----------------------------------------------------------------------------------------------------
dat <- readxl::read_xlsx(here::here("data","AirQualityUCI.xlsx"))
dat[dat==-200] <- NA
dat <- dat %>% select_if(is.numeric) %>% filter(complete.cases(.))

## ----delnullvar, out.width='70%'----------------------------------------------------------------------------------
print("These variables show null variance. Consider their deletion before the PCA")
summary(dat[,(apply(dat,2,var)==0),drop=F])
dat.or <- dat
dat <- dat[,!(apply(dat,2,var)==0),drop=F]

## ----univanalysis-------------------------------------------------------------------------------------------------
summary(dat)
boxplot(dat,
				data=dat,
				main="Air Quality dataset",
				xlab="Parameter",
				ylab="",
				col="green3",
				border="black"
)
dat %>% gather() %>% head()

if(file.exists("AQ_optAElayers_relu.RData")){
	load("../AQ_optAElayers_relu.RData")
} else {
	tic("Autoencoder optimization")
	# opt.AE.results <- autoenc_opt(dat, A.values = c(1:10), tr = NULL, kcv = 20, act.fun = "relu", n.epochs = 50)
	opt.AElay.results.relu <- autoenc_opt_layers(dat, A=3, tr = NULL, kcv = 10,act.fun = "relu", n.epochs = 50)
	time.EA.opt <- toc()
	
	save("opt.AElay.results.relu",file="AQ_optAElayers_relu.RData")
}

if(file.exists("AQ_optAElayers_softmax.RData")){
	load("../AQ_optAElayers_softmax.RData")
} else {
	tic("Autoencoder optimization")
	opt.AElay.results.softmax <- autoenc_opt_layers(dat, A=3, tr = NULL, kcv = 10,act.fun = "softmax", n.epochs = 50)
	time.EA.opt <- toc()
	
	save("opt.AElay.results.softmax",file="AQ_optAElayers_softmax.RData")
}

opt.AElay.results.relu <- load("AQ_optAElayers_relu.RData")
opt.AElay.results.softmax <- load("AQ_optAElayers_softmax.RData")
# df.anova.test <- as.data.frame(c(t(opt.AElay.results.relu)))
# colnames(df.anova.test) <- "MSE"
# df.anova.test$KFold <- paste0("Fold ",c(1:10))
# df.anova.test$Method <- "Autoencoder"
# df.anova.test$NLayers <- as.factor(rep(c(1,2,3),each = 10))
# df.anova.test$Repetition <- as.factor(rep(c(1:10), times=3))
# lsd.MSE <- aov(MSE ~ NLayers + Repetition, data = df.anova.test)
# summary(lsd.MSE)
# mse.lsd <- sum(lsd.MSE$residuals^2)/(lsd.MSE$df.residual)
# lsd.width <- sqrt(mse.lsd*2/length(unique(df.anova.test$Repetition)))*
# 	(qt(1-0.025,lsd.MSE$df.residual))
# lsdfig(df.anova.test,vy.name = "MSE",vx.name ="NLayers",vg.name = NULL,
# 			 yw=lsd.width, col=rgb(0,1,0,0.5),graph.out = "errorbar", 
# 			 ytext="MSE", xtext="N.Layers", 
# 			 tittext = "Autoencoder architecture (Dublin Footfall data)")
# 
# df.anova.test.log <- df.anova.test
# df.anova.test.log$MSE <- log10(df.anova.test$MSE)
# lsd.log.MSE <- aov(MSE ~ NLayers + Repetition, data = df.anova.test.log)
# summary(lsd.log.MSE)
# 
# mse.lsd.log <- sum(lsd.log.MSE$residuals^2)/(lsd.log.MSE$df.residual)
# exp2.lsd.results <- lsdAnalysis(exp2.data, "Method", hier = F)
# lsd.width.log <- sqrt(mse.lsd.log*2/length(unique(df.anova.test.log$Repetition)))*
# 	(qt(1-0.025,lsd.log.MSE$df.residual))
# lsdfig(df.anova.test.log,"MSE","NLayers","", lsd.width.log, col=rgb(0,1,0,0.5), 
# 			 graph.out = "errorbar", ytext="log10 (MSE)", xtext="N.Layers", 
# 			 tittext = "Autoencoder architecture")
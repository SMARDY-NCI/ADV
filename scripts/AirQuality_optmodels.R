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

load("AQ_optAElayers_relu.RData")
load("AQ_optAElayers_softmax.RData")



df.anova.test.relu <- as.data.frame(c(t(opt.AElay.results.relu$losstscv)))
colnames(df.anova.test.relu) <- "MSE_testcv"
df.anova.test.relu$MSE_test <- c(t(opt.AElay.results.relu$lossts))
df.anova.test.relu$KFold <- paste0("Fold ",c(1:10))
df.anova.test.relu$Method <- "Autoencoder"
df.anova.test.relu$ActFunction <- "ReLu"
df.anova.test.relu$NHLayers <- as.factor(rep(c(0,1,2),each = 10))
df.anova.test.relu$Repetition <- as.factor(rep(c(1:10), times=3))

aov.mse.testcv <- summary(aov(MSE_testcv ~ NHLayers + Error(Repetition), data = df.anova.test.relu))
mse.aov.testcv <- aov.mse.testcv[["Error: Within"]][[1]]["Residuals","Mean Sq"]

aov.mse.test <- summary(aov(MSE_test ~ NHLayers + Error(Repetition), data = df.anova.test.relu))
mse.aov.test <- aov.mse.test[["Error: Within"]][[1]]["Residuals","Mean Sq"]

lsd.testcv.width <- sqrt(mse.aov.testcv*2/length(unique(df.anova.test.relu$Repetition)))*
	(qt(1-0.025,aov.mse.testcv[["Error: Within"]][[1]]["Residuals","Df"]))
lsd.test.width <- sqrt(mse.aov.test*2/length(unique(df.anova.test.relu$Repetition)))*
	(qt(1-0.025,aov.mse.test[["Error: Within"]][[1]]["Residuals","Df"]))

lsdfig(df.anova.test.relu,vy.name = "MSE_testcv",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.testcv.width, col=rgb(0,1,0,0.5),graph.out = "errorbar",
			 ytext="MSE (cross validation)", xtext="Number of Hidden Layers",
			 tittext = "Autoencoder architecture (Air Quality data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

lsdfig(df.anova.test.relu,vy.name = "MSE_test",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.test.width, col=rgb(0,1,0,0.5),graph.out = "errorbar",
			 ytext="MSE (external validation)", xtext="Number of Hidden Layers",
			 tittext = "Autoencoder architecture (Air Quality data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

lsd.AQopt.cv <- df.anova.test.relu[,c(-2)]
colnames(lsd.AQopt.cv)[1] <- "MSE"
lsd.AQopt.cv$Test <- "Cross-Validation"
lsd.AQopt.cv$ylo <- lsd.AQopt.cv$MSE - lsd.testcv.width
lsd.AQopt.cv$yup <- lsd.AQopt.cv$MSE + lsd.testcv.width
lsd.AQopt.ev <- df.anova.test.relu[,-1]
colnames(lsd.AQopt.ev)[1] <- "MSE"
lsd.AQopt.ev$Test <- "External-Validation"
lsd.AQopt.ev$ylo <- lsd.AQopt.ev$MSE - lsd.test.width
lsd.AQopt.ev$yup <- lsd.AQopt.ev$MSE + lsd.test.width
lsd.AQopt <- rbind(lsd.AQopt.cv,lsd.AQopt.ev)
lsd.AQopt$Test <- as.factor(lsd.AQopt$Test)
g.name <- names(lsd.AQopt[,"Test"])
g.levels <- levels(lsd.AQopt[,"Test"])
lsd.AQopt.plot <- aggregate(lsd.AQopt[,"MSE"] ~ lsd.AQopt[,"NHLayers"] + lsd.AQopt[,"Test"], 
														data=lsd.AQopt, mean)
colnames(lsd.AQopt.plot) <- c("NHLayers", "Test", "MSE")
lsd.AQopt.plot$yup <- lsd.AQopt.plot[,"MSE"] + c(rep(lsd.testcv.width,times=3),
																								 rep(lsd.test.width, times=3))
lsd.AQopt.plot$ylo <- lsd.AQopt.plot[,"MSE"] - c(rep(lsd.testcv.width,times=3),
																								 rep(lsd.test.width, times=3))

lsd_plot <- ggplot(lsd.AQopt.plot, aes(x = lsd.AQopt.plot[,"NHLayers"], 
																 color = lsd.AQopt.plot[,"Test"])) + 
	geom_errorbar(aes(ymin=ylo, ymax=yup), width=0.3, size = 1,position=position_dodge(width=0.5)) +
	guides(color = guide_legend("Test set", title.position = "top", title.hjust = 0.5),
				 linetype = "none", fill = "none") +
	theme_minimal(base_size = 12) + ylab("MSE") + theme(legend.position = "top") +
	scale_fill_manual("Test",values=g.levels) +
	xlab("Number of Hidden Layers") + 
	ggtitle("Autoencoder architecture (Air Quality data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))


lsd.MSE <- aov(MSE ~ NHLayers + Repetition, data = df.anova.test.relu)
summary(lsd.MSE)
mse.lsd <- sum(lsd.MSE$residuals^2)/(lsd.MSE$df.residual)
lsd.width <- sqrt(mse.lsd*2/length(unique(df.anova.test.relu$Repetition)))*
	(qt(1-0.025,lsd.MSE$df.residual))
lsdfig(df.anova.test.relu,vy.name = "MSE",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.width, col=rgb(0,1,0,0.5),graph.out = "errorbar",
			 ytext="MSE", xtext="Number of Hidden Layers",
			 tittext = "Autoencoder architecture (Air Quality data)") + 
	scale_y_continuous(labels = function(x) format(x, scientific = TRUE))


lsdfig(df.anova.test,vy.name = "MSE_testcv",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.testcv.width, col=rgb(0,1,0,0.5),graph.out = "errorbar", 
			 ytext="MSE (cross validation)", xtext="Number of Hidden Layers",
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

df.anova.test.softmax <- as.data.frame(c(t(opt.AElay.results.softmax$losstscv)))
colnames(df.anova.test.softmax) <- "MSE"
df.anova.test.softmax$KFold <- paste0("Fold ",c(1:10))
df.anova.test.softmax$Method <- "Autoencoder"
df.anova.test.softmax$ActFunction <- "SoftMax"
df.anova.test.softmax$NHLayers <- as.factor(rep(c(0,1,2),each = 10))
df.anova.test.softmax$Repetition <- as.factor(rep(c(1:10), times=3))


df.anova.test <- rbind(df.anova.test.relu,df.anova.test.softmax)
df.anova.test$ActFunction <- factor(df.anova.test$ActFunction)
df.anova.test$NHLayers <- factor(df.anova.test$NHLayers)
lsd.MSE <- aov(MSE ~ NHLayers + ActFunction + NHLayers*ActFunction, data = df.anova.test)
summary(lsd.MSE)
mse.lsd <- sum(lsd.MSE$residuals^2)/(lsd.MSE$df.residual)
lsd.width <- sqrt(mse.lsd*2/length(unique(df.anova.test$Repetition)))*
	(qt(1-0.025,lsd.MSE$df.residual))
lsdfig(df.anova.test.softmax,vy.name = "MSE",vx.name ="NHLayers",vg.name = NULL,
			 yw=lsd.width, col=rgb(0,1,0,0.5),graph.out = "errorbar",
			 ytext="MSE", xtext="N.Layers",
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
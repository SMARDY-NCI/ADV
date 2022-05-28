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
tic("Autoencoder optimization")
# opt.AE.results <- autoenc_opt(dat, A.values = c(1:10), tr = NULL, kcv = 20, act.fun = "relu", n.epochs = 50)
opt.AElay.results.relu <- autoenc_opt_layers(dat, A=3, tr = NULL, kcv = 10,act.fun = "relu", n.epochs = 50)
time.EA.opt <- toc()

save("opt.AElay.results",file="AQ_optAElayers_relu.RData")

# tic("PCA optimization")
# opt.PCA.results <- pca_opt(dat, A.values = c(1:10), tr = opt.AE.results$tr, kcv = 10)
# toc()
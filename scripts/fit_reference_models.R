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
source(here::here("R","pca_functions.R"))
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


## ----refmodels----------------------------------------------------------------------------------------------------
if(file.exists("../ref_models.RData") & file.exists(file="../ref_loadings.RData")){
  load("../ref_models.RData")
  load(file="../ref_loadings.RData")
} else {
  model.AE <- fit_autoencoder(dat, 4, "relu")
  plot(model.AErelu$trainhist)
  model.PCA <- fit_pca(dat, 4)
  # P.ae.ref <- as.matrix(model.AE$model$layers[[2]]$weights[[1]])
  # P.pca.ref <- model.PCA$model$rotation
  # save(list = c("P.ae.ref", "P.pca.ref"),file="ref_loadings.RData")
  save(list = c("model.AE", "model.PCA"),file="ref_models.RData")
}
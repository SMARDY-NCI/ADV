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
dat <- readxl::read_xlsx(here::here("data","AirQualityUCI.xlsx")) 
dat[dat==-200] <- NA

dat <- dat %>% select_if(is.numeric) %>% filter(complete.cases(.))

## ----delnullvar, out.width='70%'----------------------------------------------------------------------------------
print("These variables show null variance. Consider their deletion before the PCA")
summary(dat[,(apply(dat,2,var)==0),drop=F])
dat.or <- dat
dat <- dat[,!(apply(dat,2,var)==0),drop=F]

modelA.AQ <- keras_model_sequential()
modelA.AQ %>%
	layer_dense(units=ncol(dat), activation = "relu", input_shape = ncol(dat),
							use_bias = TRUE, name = "input") %>%
	layer_dense(units= 6, activation = "relu", input_shape = ncol(dat),
							use_bias = TRUE, name = "latent") %>%
	layer_dense(units=ncol(dat), activation = "relu", input_shape = 6,
							use_bias = TRUE, name = "output")

## ----refmodels----------------------------------------------------------------------------------------------------

model.AE <- fit_autoencoder(dat, 6, modelA.AQ, "relu")
plot(model.AE$trainhist)
model.PCA <- fit_pca(dat, 6)
P.ae.ref.all <- list()
P.ae.ref.all$input_layer <- as.matrix(model.AE$model$layers[[1]]$weights[[1]])
P.ae.ref.all$latent_layer <- as.matrix(model.AE$model$layers[[2]]$weights[[1]])
P.ae.ref.all$output_layer <- as.matrix(model.AE$model$layers[[3]]$weights[[1]])
P.ae.ref <- P.ae.ref.all$latent_layer
P.pca.ref <- model.PCA$model$rotation
save(list = c("P.ae.ref", "P.pca.ref", "P.ae.ref.all"),file="AQref_loadings.RData")
save(list = c("model.AE", "model.PCA"),file="AQref_models.RData")

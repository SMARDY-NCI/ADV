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
	layer_dense(units=8, activation = "relu", input_shape = ncol(dat),
							use_bias = TRUE, name = "hidden_in_1") %>%
	layer_dense(units= 3, activation = "relu", input_shape = 8,
							use_bias = TRUE, name = "latent") %>%
	layer_dense(units=8, activation = "relu", input_shape = 3,
							use_bias = TRUE, name = "hidden_out_1") %>%
	layer_dense(units=ncol(dat), activation = "relu", input_shape = 8,
							use_bias = TRUE, name = "output")

load("AQref_models.RData")
load(file="AQref_loadings.RData")

## ----coxtransforn, results='hide', echo=FALSE, message=FALSE------------------------------------------------------
# library(AID)
pca_transvars <- vpca_transcols(data = dat, 3, ref.P = P.pca.ref, 
                                   k_ho = 10, rm_pctges = c(5,10,seq(20,80,by=20)), xscale = TRUE)
autoencoder_transvars <- vae_transcols(data = dat, 3, ref.P = P.ae.ref, 
																					model.ae = modelA.AQ, ho.part = pca_transvars$ho,
																					n.latent.layer = 3,
                                          k_ho = 10, rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvars", "autoencoder_transvars"),file="AQ_coltrans_models.RData")


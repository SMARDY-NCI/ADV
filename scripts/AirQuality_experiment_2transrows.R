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
dat <- readxl::read_xlsx(here::here("data","AirQualityUCI.xlsx")) %>%
  select_if(is.numeric) %>%
  filter(complete.cases(.))


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

load("AQref_models.RData")
load(file="AQref_loadings.RData")

## ----transform, results="hide", echo=FALSE,out.width="50%",fig.asp=0.9--------------------------------------------
pca_transvars <- vpca_rowpctge(data = dat, 6, ref.P = P.pca.ref, 
                               k_ho = 10, rm_pctges = c(5,10,seq(20,80,by=20)))

autoencoder_transvars <- vae_rowpctge(data = dat, 6, ref.P = P.ae.ref,
																			model.ae = modelA.AQ, ho.part = pca_transvars$ho,
                                      k_ho = 10, n.latent.layer = 2,
                                      rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvars", "autoencoder_transvars"),file="AQ_RowsPctge_models.RData")

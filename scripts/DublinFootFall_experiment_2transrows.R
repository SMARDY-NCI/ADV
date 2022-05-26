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

load("ref_models.RData")
load(file="ref_loadings.RData")

## ----transform, results="hide", echo=FALSE,out.width="50%",fig.asp=0.9--------------------------------------------
pca_transvars <- vpca_rowpctge(data = dat, 4, ref.P = P.pca.ref, 
															 k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))

autoencoder_transvars <- vae_rowpctge(data = dat, 4, 
																			ref.P = P.ae.ref,
																			ho.part = pca_transvars$ho,
																			k_ho = 20, 
																			rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvars", "autoencoder_transvars"),file="RowsPctge_models.RData")

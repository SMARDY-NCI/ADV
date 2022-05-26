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
source(here::here("pcals.R"))
source(here::here("autoencoder.R"))
source(here::here("autoencoder_opt.R"))
source(here::here("autoenc_opt_layers.R"))
source(here::here("pca_functions.R"))
source(here::here("pcambtsrR.R"))
source(here::here("lsdAnalysis.R"))
source(here::here("lsdfig.R"))


## ----load data----------------------------------------------------------------------------------------------------
dat <- readxl::read_xlsx(here::here("data","AirQualityUCI.xlsx")) %>%
  select_if(is.numeric) %>%
  filter(complete.cases(.))


## ----delnullvar, out.width='70%'----------------------------------------------------------------------------------
print("These variables show null variance. Consider their deletion before the PCA")
summary(dat[,(apply(dat,2,var)==0),drop=F])
dat.or <- dat
dat <- dat[,!(apply(dat,2,var)==0),drop=F]

## ----coxtransforn, results='hide', echo=FALSE, message=FALSE------------------------------------------------------
library(AID)
pca_transvarscox <- vpca_transcols(data = dat, 6, ref.P = P.pca.ref, 
                                   k_ho = 10, rm_pctges = c(5,10,seq(20,80,by=20)))
autoencoder_transvarscox <- vae_transcols(data = dat, 6, 
                                          ref.P = P.ae.ref, ho.part = pca_transvarscox$ho,
                                          k_ho = 10, rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvarscox", "autoencoder_transvarscox"),file="AQ_CoxTrans_models.RData")


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

## ----remrows, results="hide",out.width="50%",fig.asp=0.9----------------------------------------------------------
load("ref_loadings.RData")
library(AID)
pca_transvarscox <- vpca_transcols(data = dat, 4, ref.P = P.pca.ref, 
                                   k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
autoencoder_transvarscox <- vae_transcols(data = dat, 4, 
                                          ref.P = P.ae.ref, ho.part = pca_transvarscox$ho,
                                          k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvarscox", "autoencoder_transvarscox"),file="CoxTrans_models.RData")

# pca_transvarscox$para_test$Method <- "PCA"
autoencoder_transvarscox$para_test$Method <- "AutoEncoder"
exp3.data <- rbind(pca_transvarscox$para_test[,-c(1,2)], autoencoder_transvarscox$para_test)
exp3.lsd.results <- lsdAnalysis(exp3.data, "Method", "Coltrans")
exp3.plots <- ggpubr::ggarrange(plotlist = exp3.lsd.results$l.plots, ncol = 2, nrow = 4,
                                font.label = list(size = 3, color = "black", face = "bold", family = NULL),
                                common.legend = TRUE)
ggpubr::annotate_figure(exp3.mdi.plots[[2]], top = ggpubr::text_grob("Experiment BoxCox Transform. (LSD interval)",
                                                                     color = "black", face = "bold", size = 12))
## ----remvars, results = "hide"------------------------------------------------------------------------------------



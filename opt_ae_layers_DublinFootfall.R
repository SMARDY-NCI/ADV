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
source(here::here("pcals.R"))
source(here::here("autoencoder.R"))
source(here::here("pca_functions.R"))
source(here::here("AEoptlayers.R"))
source(here::here("pcambtsrR.R"))
source(here::here("lsdAnalysis.R"))
source(here::here("lsdfig.R"))


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
dat %>% gather() %>% head()
opt.AElay.results <- AEoptlayers(dat, A=4, tr = NULL, kcv = 10,act.fun = "relu", n.epochs = 50)

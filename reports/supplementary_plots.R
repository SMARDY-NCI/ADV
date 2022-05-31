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

###############################################################################
#### Dublin Footfall 
###############################################################################

## ----load data----------------------------------------------------------------------------------------------------
dat.df <- read.csv(here::here("data","pedestrian-count-dcc-2022.csv")) %>%
	select_if(is.numeric(x)) %>%
	filter(complete.cases(.))

## ----delnullvar, out.width='70%'----------------------------------------------------------------------------------
print("These variables show null variance. Consider their deletion before the PCA")
summary(dat.df[,(apply(dat.df,2,var)==0),drop=F])
dat.df <- dat.df[,!(apply(dat.df,2,var)==0),drop=F]
dat.df <- dat.df[,seq(1,ncol(dat.df),by=3)]
dat.df %>% gather() %>% head()

ggplot(data = dat.df) + geom_line(aes(y=value)) + ggtitle("Dublin Footfall data")

###############################################################################
#### Air Quality 
###############################################################################

dat.aq <- readxl::read_xlsx(here::here("data","AirQualityUCI.xlsx"))
dat.aq[dat.aq==-200] <- NA
dat.aq <- dat.aq %>% select_if(is.numeric) %>% filter(complete.cases(.))

## ----delnullvar, out.width='70%'----------------------------------------------------------------------------------
print("These variables show null variance. Consider their deletion before the PCA")
summary(dat.aq[,(apply(dat.aq,2,var)==0),drop=F])
dat.aq <- dat.aq[,!(apply(dat.aq,2,var)==0),drop=F]
dat.aq %>% gather() %>% head()

ggplot(data = dat.aq) + geom_line() + ggtitle("Air Quality data")

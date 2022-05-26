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
dat <- readxl::read_xlsx(here::here("data","AirQualityUCI.xlsx")) %>%
	select_if(is.numeric) %>%
	filter(complete.cases(.))


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

ggplot(gather(dat), aes(value, after_stat(density))) + 
	geom_histogram(bins = 30) + 
	facet_wrap(~key, scales = 'free')

dat.log <- log10(dat) %>%
	filter(complete.cases(.))
c.names.vec <- colnames(dat)
c.names.vec[grepl("^T",c.names.vec)] <-"Temperature"
colnames(dat) <- c.names.vec
colnames(dat.log) <- c.names.vec
library(gridExtra)
lapply(c.names.vec,function(x){
	p1 <- ggplot(data = dat[,x], mapping = aes(sample = x)) +
		stat_qq_band(fill="blue",alpha = 0.15) +
		stat_qq_line(col="blue") +
		stat_qq_point(col="black",size=1)+
		labs(title = x)+theme_bw()
	p2 <- ggplot(data = dat.log[,x], mapping = aes(sample = x)) +
		stat_qq_band(fill="green",alpha = 0.15) +
		stat_qq_line(col="green") +
		stat_qq_point(col="black",size=1)+
		labs(title = x)+theme_bw()
	p1p2 <- grid.arrange(p1,p2,ncol=2)
	p1p2})

tic("Autoencoder optimization")
# opt.AE.results <- autoenc_opt(dat, A.values = c(1:10), tr = NULL, kcv = 20, act.fun = "relu", n.epochs = 50)
opt.AElay.results <- autoenc_opt_layers(dat, A=6, tr = NULL, kcv = 20,act.fun = "relu", n.epochs = 50)
time.EA.opt <- toc()

tic("PCA optimization")
opt.PCA.results <- pca_opt(dat, A.values = c(1:10), tr = opt.AE.results$tr, kcv = 20)
toc()

## ----mdimputation,echo=FALSE,out.width="50%",fig.asp=0.9----------------------------------------------------------
pca_remcells <- vpca_removecells(data = dat, 4, ref.P = P.pca.ref, 
																 k_ho = 20, rm_pctges = c(1,5,10,seq(20,80,by=20)))

autoencoder_remcells <- vae_removecells(data = dat, 4,
																				ref.P = P.ae.ref,
																				ho.part = pca_remcells$ho,
																				k_ho = 20,
																				rm_pctges = c(1,5,10,seq(20,80,by=20)))
save(list = c("pca_remcells", "autoencoder_remcells"),file="MissingData_models.RData")


## ----transform, results="hide", echo=FALSE,out.width="50%",fig.asp=0.9--------------------------------------------
pca_transvars <- vpca_rowpctge(data = dat, 4, ref.P = P.pca.ref, 
															 k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))

autoencoder_transvars <- vae_rowpctge(data = dat, 4, 
																			ref.P = P.ae.ref,
																			ho.part = pca_transvars$ho,
																			k_ho = 20, 
																			rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvars", "autoencoder_transvars"),file="RowsPctge_models.RData")


## ----coxtransforn, results='hide', echo=FALSE, message=FALSE------------------------------------------------------
library(AID)
pca_transvarscox <- vpca_transcols(data = dat, 4, ref.P = P.pca.ref, 
																	 k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
autoencoder_transvarscox <- vae_transcols(data = dat, 4, 
																					ref.P = P.ae.ref, ho.part = pca_transvarscox$ho,
																					k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
save(list = c("pca_transvarscox", "autoencoder_transvarscox"),file="CoxTrans_models.RData")


## ----remrows, results="hide",out.width="50%",fig.asp=0.9----------------------------------------------------------
pca_remrows <- vpca_removerows(data = dat, 4, ref.P = P.pca.ref, k_ho = 20, 
															 rm_pctges = c(2,5,10,seq(20,80,by=20)))
autoencoder_remrows <- vautoencoder_removerows(data = dat, 4, ref.P = P.ae.ref,k_ho = 20, 
																							 ho.part = pca_remrows$ho,
																							 rm_pctges = c(2,5,10,seq(20,80,by=20)))
save(list = c("pca_remrows", "autoencoder_remrows"),file="RemRows_models.RData")

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


## ----univanalysis-------------------------------------------------------------------------------------------------
summary(dat)
boxplot(dat,
data=dat,
main="Counts of pedestrians per each street",
xlab="Street",
ylab="Counts",
col="green3",
border="black"
)
dat %>% gather() %>% head()

ggplot(gather(dat), aes(value, after_stat(density))) + 
    geom_histogram(bins = 30) + 
    facet_wrap(~key, scales = 'free')

library(gridExtra)
lapply(1:ncol(dat),function(x){
  p1 <- ggplot(data = dat, mapping = aes(sample = dat[,x])) +
  stat_qq_band(fill="blue",alpha = 0.15) +
  stat_qq_line(col="blue") +
  stat_qq_point(col="black",size=1)+
  labs(title = colnames(dat)[x])+theme_bw()
  
  p2 <- ggplot(data = dat.log, mapping = aes(sample = dat.log[,x])) +
  stat_qq_band(fill="green",alpha = 0.15) +
  stat_qq_line(col="green") +
  stat_qq_point(col="black",size=1)+
  labs(title = colnames(dat.log)[x])+theme_bw()
  
  p1p2 <- grid.arrange(p1,p2,ncol=2)
  p1p2})



## ----optmodels, echo=FALSE----------------------------------------------------------------------------------------
# tic("Autoencoder optimization")
# opt.AE.results <- autoenc_opt(dat, A.values = c(1:10), tr = NULL, kcv = 10, act.fun = "relu", n.epochs = 50) 
# time.EA.opt <- toc()

# tic("PCA optimization")
# opt.PCA.results <- pca_opt(dat, A.values = c(1:10), tr = opt.AE.results$tr, kcv = 10) 
# time.PCA.opt <- toc()
load("../opt_models.RData")
df.anova.test <- as.data.frame(c(as.vector(opt.AE.results$lossts[1:10,]), 
                                     as.vector(opt.PCA.results$lossts)))
colnames(df.anova.test) <- "MSE"
df.anova.test$Method <- rep(c("Autoencoder", "PCA"),each = 10*10)
df.anova.test$NLVs <- as.factor(rep(c(1:10),times = 2*10))
df.anova.test$Repetition <- as.factor(rep(rep(c(1:10),each = 10), times=2))
lsd.MSE <- aov(MSE ~ Method + NLVs + Repetition, data = df.anova.test)
summary(lsd.MSE)

df.anova.test.log <- df.anova.test
df.anova.test.log$MSE <- log10(df.anova.test$MSE)
lsd.log.MSE <- aov(MSE ~ Method + NLVs + Repetition, data = df.anova.test.log)
summary(lsd.log.MSE)

mse.lsd.log <- sum(lsd.log.MSE$residuals^2)/(lsd.log.MSE$df.residual)
lsd.width <- sqrt(mse.lsd.log*2/length(unique(df.anova.test.log$Repetition)))*
  (qt(1-0.025,lsd.log.MSE$df.residual))
lsdfig(df.anova.test.log,"MSE","NLVs", lsd.width, col=rgb(0,1,0,0.5), ytext="log10 (MSE)", 
       xtext="Latent dimension", tittext = "Reference models")


## ----refmodels----------------------------------------------------------------------------------------------------
if(file.exists("../ref_models.RData") & file.exists(file="../ref_loadings.RData")){
  load("../ref_models.RData")
  load(file="../ref_loadings.RData")
} else {
  model.AE <- fit_autoencoder(dat, 4, "relu")
  plot(model.AErelu$trainhist)
  model.PCA <- fit_pca(dat, 4)
  P.ae.ref <- as.matrix(model.AE$model$layers[[2]]$weights[[1]])
  P.pca.ref <- model.PCA$model$rotation
  save(list = c("P.ae.ref", "P.pca.ref"),file="ref_loadings.RData")
  save(list = c("model.AE", "model.PCA"),file="ref_models.RData")
}


## ----mdimputation,echo=FALSE,out.width="50%",fig.asp=0.9----------------------------------------------------------
if(file.exists("../MissingData_models.RData")){
  load("../MissingData_models.RData")
} else {
  pca_remcells <- vpca_removecells(data = dat, 4, ref.P = P.pca.ref, 
                                   k_ho = 20, rm_pctges = c(1,5,10,seq(20,80,by=20)))
  
  autoencoder_remcells <- vae_removecells(data = dat, 4,
                                          ref.P = P.ae.ref,
                                          ho.part = pca_remcells$ho,
                                          k_ho = 20,
                                          rm_pctges = c(1,5,10,seq(20,80,by=20)))
  save(list = c("pca_remcells", "autoencoder_remcells"),file="MissingData_models.RData")
}
pca_remcells$para_test$Method <- "PCA"
autoencoder_remcells$para_test$Method <- "AutoEncoder"
mdi.exp.data <- rbind(pca_remcells$para_test, autoencoder_remcells$para_test)
lsd.results <- lsdAnalysis(mdi.exp.data, "Method", "MDpctge")
mdi.plots <- ggpubr::ggarrange(plotlist = lsd.results$l.plots, ncol = 2, nrow = 4,
                  font.label = list(size = 3, color = "black", face = "bold", family = NULL),
                  common.legend = TRUE)
ggpubr::annotate_figure(mdi.plots[[2]], top = ggpubr::text_grob("Experiment MD (LSD interval)",
                                       color = "black", face = "bold", size = 12))


## ----transform, results="hide", echo=FALSE,out.width="50%",fig.asp=0.9--------------------------------------------
if(file.exists("../RowsPctge_models.RData")){
  load("../RowsPctge_models.RData")
} else {
  pca_transvars <- vpca_rowpctge(data = dat, 4, ref.P = P.pca.ref, 
                                 k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
  
  autoencoder_transvars <- vae_rowpctge(data = dat, 4, 
                                        ref.P = P.ae.ref,
                                        ho.part = pca_transvars$ho,
                                        k_ho = 20, 
                                        rm_pctges = c(5,10,seq(20,80,by=20)))
  save(list = c("pca_transvars", "autoencoder_transvars"),file="RowsPctge_models.RData")
}
pca_transvars$para_test$Method <- "PCA"
autoencoder_transvars$para_test$Method <- "AutoEncoder"
exp2.data <- rbind(pca_transvars$para_test, autoencoder_transvars$para_test)
exp2.lsd.results <- lsdAnalysis(exp2.data, "Method", hier = F)
exp2.plots <- ggpubr::ggarrange(plotlist = exp2.lsd.results$l.plots, ncol = 2, nrow = 4,
                  font.label = list(size = 3, color = "black", face = "bold", family = NULL),
                  common.legend = TRUE)
ggpubr::annotate_figure(exp2.plots, top = ggpubr::text_grob("Experiment transformation to pctage (LSD interval)", color = "black", face = "bold", size = 12))


## ----coxtransforn, results='hide', echo=FALSE, message=FALSE------------------------------------------------------
library(AID)
# load("ref_models.RData")
if(file.exists("../CoxTrans_models.RData")){
  load("../CoxTrans_models.RData")
} else {
  pca_transvarscox <- vpca_transcols(data = dat, 4, ref.P = P.pca.ref, 
                               k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
  autoencoder_transvarscox <- vae_transcols(data = dat, 4, 
                                            ref.P = P.ae.ref, ho.part = pca_transvarscox$ho,
                                            k_ho = 20, rm_pctges = c(5,10,seq(20,80,by=20)))
  save(list = c("pca_transvarscox", "autoencoder_transvarscox"),file="CoxTrans_models.RData")
}
# pca_transvarscox$para_test$Method <- "PCA"
autoencoder_transvarscox$para_test$Method <- "AutoEncoder"
exp3.data <- rbind(pca_transvarscox$para_test[,-c(1,2)], autoencoder_transvarscox$para_test)
exp3.lsd.results <- lsdAnalysis(exp3.data, "Method", "Coltrans")
exp3.plots <- ggpubr::ggarrange(plotlist = exp3.lsd.results$l.plots, ncol = 2, nrow = 4,
                  font.label = list(size = 3, color = "black", face = "bold", family = NULL),
                  common.legend = TRUE)
ggpubr::annotate_figure(exp3.mdi.plots[[2]], top = ggpubr::text_grob("Experiment BoxCox Transform. (LSD interval)",
                                       color = "black", face = "bold", size = 12))


## ----remrows, results="hide",out.width="50%",fig.asp=0.9----------------------------------------------------------
if(file.exists("../RemRows_models.RData")){
  load("../RemRows_models.RData")
} else {
  pca_remrows <- vpca_removerows(data = dat, 4, ref.P = P.pca.ref, k_ho = 20, 
                                 rm_pctges = c(2,5,10,seq(20,80,by=20)))
  autoencoder_remrows <- vautoencoder_removerows(data = dat, 4, ref.P = P.ae.ref,k_ho = 20, 
                                                 ho.part = pca_remrows$ho,
                                                 rm_pctges = c(2,5,10,seq(20,80,by=20)))
  save(list = c("pca_remrows", "autoencoder_remrows"),file="RemRows_models.RData")
}
pca_remrows$para_test$Method <- "PCA"
autoencoder_remrows$para_test$Method <- "AutoEncoder"
exp4.data <- rbind(pca_remrows$para_test[,-c(1,2)], autoencoder_remrows$para_test)
exp4.lsd.results <- lsdAnalysis(exp4.data, "Method", "RWoutpctge")
exp4.mdi.plots <- ggpubr::ggarrange(plotlist = exp4.lsd.results$l.plots, ncol = 2, nrow = 4,
                  font.label = list(size = 3, color = "black", face = "bold", family = NULL),
                  common.legend = TRUE)
ggpubr::annotate_figure(exp4.mdi.plots[[2]], top = ggpubr::text_grob("Experiment Rows reduction (LSD interval)",
                                       color = "black", face = "bold", size = 12))


## ----remvars, results = "hide"------------------------------------------------------------------------------------



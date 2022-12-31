library(tidyverse)
library(lubridate)
library(corrplot) 
#install.packages("devtools")
#library(devtools) 
#install_github("vqv/ggbiplot2")
#library(ggbiplot2)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

train_data_noNAs <- readRDS(path)

#remover colunas com valores não numéricos
ogi_data_ft <- train_data_noNAs[ , unlist(lapply(train_data_noNAs, is.numeric))]
ogi_data_ft <- ogi_data_ft[, -c(11,12)]

#Principal Component Analysis (PCA)
ogi_pca <- prcomp(ogi_data_ft, scale = TRUE, center=TRUE)
ogi_pca
#ggbiplot2(ogi_pca, labels=rownames(ogi_data_ft))

ogi_corr <- cor(ogi_data_ft)

corrplot(ogi_corr, type="lower",is.corr = FALSE, method="number", number.cex = 0.5,diag=FALSE) 
corrplot.mixed(ogi_corr, lower = "circle", upper = "number", number.cex = 0.5, tl.col = "black", tl.cex = 0.5)

ogi_corr1 <- cor.mtest(ogi_data_ft, conf.level=0.95)
corrplot(ogi_corr, p.mat = ogi_corr1$p, type="lower", diag=FALSE, sig.level = 0.05, insig="blank")

corrplot(cor(ogi_corr), method = "circle")

corrplot(cor(ogi_corr), method = "number")
#############################
####CART TREES#########
#############################
library(tidyverse)
library(tidymodels)

path <- paste( getwd(), "/Rdata/Train_Data_noNa.rds",sep = "")

fire_Train_Data <- readRDS(path)

path <- paste( getwd(), "/Rdata/Test_Data_noNa.rds",sep = "")

fire_Test_Data <- readRDS(path)
fire_Test_Data$intentional_cause = NA

model_rt <- decision_tree(mode="regression", engine="rpart")
rt_fit <- model_rt %>% fit(intentional_cause ~ ., data = fire_Train_Data)
library(rpart.plot)
# to extract it from the parsnip
rt_fit %>% extract_fit_engine() %>% rpart.plot(roundint=FALSE)

library(vip) 
vip(rt_fit)

fire_tree <- rt_fit %>% extract_fit_engine() 
fire_tree$variable.importance
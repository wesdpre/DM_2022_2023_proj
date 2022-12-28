library(tidyverse)
library(lubridate)
library(corrplot) 
#install.packages("devtools")
#library(devtools) 
#install_github("vqv/ggbiplot2")
#library(ggbiplot2)

#remover colunas com valores não numéricos
numeric_data <- test_data[ , unlist(lapply(dados, is.numeric))]

#remover colunas WindkmhGust, Precmm, TotClOct, lowClOct, VisKm pois têm demasiados NA's (> 10% do conjunto de dados)
ogi_data_ft <- numeric_data[, -c(1,5,8,6,7,9,11,13,14)]

#remover todos os valores NA
y = c("TemperatureCAvg", "TemperatureCMax", "TemperatureCMin", "TdAvgC", "HrAvg", "WindkmhInt", "PresslevHp")
vars <- "y"
ogimet_data_no_nas <- drop_na(ogi_data_ft, any_of(y))

ogi_corr <- cor(ogimet_data_no_nas)

corrplot(ogi_corr, type="lower",is.corr = FALSE, method="number", number.cex = 0.5,diag=FALSE) 
corrplot.mixed(ogi_corr, lower = "circle", upper = "number", number.cex = 0.5, tl.col = "black", tl.cex = 0.5)

ogi_corr1 <- cor.mtest(ogimet_data_no_nas, conf.level=0.95)
corrplot(ogi_corr, p.mat = ogi_corr1$p, type="lower", diag=FALSE, sig.level = 0.05, insig="blank")

#Principal Component Analysis (PCA)
ogi_pca <- prcomp(ogimet_data_no_nas, scale = TRUE, center=TRUE)
ogi_pca
#ggbiplot2(ogi_pca, labels=rownames(ogimet_data_no_nas))

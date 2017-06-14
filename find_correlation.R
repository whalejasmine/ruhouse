setwd("~/Documents/ruhouse")
library(readr)
###library(data.table)
library(caret)
library(corrplot)

train<- read_csv("./data/output/train_feature_corr.csv")
macro <- read_csv("./data/output/macro_corr.csv")
train.scale<- scale(train,center=TRUE,scale=TRUE);
#scale all the features (from feature 2 bacause feature 1 is the predictor output)
corMatMy <- cor(train.scale)
#visualize the matrix, clustering features by correlation index.
high_corr <- findCorrelation(corMatMy,cutoff = 0.9,verbose = FALSE)
train_re_corr <- train[,-high_corr]
dim(train_re_corr)

write_csv(train_re_corr,"./data/output/train_feature_multicoliear.csv")
#macro
macro.scale <-scale(macro,center = TRUE,scale = TRUE)
corr_macro <- cor(macro.scale)
highly_corr_macro <- findCorrelation(corr_macro,cutoff = 0.7,verbose = FALSE)
macro_re_corr <-macro[,-highly_corr_macro]
dim(macro_re_corr)
write_csv(macro_re_corr,"./data/output/macro_re_corr.csv")
#trainwithmacros
train_with_macros <- read_csv("./data/output/train_with_macros.csv")
dim(train_with_macros)
train_with_macros.scale <-scale(train_with_macros,center = TRUE,scale = TRUE)
corr_train_with_macros <- cor(train_with_macros.scale)
highly_corr_train_with_macros <- findCorrelation(corr_train_with_macros,cutoff = 0.8,verbose = FALSE)
train_with_macros_re_corr <-train_with_macros[,-highly_corr_train_with_macros]
dim(train_with_macros_re_corr)
write_csv(train_with_macros_re_corr,"./data/output/train_with_macros_re_corr.csv")

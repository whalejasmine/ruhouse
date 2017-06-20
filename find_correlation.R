setwd("~/Documents/ruhouse")
library(readr)
###library(data.table)
library(caret)
library(corrplot)

train<- read_csv("./data/output/train_feature_corr.csv")
#train<-read_csv("./data/input/train.csv")
train<- sapply(train, as.numeric)
macro <- read_csv("./data/output/macro_corr.csv")
train.scale<- scale(train,center=TRUE,scale=TRUE);
#scale all the features (from feature 2 bacause feature 1 is the predictor output)
corMatMy <- cor(train.scale)
#visualize the matrix, clustering features by correlation index.
high_corr <- findCorrelation(corMatMy,cutoff = 0.9,verbose = FALSE)
corrpred <- names(train)[high_corr]
train_re_corr <- train[,high_corr]
dim(train_re_corr)

write_csv(train_re_corr,"./data/output/train_corr_drop.csv")
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


#VIF function

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}
dtrain<- read_csv("./data/output/x_train_vif.csv")
dtrain$id <- dtrain$timestamp <- dtrain$price_doc <- NULL
macor_vif<- read_csv("./data/output/macro_vif.csv")
col<- vif_func(in_frame=dtrain,thresh=5,trace=T)
cat(paste(shQuote(col, type="cmd"), collapse=", "))
col_vif = replace(col, col =="X16_29_male", "16_29_male")
vars_vif_drop <-names(train) %in% col_vif
train_vif_feature <-train[!vars_vif_drop]
write_csv(train_vif_feature,"./data/output/train_vif_fetrain.csv")

col_macro<- vif_func(in_frame=macro,thresh=5,trace=T)
cat(paste(shQuote(col_macro, type="cmd"), collapse=", "))

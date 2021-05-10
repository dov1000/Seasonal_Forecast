
setwd("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/")

#source('Data.R')

#rm(list=ls())

#invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
lev <- c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1")
lev_mes <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1","1.1","1.2")
lev_ob <- c("1","2","3","4","5","6","7","8","9","10")
atras <-0

library('plyr')
library('xgboost')
library('caret')
library("tidyverse")
library("e1071")
library('MASS')
library('mda')
library('keras')

#modelos Reventazon
xgboost_reventazon <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Reventazon/xgboost")
knn_reventazon <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Reventazon/knn")
nbayes_reventazon <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Reventazon/NBayes")
svm_reventazon <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Reventazon/SVM")
md_reventazon <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Reventazon/MixDiscAna")


#Reventazon


data_Reventazon <- read.csv('Reventazon.csv') %>% 
  as_tibble()


deciles <- quantile(data_Reventazon$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Reventazon %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

#Cuando vamos a entrenar quitamos el comentario en el drop_na()

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()

n <- nrow(data_model)-atras

data_model <- data_model[1:n,]

ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_reventazon <- predict(xgboost_reventazon, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_reventazon <- stats::predict(svm_reventazon, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_reventazon_svm <- attr(res2_reventazon,'probabilities')

#knn
res3_reventazon <- predict(knn_reventazon, newdata = data_knn, reshape = TRUE)

#nbayes
res4_reventazon <- predict(nbayes_reventazon, data_model, type = "raw",reshape = TRUE)

#md
res5_reventazon <- predict(md_reventazon, data_model)



pred_mod_1_reventazon <- max.col(res1_reventazon,ties.method = "last")
pred_mod_2_reventazon <- as.numeric(res2_reventazon)
pred_mod_3_reventazon <- res3_reventazon <- as.numeric(res3_reventazon)-1
pred_mod_4_reventazon <- max.col(res4_reventazon,ties.method = "last")
pred_mod_5_reventazon <- as.numeric(res5_reventazon)

modelos_reventazon <- tibble(.rows = length(pred_mod_1_reventazon))
modelos_reventazon$xgb <- pred_mod_1_reventazon/10
modelos_reventazon$svm <- pred_mod_2_reventazon/10
modelos_reventazon$knn <- pred_mod_3_reventazon/10
modelos_reventazon$nbayes <- pred_mod_4_reventazon/10
modelos_reventazon$mda <- pred_mod_5_reventazon/10
modelos_reventazon$meses <- data_model$Mes/10
modelos_reventazon$observado <- as.numeric(data_model$ind2)


#nn
#nn_reventazon <- load_model_hdf5('model_nns_ensamble_reventazon.h5')
#final_reventazon <- nn_reventazon %>% predict_proba(as.matrix(modelos_reventazon[,1:6]))



library('randomForest')

datos <- modelos_reventazon %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:200,]
test = datos[201:232,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_reventazon")



#Ventanas
#modelos Ventanas
xgboost_Ventanas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Ventanas/xgboost")
knn_Ventanas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Ventanas/knn")
nbayes_Ventanas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Ventanas/NBayes")
svm_Ventanas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Ventanas/SVM")
md_Ventanas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Ventanas/MixDiscAna")


data_Ventanas <- read.csv('Ventanas.csv') %>% 
  as_tibble()


deciles <- quantile(data_Ventanas$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Ventanas %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()

atras = 0

n <- nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))


#xgboost
res1_Ventanas <- predict(xgboost_Ventanas$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Ventanas <- stats::predict(svm_Ventanas, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Ventanas_svm <- attr(res2_Ventanas,'probabilities')

#knn
res3_Ventanas <- predict(knn_Ventanas, newdata = data_knn)

#nbayes
res4_Ventanas <- predict(nbayes_Ventanas, data_model, type = 'raw', reshape = TRUE)

#md
res5_Ventanas <- predict(md_Ventanas, data_model)

#nn_Ventanas <- load_model_hdf5('model_nns_ensamble_Ventanas.h5')

pred_mod_1_Ventanas <- max.col(res1_Ventanas,ties.method = "last")
pred_mod_2_Ventanas <- as.numeric(res2_Ventanas)
pred_mod_3_Ventanas <- res3_Ventanas <- as.numeric(res3_Ventanas)-1
pred_mod_4_Ventanas <- max.col(res4_Ventanas,ties.method = "last")
pred_mod_5_Ventanas <- as.numeric(res5_Ventanas)

modelos_Ventanas <- tibble(.rows = length(pred_mod_5_Ventanas))
modelos_Ventanas$xgb <- pred_mod_1_Ventanas/10
modelos_Ventanas$svm <- pred_mod_2_Ventanas/10
modelos_Ventanas$knn <- pred_mod_3_Ventanas/10
modelos_Ventanas$nbayes <- pred_mod_4_Ventanas/10
modelos_Ventanas$mda <- pred_mod_5_Ventanas/10
modelos_Ventanas$meses <- tail(data_model,1)$Mes/10
modelos_Ventanas$observado <- as.numeric(data_model$ind2)

#final_Ventanas<- nn_Ventanas %>% predict_proba(as.matrix(modelos_Ventanas[,1:6]))



datos <- modelos_Ventanas %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:544,]
test = datos[501:545,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Ventanas")



#Angostura



#modelos Angostura
xgboost_Angostura <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Angostura/xgboost")
knn_Angostura <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Angostura/knn")
nbayes_Angostura <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Angostura/NBayes")
svm_Angostura <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Angostura/SVM")
md_Angostura <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Angostura/MixDiscAna")

data_Angostura <- read.csv('Angostura.csv') %>% 
  as_tibble()


deciles <- quantile(data_Angostura$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Angostura %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Angostura <- predict(xgboost_Angostura$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Angostura <- stats::predict(svm_Angostura, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Angostura_svm <- attr(res2_Angostura,'probabilities')

#knn
res3_Angostura <- predict(knn_Angostura, newdata = data_knn)

#nbayes
res4_Angostura <- predict(nbayes_Angostura, data_model, type = 'raw', reshape= TRUE)

#md
res5_Angostura <- predict(md_Angostura, data_model)

#nn_Angostura <- load_model_hdf5('model_nns_ensamble_Angostura.h5')

pred_mod_1_Angostura <- max.col(res1_Angostura,ties.method = "last")
pred_mod_2_Angostura <- as.numeric(res2_Angostura)
pred_mod_3_Angostura <- res3_Angostura <- as.numeric(res3_Angostura)-1
pred_mod_4_Angostura <- max.col(res4_Angostura,ties.method = "last")
pred_mod_5_Angostura <- as.numeric(res5_Angostura)

modelos_Angostura <- tibble(.rows = length(pred_mod_5_Angostura))
modelos_Angostura$xgb <- pred_mod_1_Angostura/10
modelos_Angostura$svm <- pred_mod_2_Angostura/10
modelos_Angostura$knn <- pred_mod_3_Angostura/10
modelos_Angostura$nbayes <- pred_mod_4_Angostura/10
modelos_Angostura$mda <- pred_mod_5_Angostura/10
modelos_Angostura$meses <- tail(data_model,1)$Mes/10
modelos_Angostura$observado <- as.numeric(data_model$ind2)

#final_Angostura <- nn_Angostura %>% predict_proba(as.matrix(modelos_Angostura[,1:6]))

library('randomForest')

datos <- modelos_Angostura %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:532,]
test = datos[501:532,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Angostura")




#Arenal


#modelos Arenal
xgboost_Arenal <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/xgboost")
knn_Arenal <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/knn")
nbayes_Arenal <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/NBayes")
svm_Arenal <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/SVM")
md_Arenal <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/MixDiscAna")

data_Arenal <- read.csv('Arenal.csv') %>% 
  as_tibble()


deciles <- quantile(data_Arenal$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Arenal %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Arenal <- predict(xgboost_Arenal$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Arenal <- stats::predict(svm_Arenal, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Arenal_svm <- attr(res2_Arenal,'probabilities')

#knn
res3_Arenal <- predict(knn_Arenal, newdata = data_knn)

#nbayes
res4_Arenal <- predict(nbayes_Arenal, data_model, type = 'raw', reshape= TRUE)

#md
res5_Arenal <- predict(md_Arenal, data_model)

#nn_Arenal <- load_model_hdf5('model_nns_ensamble_Arenal.h5')

pred_mod_1_Arenal <- max.col(res1_Arenal,ties.method = "last")
pred_mod_2_Arenal <- as.numeric(res2_Arenal)
pred_mod_3_Arenal <- res3_Arenal <- as.numeric(res3_Arenal)-1
pred_mod_4_Arenal <- max.col(res4_Arenal,ties.method = "last")
pred_mod_5_Arenal <- as.numeric(res5_Arenal)

modelos_Arenal <- tibble(.rows = length(pred_mod_5_Arenal))
modelos_Arenal$xgb <- pred_mod_1_Arenal/10
modelos_Arenal$svm <- pred_mod_2_Arenal/10
modelos_Arenal$knn <- pred_mod_3_Arenal/10
modelos_Arenal$nbayes <- pred_mod_4_Arenal/10
modelos_Arenal$mda <- pred_mod_5_Arenal/10
modelos_Arenal$meses <- tail(data_model,1)$Mes/10
modelos_Arenal$observado <- as.numeric(data_model$ind2)

#final_Arenal <- nn_Arenal %>% predict_proba(as.matrix(modelos_Arenal[,1:6]))

library('randomForest')

datos <- modelos_Arenal %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:508,]
test = datos[490:508,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Arenal")


#Cachi


#modelos Cachi
xgboost_Cachi <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cachi/xgboost")
knn_Cachi <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cachi/knn")
nbayes_Cachi <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cachi/NBayes")
svm_Cachi <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cachi/SVM")
md_Cachi <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cachi/MixDiscAna")

data_Cachi <- read.csv('Cachi.csv') %>% 
  as_tibble()


deciles <- quantile(data_Cachi$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Cachi %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Cachi <- predict(xgboost_Cachi$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Cachi <- stats::predict(svm_Cachi, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Cachi_svm <- attr(res2_Cachi,'probabilities')

#knn
res3_Cachi <- predict(knn_Cachi, newdata = data_knn)

#nbayes
res4_Cachi <- predict(nbayes_Cachi, data_model, type = 'raw', reshape= TRUE)

#md
res5_Cachi <- predict(md_Cachi, data_model)

#nn_Cachi <- load_model_hdf5('model_nns_ensamble_Cachi.h5')

pred_mod_1_Cachi <- max.col(res1_Cachi,ties.method = "last")
pred_mod_2_Cachi <- as.numeric(res2_Cachi)
pred_mod_3_Cachi <- res3_Cachi <- as.numeric(res3_Cachi)-1
pred_mod_4_Cachi <- max.col(res4_Cachi,ties.method = "last")
pred_mod_5_Cachi <- as.numeric(res5_Cachi)

modelos_Cachi <- tibble(.rows = length(pred_mod_5_Cachi))
modelos_Cachi$xgb <- pred_mod_1_Cachi/10
modelos_Cachi$svm <- pred_mod_2_Cachi/10
modelos_Cachi$knn <- pred_mod_3_Cachi/10
modelos_Cachi$nbayes <- pred_mod_4_Cachi/10
modelos_Cachi$mda <- pred_mod_5_Cachi/10
modelos_Cachi$meses <- tail(data_model,1)$Mes/10
modelos_Cachi$observado <- as.numeric(data_model$ind2)

#final_Cachi <- nn_Cachi %>% predict_proba(as.matrix(modelos_Cachi[,1:6]))

#library('randomForest')

datos <- modelos_Cachi %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:544,]
test = datos[501:544,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Cachi")


#Cariblanco


#modelos Cariblanco
xgboost_Cariblanco <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cariblanco/xgboost")
knn_Cariblanco <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cariblanco/knn")
nbayes_Cariblanco <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cariblanco/NBayes")
svm_Cariblanco <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cariblanco/SVM")
md_Cariblanco <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Cariblanco/MixDiscAna")

data_Cariblanco <- read.csv('Cariblanco.csv') %>% 
  as_tibble()


deciles <- quantile(data_Cariblanco$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Cariblanco %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Cariblanco <- predict(xgboost_Cariblanco$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Cariblanco <- stats::predict(svm_Cariblanco, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Cariblanco_svm <- attr(res2_Cariblanco,'probabilities')

#knn
res3_Cariblanco <- predict(knn_Cariblanco, newdata = data_knn)

#nbayes
res4_Cariblanco <- predict(nbayes_Cariblanco, data_model, type = 'raw', reshape= TRUE)

#md
res5_Cariblanco <- predict(md_Cariblanco, data_model)

#nn_Cariblanco <- load_model_hdf5('model_nns_ensamble_Cariblanco.h5')

pred_mod_1_Cariblanco <- max.col(res1_Cariblanco,ties.method = "last")
pred_mod_2_Cariblanco <- as.numeric(res2_Cariblanco)
pred_mod_3_Cariblanco <- res3_Cariblanco <- as.numeric(res3_Cariblanco)-1
pred_mod_4_Cariblanco <- max.col(res4_Cariblanco,ties.method = "last")
pred_mod_5_Cariblanco <- as.numeric(res5_Cariblanco)

modelos_Cariblanco <- tibble(.rows = length(pred_mod_5_Cariblanco))
modelos_Cariblanco$xgb <- pred_mod_1_Cariblanco/10
modelos_Cariblanco$svm <- pred_mod_2_Cariblanco/10
modelos_Cariblanco$knn <- pred_mod_3_Cariblanco/10
modelos_Cariblanco$nbayes <- pred_mod_4_Cariblanco/10
modelos_Cariblanco$mda <- pred_mod_5_Cariblanco/10
modelos_Cariblanco$meses <- tail(data_model,1)$Mes/10
modelos_Cariblanco$observado <- as.numeric(data_model$ind2)

#final_Cariblanco <- nn_Cariblanco %>% predict_proba(as.matrix(modelos_Cariblanco[,1:6]))

#library('randomForest')

datos <- modelos_Cariblanco %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:448,]
test = datos[501:532,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Cariblanco")


#Penas


#modelos Penas
xgboost_Penas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Penas/xgboost")
knn_Penas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Penas/knn")
nbayes_Penas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Penas/NBayes")
svm_Penas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Penas/SVM")
md_Penas <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Penas/MixDiscAna")

data_Penas <- read.csv('Penas.csv') %>% 
  as_tibble()


deciles <- quantile(data_Penas$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Penas %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Penas <- predict(xgboost_Penas$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Penas <- stats::predict(svm_Penas, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Penas_svm <- attr(res2_Penas,'probabilities')

#knn
res3_Penas <- predict(knn_Penas, newdata = data_knn)

#nbayes
res4_Penas <- predict(nbayes_Penas, data_model, type = 'raw', reshape= TRUE)

#md
res5_Penas <- predict(md_Penas, data_model)

#nn_Penas <- load_model_hdf5('model_nns_ensamble_Penas.h5')

pred_mod_1_Penas <- max.col(res1_Penas,ties.method = "last")
pred_mod_2_Penas <- as.numeric(res2_Penas)
pred_mod_3_Penas <- res3_Penas <- as.numeric(res3_Penas)-1
pred_mod_4_Penas <- max.col(res4_Penas,ties.method = "last")
pred_mod_5_Penas <- as.numeric(res5_Penas)

modelos_Penas <- tibble(.rows = length(pred_mod_5_Penas))
modelos_Penas$xgb <- pred_mod_1_Penas/10
modelos_Penas$svm <- pred_mod_2_Penas/10
modelos_Penas$knn <- pred_mod_3_Penas/10
modelos_Penas$nbayes <- pred_mod_4_Penas/10
modelos_Penas$mda <- pred_mod_5_Penas/10
modelos_Penas$meses <- tail(data_model,1)$Mes/10
modelos_Penas$observado <- as.numeric(data_model$ind2)

#final_Penas <- nn_Penas %>% predict_proba(as.matrix(modelos_Penas[,1:6]))

#library('randomForest')

datos <- modelos_Penas %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:436,]
test = datos[501:532,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Penas")



#Pirris


#modelos Pirris
xgboost_Pirris <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Pirris/xgboost")
knn_Pirris <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Pirris/knn")
nbayes_Pirris <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Pirris/NBayes")
svm_Pirris <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Pirris/SVM")
md_Pirris <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Pirris/MixDiscAna")

data_Pirris <- read.csv('Pirris.csv') %>% 
  as_tibble()


deciles <- quantile(data_Pirris$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Pirris %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Pirris <- predict(xgboost_Pirris$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Pirris <- stats::predict(svm_Pirris, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Pirris_svm <- attr(res2_Pirris,'probabilities')

#knn
res3_Pirris <- predict(knn_Pirris, newdata = data_knn)

#nbayes
res4_Pirris <- predict(nbayes_Pirris, data_model, type = 'raw', reshape= TRUE)

#md
res5_Pirris <- predict(md_Pirris, data_model)

#nn_Pirris <- load_model_hdf5('model_nns_ensamble_Pirris.h5')

pred_mod_1_Pirris <- max.col(res1_Pirris,ties.method = "last")
pred_mod_2_Pirris <- as.numeric(res2_Pirris)
pred_mod_3_Pirris <- res3_Pirris <- as.numeric(res3_Pirris)-1
pred_mod_4_Pirris <- max.col(res4_Pirris,ties.method = "last")
pred_mod_5_Pirris <- as.numeric(res5_Pirris)

modelos_Pirris <- tibble(.rows = length(pred_mod_5_Pirris))
modelos_Pirris$xgb <- pred_mod_1_Pirris/10
modelos_Pirris$svm <- pred_mod_2_Pirris/10
modelos_Pirris$knn <- pred_mod_3_Pirris/10
modelos_Pirris$nbayes <- pred_mod_4_Pirris/10
modelos_Pirris$mda <- pred_mod_5_Pirris/10
modelos_Pirris$meses <- tail(data_model,1)$Mes/10
modelos_Pirris$observado <- as.numeric(data_model$ind2)

#final_Pirris <- nn_Pirris %>% predict_proba(as.matrix(modelos_Pirris[,1:6]))

#library('randomForest')

datos <- modelos_Pirris %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:356,]
test = datos[501:532,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Pirris")



#RioMacho


#modelos RioMacho
xgboost_RioMacho <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/RioMacho/xgboost")
knn_RioMacho <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/RioMacho/knn")
nbayes_RioMacho <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/RioMacho/NBayes")
svm_RioMacho <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/RioMacho/SVM")
md_RioMacho <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/RioMacho/MixDiscAna")

data_RioMacho <- read.csv('RioMacho.csv') %>% 
  as_tibble()


deciles <- quantile(data_RioMacho$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_RioMacho %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_RioMacho <- predict(xgboost_RioMacho$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_RioMacho <- stats::predict(svm_RioMacho, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_RioMacho_svm <- attr(res2_RioMacho,'probabilities')

#knn
res3_RioMacho <- predict(knn_RioMacho, newdata = data_knn)

#nbayes
res4_RioMacho <- predict(nbayes_RioMacho, data_model, type = 'raw', reshape= TRUE)

#md
res5_RioMacho <- predict(md_RioMacho, data_model)

#nn_RioMacho <- load_model_hdf5('model_nns_ensamble_RioMacho.h5')

pred_mod_1_RioMacho <- max.col(res1_RioMacho,ties.method = "last")
pred_mod_2_RioMacho <- as.numeric(res2_RioMacho)
pred_mod_3_RioMacho <- res3_RioMacho <- as.numeric(res3_RioMacho)-1
pred_mod_4_RioMacho <- max.col(res4_RioMacho,ties.method = "last")
pred_mod_5_RioMacho <- as.numeric(res5_RioMacho)

modelos_RioMacho <- tibble(.rows = length(pred_mod_5_RioMacho))
modelos_RioMacho$xgb <- pred_mod_1_RioMacho/10
modelos_RioMacho$svm <- pred_mod_2_RioMacho/10
modelos_RioMacho$knn <- pred_mod_3_RioMacho/10
modelos_RioMacho$nbayes <- pred_mod_4_RioMacho/10
modelos_RioMacho$mda <- pred_mod_5_RioMacho/10
modelos_RioMacho$meses <- tail(data_model,1)$Mes/10
modelos_RioMacho$observado <- as.numeric(data_model$ind2)

#final_RioMacho <- nn_RioMacho %>% predict_proba(as.matrix(modelos_RioMacho[,1:6]))

#library('randomForest')

datos <- modelos_RioMacho %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:472,]
test = datos[501:532,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_RioMacho")




#Toro1


#modelos Toro1
xgboost_Toro1 <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Toro1/xgboost")
knn_Toro1 <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Toro1/knn")
nbayes_Toro1 <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Toro1/NBayes")
svm_Toro1 <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Toro1/SVM")
md_Toro1 <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Toro1/MixDiscAna")

data_Toro1 <- read.csv('Toro1.csv') %>% 
  as_tibble()


deciles <- quantile(data_Toro1$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Toro1 %>% 
  dplyr::select(3:16) 

#Creamos nuevas categorias de indices desplazados
data_model <- data_model %>% 
  mutate(mei_1 = lag(data_model$mei,1),
         tna_1 = lag(data_model$tna,1),
         tni_1 = lag(data_model$tni,1),
         nao_1 = lag(data_model$nao,1),
         ao_1 = lag(data_model$ao,1)) %>% 
  drop_na()  

data_model <- data_model %>% 
  mutate(ind = case_when(lluvia < deciles[1] ~ 0,
                         between(lluvia,deciles[1],deciles[2])~ 1,
                         between(lluvia,deciles[2],deciles[3]) ~ 2,
                         between(lluvia,deciles[3],deciles[4]) ~ 3,
                         between(lluvia,deciles[4],deciles[5]) ~ 4,
                         between(lluvia,deciles[5],deciles[6]) ~ 5,
                         between(lluvia,deciles[6],deciles[7]) ~ 6,
                         between(lluvia,deciles[7],deciles[8]) ~ 7,
                         between(lluvia,deciles[8],deciles[9]) ~ 8,
                         lluvia > deciles[9] ~9 )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()


atras <- 0

n<-nrow(data_model)-atras
data_model <- data_model[1:n,]
ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



data_knn <- data_model %>% 
  mutate(ind3 = case_when(ind2 ==0 ~ 'n0',
                          ind2 ==1 ~ 'n1',
                          ind2 ==2 ~ 'n2',
                          ind2 ==3 ~ 'n3',
                          ind2 ==4 ~ 'n4',
                          ind2 ==5 ~ 'n5',
                          ind2 ==6 ~ 'n6',
                          ind2 ==7 ~ 'n7',
                          ind2 ==8 ~ 'n8',
                          ind2 ==9 ~ 'n9')) %>% 
  mutate(ind = case_when(ind ==0 ~ 'n0',
                         ind ==1 ~ 'n1',
                         ind ==2 ~ 'n2',
                         ind ==3 ~ 'n3',
                         ind ==4 ~ 'n4',
                         ind ==5 ~ 'n5',
                         ind ==6 ~ 'n6',
                         ind ==7 ~ 'n7',
                         ind ==8 ~ 'n8',
                         ind ==9 ~ 'n9'))



data_knn <- dplyr::select(data_knn,-c(ind2))

data_knn <- data_knn %>% rename(ind2 = ind3)

data_knn<- data_knn %>%  mutate(ind2=factor(ind2)) %>%  mutate(ind = factor(ind))



#xgboost
res1_Toro1 <- predict(xgboost_Toro1$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)

#svm
res2_Toro1 <- stats::predict(svm_Toro1, newdata = data_model[,1:20], probability = TRUE, reshape = TRUE)
probs_Toro1_svm <- attr(res2_Toro1,'probabilities')

#knn
res3_Toro1 <- predict(knn_Toro1, newdata = data_knn)

#nbayes
res4_Toro1 <- predict(nbayes_Toro1, data_model, type = 'raw', reshape= TRUE)

#md
res5_Toro1 <- predict(md_Toro1, data_model)

#nn_Toro1 <- load_model_hdf5('model_nns_ensamble_Toro1.h5')

pred_mod_1_Toro1 <- max.col(res1_Toro1,ties.method = "last")
pred_mod_2_Toro1 <- as.numeric(res2_Toro1)
pred_mod_3_Toro1 <- res3_Toro1 <- as.numeric(res3_Toro1)-1
pred_mod_4_Toro1 <- max.col(res4_Toro1,ties.method = "last")
pred_mod_5_Toro1 <- as.numeric(res5_Toro1)

modelos_Toro1 <- tibble(.rows = length(pred_mod_5_Toro1))
modelos_Toro1$xgb <- pred_mod_1_Toro1/10
modelos_Toro1$svm <- pred_mod_2_Toro1/10
modelos_Toro1$knn <- pred_mod_3_Toro1/10
modelos_Toro1$nbayes <- pred_mod_4_Toro1/10
modelos_Toro1$mda <- pred_mod_5_Toro1/10
modelos_Toro1$meses <- tail(data_model,1)$Mes/10
modelos_Toro1$observado <- as.numeric(data_model$ind2)

#final_Toro1 <- nn_Toro1 %>% predict_proba(as.matrix(modelos_Toro1[,1:6]))

#library('randomForest')

datos <- modelos_Toro1 %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

train = datos[1:405,]
test = datos[501:532,]

rf <- randomForest(observado ~., data = train,ntree = 1000)
pred = predict(rf, newdata=test[-7])

saveRDS(rf,file="C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/RandomForest/rf_Toro1")

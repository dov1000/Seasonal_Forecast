#Cuando vamos a entrenar quitamos el comentario en el drop_na()
setwd("D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/")

#source('Data.R')

atras <-0
lev <- c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1")
lev_mes <- c("0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1","1.1","1.2")
lev_ob <- c("1","2","3","4","5","6","7","8","9","10")


library('plyr')
library('xgboost')
library('caret')
library("tidyverse")
library("e1071")
library('MASS')
library('mda')
library('keras')
library('randomForest')
library("writexl")
#modelos Reventaz
xgboost_reventazon <- readRDS("./xgboost_Reventazon")
knn_reventazon <- readRDS("./knn_Reventazon")
nbayes_reventazon <- readRDS("./NBayes_Reventazon")
svm_reventazon <- readRDS("./SVM_Reventazon")
md_reventazon <- readRDS("./MixDiscAna_Reventazon")
rf_reventazon <- readRDS("./rf_reventazon")
#modelos Angostura
xgboost_Angostura <- readRDS("./xgboost_Angostura")
knn_Angostura <- readRDS("./knn_Angostura")
nbayes_Angostura <- readRDS("./NBayes_Angostura")
svm_Angostura <- readRDS("./SVM_Angostura")
md_Angostura <- readRDS("./MixDiscAna_Angostura")
rf_Angostura <- readRDS("./rf_Angostura")
#modelos Arenal
xgboost_Arenal <- readRDS("./xgboost_Arenal")
knn_Arenal <- readRDS("./knn_Arenal")
nbayes_Arenal <- readRDS("./NBayes_Arenal")
svm_Arenal <- readRDS("./SVM_Arenal")
md_Arenal <- readRDS("./MixDiscAna_Arenal")
rf_Arenal <- readRDS("./rf_Arenal")
#modelos Cachi
xgboost_Cachi <- readRDS("./xgboost_Cachi")
knn_Cachi <- readRDS("./knn_Cachi")
nbayes_Cachi <- readRDS("./NBayes_Cachi")
svm_Cachi <- readRDS("./SVM_Cachi")
md_Cachi <- readRDS("./MixDiscAna_Cachi")
rf_Cachi <- readRDS("./rf_Cachi")
#modelos Cariblanco
xgboost_Cariblanco <- readRDS("./xgboost_Cariblanco")
knn_Cariblanco <- readRDS("./knn_Cariblanco")
nbayes_Cariblanco <- readRDS("./NBayes_Cariblanco")
svm_Cariblanco <- readRDS("./SVM_Cariblanco")
md_Cariblanco <- readRDS("./MixDiscAna_Cariblanco")
rf_Cariblanco <- readRDS("./rf_Cariblanco")
#modelos Penas
xgboost_Penas <- readRDS("./xgboost_Penas")
knn_Penas <- readRDS("./knn_Penas")
nbayes_Penas <- readRDS("./NBayes_Penas")
svm_Penas <- readRDS("./SVM_Penas")
md_Penas <- readRDS("./MixDiscAna_Penas")
rf_Penas <- readRDS("./rf_Penas")
#modelos Pirris
xgboost_Pirris <- readRDS("./xgboost_Pirris")
knn_Pirris <- readRDS("./knn_Pirris")
nbayes_Pirris <- readRDS("./NBayes_Pirris")
svm_Pirris <- readRDS("./SVM_Pirris")
md_Pirris <- readRDS("./MixDiscAna_Pirris")
rf_Pirris <- readRDS("./rf_Pirris")
#modelos RioMacho
xgboost_RioMacho <- readRDS("./xgboost_RioMacho")
knn_RioMacho <- readRDS("./knn_RioMacho")
nbayes_RioMacho <- readRDS("./NBayes_RioMacho")
svm_RioMacho <- readRDS("./SVM_RioMacho")
md_RioMacho <- readRDS("./MixDiscAna_RioMacho")
rf_RioMacho <- readRDS("./rf_RioMacho")
#modelos Toro1
xgboost_Toro1 <- readRDS("./xgboost_Toro1")
knn_Toro1 <- readRDS("./knn_Toro1")
nbayes_Toro1 <- readRDS("./NBayes_Toro1")
svm_Toro1 <- readRDS("./SVM_Toro1")
md_Toro1 <- readRDS("./MixDiscAna_Toro1")
rf_Toro1 <- readRDS("./rf_Toro1")
#modelos Ventanas
xgboost_Ventanas <- readRDS("./xgboost_Ventanas")
knn_Ventanas <- readRDS("./knn_Ventanas")
nbayes_Ventanas <- readRDS("./NBayes_Ventanas")
svm_Ventanas <- readRDS("./SVM_Ventanas")
md_Ventanas <- readRDS("./MixDiscAna_Ventanas")
rf_Ventanas <- readRDS("./rf_Ventanas")
#Inicio resultados modelos



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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_reventazon <- predict(xgboost_reventazon, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_reventazon <- stats::predict(svm_reventazon, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_reventazon_svm <- attr(res2_reventazon,'probabilities')

#knn
res3_reventazon <- predict(knn_reventazon, newdata = tail(data_knn,1), reshape = TRUE)

#nbayes
res4_reventazon <- predict(nbayes_reventazon, tail(data_model,1), type = "raw",reshape = TRUE)

#md
res5_reventazon <- predict(md_reventazon, tail(data_model,1))

#nn
#nn_reventazon <- load_model_hdf5('model_nns_ensamble_reventazon.h5')

pred_mod_1_reventazon <- max.col(res1_reventazon,ties.method = "last")
pred_mod_2_reventazon <- as.numeric(res2_reventazon)
pred_mod_3_reventazon <- res3_reventazon <- as.numeric(res3_reventazon)-1
pred_mod_4_reventazon <- max.col(res4_reventazon,ties.method = "last")
pred_mod_5_reventazon <- as.numeric(res5_reventazon)

modelos_reventazon <- tibble(.rows = 1)
modelos_reventazon$xgb <- pred_mod_1_reventazon/10
modelos_reventazon$svm <- pred_mod_2_reventazon/10
modelos_reventazon$knn <- pred_mod_3_reventazon/10
modelos_reventazon$nbayes <- pred_mod_4_reventazon/10
modelos_reventazon$mda <- pred_mod_5_reventazon/10
modelos_reventazon$meses <- tail(data_model,1)$Mes/10
modelos_reventazon$observado <- as.numeric(tail(data_model,1)$ind2)



#final_reventazon <- nn_reventazon %>% predict_proba(as.matrix(modelos_reventazon[,1:6]))


#prueba <- readRDS("xgb_modelos_rev")


#da <- xgb.DMatrix(data = as.matrix(modelos_reventazon[,1:6]),label=as.integer(as.matrix(modelos_reventazon[,7]))-1)

#final2 <- predict(prueba,newdata = da,probability = TRUE , reshape = TRUE)

modelos_reventazon <- modelos_reventazon %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_reventazon = predict(rf_reventazon, newdata=modelos_reventazon[-7],type="prob")

#Ventanas


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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Ventanas <- predict(xgboost_Ventanas$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Ventanas <- stats::predict(svm_Ventanas, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Ventanas_svm <- attr(res2_Ventanas,'probabilities')

#knn
res3_Ventanas <- predict(knn_Ventanas, newdata = tail(data_knn,1))

#nbayes
res4_Ventanas <- predict(nbayes_Ventanas, tail(data_model,1), type = 'raw', reshape = TRUE)

#md
res5_Ventanas <- predict(md_Ventanas, tail(data_model,1))

#nn_Ventanas <- load_model_hdf5('model_nns_ensamble_Ventanas.h5')

pred_mod_1_Ventanas <- max.col(res1_Ventanas,ties.method = "last")
pred_mod_2_Ventanas <- as.numeric(res2_Ventanas)
pred_mod_3_Ventanas <- res3_Ventanas <- as.numeric(res3_Ventanas)-1
pred_mod_4_Ventanas <- max.col(res4_Ventanas,ties.method = "last")
pred_mod_5_Ventanas <- as.numeric(res5_Ventanas)

modelos_Ventanas <- tibble(.rows = 1)
modelos_Ventanas$xgb <- pred_mod_1_Ventanas/10
modelos_Ventanas$svm <- pred_mod_2_Ventanas/10
modelos_Ventanas$knn <- pred_mod_3_Ventanas/10
modelos_Ventanas$nbayes <- pred_mod_4_Ventanas/10
modelos_Ventanas$mda <- pred_mod_5_Ventanas/10
modelos_Ventanas$meses <- tail(data_model,1)$Mes/10
modelos_Ventanas$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Ventanas<- nn_Ventanas %>% predict_proba(as.matrix(modelos_Ventanas[,1:6]))

modelos_Ventanas <- modelos_Ventanas %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))
final_Ventanas = predict(rf_Ventanas, newdata=modelos_Ventanas[-7],type="prob")

#Angostura

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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Angostura <- predict(xgboost_Angostura$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Angostura <- stats::predict(svm_Angostura, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Angostura_svm <- attr(res2_Angostura,'probabilities')

#knn
res3_Angostura <- predict(knn_Angostura, newdata = tail(data_knn,1))

#nbayes
res4_Angostura <- predict(nbayes_Angostura, tail(data_model,1), type = 'raw', reshape= TRUE)

#md
res5_Angostura <- predict(md_Angostura, tail(data_model,1))

#nn_Angostura <- load_model_hdf5('model_nns_ensamble_Angostura.h5')

pred_mod_1_Angostura <- max.col(res1_Angostura,ties.method = "last")
pred_mod_2_Angostura <- as.numeric(res2_Angostura)
pred_mod_3_Angostura <- res3_Angostura <- as.numeric(res3_Angostura)-1
pred_mod_4_Angostura <- max.col(res4_Angostura,ties.method = "last")
pred_mod_5_Angostura <- as.numeric(res5_Angostura)

modelos_Angostura <- tibble(.rows = 1)
modelos_Angostura$xgb <- pred_mod_1_Angostura/10
modelos_Angostura$svm <- pred_mod_2_Angostura/10
modelos_Angostura$knn <- pred_mod_3_Angostura/10
modelos_Angostura$nbayes <- pred_mod_4_Angostura/10
modelos_Angostura$mda <- pred_mod_5_Angostura/10
modelos_Angostura$meses <- tail(data_model,1)$Mes/10
modelos_Angostura$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Angostura <- nn_Angostura %>% predict_proba(as.matrix(modelos_Angostura[,1:6]))

modelos_Angostura <- modelos_Angostura %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))
final_Angostura = predict(rf_Angostura, newdata=modelos_Angostura[-7],type="prob")

#Arenal

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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Arenal <- predict(xgboost_Arenal$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Arenal <- stats::predict(svm_Arenal, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Arenal_svm <- attr(res2_Arenal,'probabilities')

#knn
res3_Arenal <- predict(knn_Arenal, newdata = tail(data_knn,1))

#nbayes
res4_Arenal <- predict(nbayes_Arenal, tail(data_model,1), type = 'raw', reshape= TRUE)

#md
res5_Arenal <- predict(md_Arenal, tail(data_model,1))


#nn_Arenal <- load_model_hdf5('model_nns_ensamble_Arenal.h5')

pred_mod_1_Arenal <- max.col(res1_Arenal,ties.method = "last")
pred_mod_2_Arenal <- as.numeric(res2_Arenal)
pred_mod_3_Arenal <- res3_Arenal <- as.numeric(res3_Arenal)-1
pred_mod_4_Arenal <- max.col(res4_Arenal,ties.method = "last")
pred_mod_5_Arenal <- as.numeric(res5_Arenal)

modelos_Arenal <- tibble(.rows = 1)
modelos_Arenal$xgb <- pred_mod_1_Arenal/10
modelos_Arenal$svm <- pred_mod_2_Arenal/10
modelos_Arenal$knn <- pred_mod_3_Arenal/10
modelos_Arenal$nbayes <- pred_mod_4_Arenal/10
modelos_Arenal$mda <- pred_mod_5_Arenal/10
modelos_Arenal$meses <- tail(data_model,1)$Mes/10
modelos_Arenal$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Arenal <- nn_Arenal %>% predict_proba(as.matrix(modelos_Arenal[,1:6]))
modelos_Arenal <- modelos_Arenal %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))


final_Arenal = predict(rf_Arenal, newdata=modelos_Arenal[-7],type="prob")



#Cachi


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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Cachi <- predict(xgboost_Cachi$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Cachi <- stats::predict(svm_Cachi, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Cachi_svm <- attr(res2_Cachi,'probabilities')

#knn
res3_Cachi <- predict(knn_Cachi, newdata = tail(data_knn,1))

#nbayes
res4_Cachi <- predict(nbayes_Cachi, tail(data_model,1), type = 'raw', reshape= TRUE)

#md
res5_Cachi <- predict(md_Cachi, tail(data_model,1))


#nn_Cachi <- load_model_hdf5('model_nns_ensamble_Cachi.h5')

pred_mod_1_Cachi <- max.col(res1_Cachi,ties.method = "last")
pred_mod_2_Cachi <- as.numeric(res2_Cachi)
pred_mod_3_Cachi <- res3_Cachi <- as.numeric(res3_Cachi)-1
pred_mod_4_Cachi <- max.col(res4_Cachi,ties.method = "last")
pred_mod_5_Cachi <- as.numeric(res5_Cachi)

modelos_Cachi <- tibble(.rows = 1)
modelos_Cachi$xgb <- pred_mod_1_Cachi/10
modelos_Cachi$svm <- pred_mod_2_Cachi/10
modelos_Cachi$knn <- pred_mod_3_Cachi/10
modelos_Cachi$nbayes <- pred_mod_4_Cachi/10
modelos_Cachi$mda <- pred_mod_5_Cachi/10
modelos_Cachi$meses <- tail(data_model,1)$Mes/10
modelos_Cachi$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Cachi <- nn_Cachi %>% predict_proba(as.matrix(modelos_Cachi[,1:6]))

modelos_Cachi <- modelos_Cachi %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_Cachi = predict(rf_Cachi, newdata=modelos_Cachi[-7],type="prob")

#Cariblanco

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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Cariblanco <- predict(xgboost_Cariblanco$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Cariblanco <- stats::predict(svm_Cariblanco, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Cariblanco_svm <- attr(res2_Cariblanco,'probabilities')

#knn
res3_Cariblanco <- predict(knn_Cariblanco, newdata = tail(data_knn,1))

#nbayes
res4_Cariblanco <- predict(nbayes_Cariblanco, tail(data_model,1), type = 'raw', reshape= TRUE)

#md
res5_Cariblanco <- predict(md_Cariblanco, tail(data_model,1))


#nn_Cariblanco <- load_model_hdf5('model_nns_ensamble_Cariblanco.h5')

pred_mod_1_Cariblanco <- max.col(res1_Cariblanco,ties.method = "last")
pred_mod_2_Cariblanco <- as.numeric(res2_Cariblanco)
pred_mod_3_Cariblanco <- res3_Cariblanco <- as.numeric(res3_Cariblanco)-1
pred_mod_4_Cariblanco <- max.col(res4_Cariblanco,ties.method = "last")
pred_mod_5_Cariblanco <- as.numeric(res5_Cariblanco)

modelos_Cariblanco <- tibble(.rows = 1)
modelos_Cariblanco$xgb <- pred_mod_1_Cariblanco/10
modelos_Cariblanco$svm <- pred_mod_2_Cariblanco/10
modelos_Cariblanco$knn <- pred_mod_3_Cariblanco/10
modelos_Cariblanco$nbayes <- pred_mod_4_Cariblanco/10
modelos_Cariblanco$mda <- pred_mod_5_Cariblanco/10
modelos_Cariblanco$meses <- tail(data_model,1)$Mes/10
modelos_Cariblanco$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Cariblanco <- nn_Cariblanco %>% predict_proba(as.matrix(modelos_Cariblanco[,1:6]))

modelos_Cariblanco <- modelos_Cariblanco %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_Cariblanco = predict(rf_Cariblanco, newdata=modelos_Cariblanco[-7],type="prob")

#Penas

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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Penas <- predict(xgboost_Penas$finalModel, newdata = tail(ddata_model,1),probability = TRUE , reshape = TRUE)

#svm
res2_Penas <- stats::predict(svm_Penas, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Penas_svm <- attr(res2_Penas,'probabilities')

#knn
res3_Penas <- predict(knn_Penas, newdata = tail(data_knn,1))

#nbayes
res4_Penas <- predict(nbayes_Penas, tail(data_model,1), type = 'raw', reshape= TRUE)

#md
res5_Penas <- predict(md_Penas, tail(data_model,1))

#nn_Penas <- load_model_hdf5('model_nns_ensamble_Penas.h5')

pred_mod_1_Penas <- max.col(res1_Penas,ties.method = "last")
pred_mod_2_Penas <- as.numeric(res2_Penas)
pred_mod_3_Penas <- res3_Penas <- as.numeric(res3_Penas)-1
pred_mod_4_Penas <- max.col(res4_Penas,ties.method = "last")
pred_mod_5_Penas <- as.numeric(res5_Penas)

modelos_Penas <- tibble(.rows = 1)
modelos_Penas$xgb <- pred_mod_1_Penas/10
modelos_Penas$svm <- pred_mod_2_Penas/10
modelos_Penas$knn <- pred_mod_3_Penas/10
modelos_Penas$nbayes <- pred_mod_4_Penas/10
modelos_Penas$mda <- pred_mod_5_Penas/10
modelos_Penas$meses <- tail(data_model,1)$Mes/10
modelos_Penas$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Penas <- nn_Penas %>% predict_proba(as.matrix(modelos_Penas[,1:6]))
modelos_Penas <- modelos_Penas %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_Penas = predict(rf_Penas, newdata=modelos_Penas[-7],type="prob")



#Pirris

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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Pirris <- predict(xgboost_Pirris$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Pirris <- stats::predict(svm_Pirris, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Pirris_svm <- attr(res2_Pirris,'probabilities')

#knn
res3_Pirris <- predict(knn_Pirris, newdata = tail(data_knn,1))

#nbayes
res4_Pirris <- predict(nbayes_Pirris, tail(data_model,1), type = 'raw', reshape = TRUE)

#md
res5_Pirris <- predict(md_Pirris, tail(data_model,1))


#nn_Pirris <- load_model_hdf5('model_nns_ensamble_Pirris.h5')

pred_mod_1_Pirris <- max.col(res1_Pirris,ties.method = "last")
pred_mod_2_Pirris <- as.numeric(res2_Pirris)
pred_mod_3_Pirris <- res3_Pirris <- as.numeric(res3_Pirris)-1
pred_mod_4_Pirris <- max.col(res4_Pirris,ties.method = "last")
pred_mod_5_Pirris <- as.numeric(res5_Pirris)

modelos_Pirris <- tibble(.rows = 1)
modelos_Pirris$xgb <- pred_mod_1_Pirris/10
modelos_Pirris$svm <- pred_mod_2_Pirris/10
modelos_Pirris$knn <- pred_mod_3_Pirris/10
modelos_Pirris$nbayes <- pred_mod_4_Pirris/10
modelos_Pirris$mda <- pred_mod_5_Pirris/10
modelos_Pirris$meses <- tail(data_model,1)$Mes/10
modelos_Pirris$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Pirris <- nn_Pirris %>% predict_proba(as.matrix(modelos_Pirris[,1:6]))
modelos_Pirris <- modelos_Pirris %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_Pirris = predict(rf_Pirris, newdata=modelos_Pirris[-7],type="prob")


#RioMacho


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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_RioMacho <- predict(xgboost_RioMacho$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_RioMacho <- stats::predict(svm_RioMacho, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_RioMacho_svm <- attr(res2_RioMacho,'probabilities')

#knn
res3_RioMacho <- predict(knn_RioMacho, newdata  = tail(data_knn,1))

#nbayes
res4_RioMacho <- predict(nbayes_RioMacho, tail(data_model,1), type = 'raw', reshape = TRUE)

#md
res5_RioMacho <- predict(md_RioMacho, tail(data_model,1))


#nn_RioMacho <- load_model_hdf5('model_nns_ensamble_RioMacho.h5')

pred_mod_1_RioMacho <- max.col(res1_RioMacho,ties.method = "last")
pred_mod_2_RioMacho <- as.numeric(res2_RioMacho)
pred_mod_3_RioMacho <- res3_RioMacho <- as.numeric(res3_RioMacho)-1
pred_mod_4_RioMacho <- max.col(res4_RioMacho,ties.method = "last")
pred_mod_5_RioMacho <- as.numeric(res5_RioMacho)

modelos_RioMacho <- tibble(.rows = 1)
modelos_RioMacho$xgb <- pred_mod_1_RioMacho/10
modelos_RioMacho$svm <- pred_mod_2_RioMacho/10
modelos_RioMacho$knn <- pred_mod_3_RioMacho/10
modelos_RioMacho$nbayes <- pred_mod_4_RioMacho/10
modelos_RioMacho$mda <- pred_mod_5_RioMacho/10
modelos_RioMacho$meses <- tail(data_model,1)$Mes/10
modelos_RioMacho$observado <- as.numeric(tail(data_model,1)$ind2)

#final_RioMacho <- nn_RioMacho %>% predict_proba(as.matrix(modelos_RioMacho[,1:6]))
modelos_RioMacho <- modelos_RioMacho %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_RioMacho = predict(rf_RioMacho, newdata=modelos_RioMacho[-7],type="prob")

#Toro1

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
  mutate(ind = as.integer(ind)) #%>% 
  #drop_na()

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
res1_Toro1 <- predict(xgboost_Toro1$finalModel, newdata = tail(ddata_model,1), probability = TRUE , reshape = TRUE)

#svm
res2_Toro1 <- stats::predict(svm_Toro1, newdata = tail(data_model[,1:20],1), probability = TRUE, reshape = TRUE)
probs_Toro1_svm <- attr(res2_Toro1,'probabilities')

#knn
res3_Toro1 <- predict(knn_Toro1, newdata = tail(data_knn,1))

#nbayes
res4_Toro1 <- predict(nbayes_Toro1, tail(data_model,1), type = 'raw', reshape = TRUE)

#md
res5_Toro1 <- predict(md_Toro1, tail(data_model,1))

#nn_Toro1 <- load_model_hdf5('model_nns_ensamble_Toro1.h5')

pred_mod_1_Toro1 <- max.col(res1_Toro1,ties.method = "last")
pred_mod_2_Toro1 <- as.numeric(res2_Toro1)
pred_mod_3_Toro1 <- res3_Toro1 <- as.numeric(res3_Toro1)-1
pred_mod_4_Toro1 <- max.col(res4_Toro1,ties.method = "last")
pred_mod_5_Toro1 <- as.numeric(res5_Toro1)

modelos_Toro1 <- tibble(.rows = 1)
modelos_Toro1$xgb <- pred_mod_1_Toro1/10
modelos_Toro1$svm <- pred_mod_2_Toro1/10
modelos_Toro1$knn <- pred_mod_3_Toro1/10
modelos_Toro1$nbayes <- pred_mod_4_Toro1/10
modelos_Toro1$mda <- pred_mod_5_Toro1/10
modelos_Toro1$meses <- tail(data_model,1)$Mes/10
modelos_Toro1$observado <- as.numeric(tail(data_model,1)$ind2)

#final_Toro1 <- nn_Toro1 %>% predict_proba(as.matrix(modelos_Toro1[,1:6]))
modelos_Toro1 <- modelos_Toro1 %>% transform(xgb = factor(xgb,levels=lev),svm=factor(svm,levels=lev),knn = factor(knn,levels=lev),nbayes = factor(nbayes,levels=lev),mda = factor(mda,levels=lev),meses = factor(meses,levels=lev_mes),observado=factor(observado,levels=lev_ob))

final_Toro1 = predict(rf_Toro1, newdata=modelos_Toro1[-7],type="prob")

####### Hasta aq�??? la creación de info que necesitamos para gráficos------------------


#rm(xgboost_Angostura,xgboost_Arenal,xgboost_Cachi,xgboost_Cariblanco,xgboost_Penas,xgboost_Pirris,xgboost_reventazon,xgboost_Ventanas,xgboost_Toro1,xgboost_RioMacho)
#rm(svm_Angostura,svm_Arenal,svm_Cachi,svm_Cariblanco,svm_Penas,svm_Pirris,svm_reventazon,svm_Ventanas,svm_Toro1,svm_RioMacho)
#rm(nbayes_Angostura,nbayes_Arenal,nbayes_Cachi,nbayes_Cariblanco,nbayes_Penas,nbayes_Pirris,nbayes_reventazon,nbayes_Ventanas,nbayes_Toro1,nbayes_RioMacho)
#rm(md_Angostura,md_Arenal,md_Cachi,md_Cariblanco,md_Penas,md_Pirris,md_reventazon,md_Ventanas,md_Toro1,md_RioMacho)
#rm(knn_Angostura,knn_Arenal,knn_Cachi,knn_Cariblanco,knn_Penas,knn_Pirris,knn_reventazon,knn_Ventanas,knn_Toro1,knn_RioMacho)
#rm(data_Angostura,data_Arenal,data_Cachi,data_Cariblanco,data_Penas,data_Pirris,data_Reventazon,data_Ventanas,data_Toro1,data_RioMacho)
#rm(data_knn)


#Creamos un tibble por estacion con la informacion de probabilidades por modelo

Reventazon <- tibble(.rows = 10)
Reventazon$xgboost <- t(res1_reventazon)
Reventazon$svm <- t(attr(res2_reventazon,'probabilities'))
Reventazon$nbayes <- t(res4_reventazon)
v <- numeric(10)
v[res3_reventazon] <- 1
Reventazon$knn <- v
v <- numeric(10)
v[as.integer(res5_reventazon)-1] <- 1
Reventazon$mda <- v

Angostura <- tibble(.rows = 10)
Angostura$xgboost <- t(res1_Angostura)
Angostura$svm <- t(attr(res2_Angostura,'probabilities'))
Angostura$nbayes <- t(res4_Angostura)
v <- numeric(10)
v[res3_Angostura] <- 1
Angostura$knn <- v
v <- numeric(10)
v[as.integer(res5_Angostura)-1] <- 1
Angostura$mda <- v

Arenal <- tibble(.rows = 10)
Arenal$xgboost <- t(res1_Arenal)
Arenal$svm <- t(attr(res2_Arenal,'probabilities'))
Arenal$nbayes <- t(res4_Arenal)
v <- numeric(10)
v[res3_Arenal] <- 1
Arenal$knn <- v
v <- numeric(10)
v[as.integer(res5_Arenal)-1] <- 1
Arenal$mda <- v

Cachi <- tibble(.rows = 10)
Cachi$xgboost <- t(res1_Cachi)
Cachi$svm <- t(attr(res2_Cachi,'probabilities'))
Cachi$nbayes <- t(res4_Cachi)
v <- numeric(10)
v[res3_Cachi] <- 1
Cachi$knn <- v
v <- numeric(10)
v[as.integer(res5_Cachi)-1] <- 1
Cachi$mda <- v

Cariblanco <- tibble(.rows = 10)
Cariblanco$xgboost <- t(res1_Cariblanco)
Cariblanco$svm <- t(attr(res2_Cariblanco,'probabilities'))
Cariblanco$nbayes <- t(res4_Cariblanco)
v <- numeric(10)
v[res3_Cariblanco] <- 1
Cariblanco$knn <- v
v <- numeric(10)
v[as.integer(res5_Cariblanco)-1] <- 1
Cariblanco$mda <- v

Penas <- tibble(.rows = 10)
Penas$xgboost <- t(res1_Penas)
Penas$svm <- t(attr(res2_Penas,'probabilities'))
Penas$nbayes <- t(res4_Penas)
v <- numeric(10)
v[res3_Penas] <- 1
Penas$knn <- v
v <- numeric(10)
v[as.integer(res5_Penas)-1] <- 1
Penas$mda <- v

Pirris <- tibble(.rows = 10)
Pirris$xgboost <- t(res1_Pirris)
Pirris$svm <- t(attr(res2_Pirris,'probabilities'))
Pirris$nbayes <- t(res4_Pirris)
v <- numeric(10)
v[res3_Pirris] <- 1
Pirris$knn <- v
v <- numeric(10)
v[as.integer(res5_Pirris)-1] <- 1
Pirris$mda <- v

RioMacho <- tibble(.rows = 10)
RioMacho$xgboost <- t(res1_RioMacho)
RioMacho$svm <- t(attr(res2_RioMacho,'probabilities'))
RioMacho$nbayes <- t(res4_RioMacho)
v <- numeric(10)
v[res3_RioMacho] <- 1
RioMacho$knn <- v
v <- numeric(10)
v[as.integer(res5_RioMacho)-1] <- 1
RioMacho$mda <- v

Toro1 <- tibble(.rows = 10)
Toro1$xgboost <- t(res1_Toro1)
Toro1$svm <- t(attr(res2_Toro1,'probabilities'))
Toro1$nbayes <- t(res4_Toro1)
v <- numeric(10)
v[res3_Toro1] <- 1
Toro1$knn <- v
v <- numeric(10)
v[as.integer(res5_Toro1)-1] <- 1
Toro1$mda <- v

Ventanas <- tibble(.rows = 10)
Ventanas$xgboost <- t(res1_Ventanas)
Ventanas$svm <- t(attr(res2_Ventanas,'probabilities'))
Ventanas$nbayes <- t(res4_Ventanas)
v <- numeric(10)
v[res3_Ventanas] <- 1
Ventanas$knn <- v
v <- numeric(10)
v[as.integer(res5_Ventanas)-1] <- 1
Ventanas$mda <- v


data_modelos <- tibble("Angostura" = Angostura,"Arenal" = Arenal,
               "Reventazon" = Reventazon, "Ventanas" = Ventanas,
               "Cachi" = Cachi, "Cariblanco" = Cariblanco,
               "Penas" = Penas, "Pirris" = Pirris,
               "RioMacho" = RioMacho, "Toro1" = Toro1)


redn <- tibble(.rows = 10)
redn$Angostura <- as.numeric(final_Angostura)
redn$Arenal <- as.numeric(final_Arenal)
redn$Reventazon <- as.numeric(final_reventazon)
redn$Ventanas <- as.numeric(final_Ventanas)
redn$Cachi <- as.numeric(final_Cachi)
redn$Cariblanco <- as.numeric(final_Cariblanco)
redn$Penas <- as.numeric(final_Penas)
redn$Pirris <- as.numeric(final_Pirris)
redn$RioMacho <- as.numeric(final_RioMacho)
redn$Toro1 <- as.numeric(final_Toro1)


#Info de lluvia media mensual para cada estacion
promedios <- tibble(.rows = 12)
promedios$Reventazon <- head(data_Reventazon['prom'],12)
promedios$Angostura <- head(data_Angostura['prom'],12)
promedios$Arenal <- head(data_Arenal['prom'],12)
promedios$Ventanas <- head(data_Ventanas['prom'],12)
promedios$Cachi <- head(data_Cachi['prom'],12)
promedios$Cariblanco <- head(data_Cariblanco['prom'],12)
promedios$Penas <- head(data_Penas['prom'],12)
promedios$Pirris <- head(data_Pirris['prom'],12)
promedios$RioMacho <- head(data_RioMacho['prom'],12)
promedios$Toro1 <- head(data_Toro1['prom'],12)


deciles_Arenal = c('91','148','223','274','326','361','404','449','502','729')
deciles_Penas = c('119','209','303','379','446','518','562','621','721','1083')
deciles_Toro1 = c('77','147','220','272','309','355','393','450','531','1283')
deciles_Cariblanco = c('101','200','292','370','411','469','519','583','735','1583')
deciles_RioMacho = c('90','135','193','251','305','342','385','427','473','817')
deciles_Cachi = c('75','115','164','221','266','310','342','374','422','602')
deciles_Angostura = c('111','171','226','283','321','367','404','444','503','758')
deciles_Reventazon = c('123','198','249','270','293','328','364','415','484','849')
deciles_Ventanas = c('16','34','56','101','176','225','274','335','413','721')
deciles_Pirris = c('14','28','49','93','152','221','265','319','389','729')

decil_val <- tibble(.rows = 10)
decil_val$Arenal <- c(91,148,223,274,326,361,404,449,502,729)
decil_val$Penas <- c(119,209,303,379,446,518,562,621,721,1083)
decil_val$Toro1 <- c(77,147,220,272,309,355,393,450,531,1283)
decil_val$Cariblanco <- c(101,200,292,370,411,469,519,583,735,1583)
decil_val$RioMacho <- c(90,135,193,251,305,342,385,427,473,817)
decil_val$Cachi <- c(75,115,164,221,266,310,342,374,422,602)
decil_val$Angostura <- c(111,171,226,283,321,367,404,444,503,758)
decil_val$Reventazon <- c(123,198,249,270,293,328,364,415,484,849)
decil_val$Ventanas <- c(16,34,56,101,176,225,274,335,413,721)
decil_val$Pirris <- c(14,28,49,93,152,221,265,319,389,729)






red_acomodada = redn %>% mutate(decil = seq(1,10)) %>% gather("Estacion","Valor",-"decil")
write_xlsx(red_acomodada,"D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Resultados/ensamble.xlsx")

rm(xgboost_Angostura,xgboost_Arenal,xgboost_Cachi,xgboost_Cariblanco,xgboost_Penas,xgboost_Pirris,xgboost_reventazon,xgboost_Ventanas,xgboost_Toro1,xgboost_RioMacho)
rm(svm_Angostura,svm_Arenal,svm_Cachi,svm_Cariblanco,svm_Penas,svm_Pirris,svm_reventazon,svm_Ventanas,svm_Toro1,svm_RioMacho)
rm(nbayes_Angostura,nbayes_Arenal,nbayes_Cachi,nbayes_Cariblanco,nbayes_Penas,nbayes_Pirris,nbayes_reventazon,nbayes_Ventanas,nbayes_Toro1,nbayes_RioMacho)
rm(md_Angostura,md_Arenal,md_Cachi,md_Cariblanco,md_Penas,md_Pirris,md_reventazon,md_Ventanas,md_Toro1,md_RioMacho)
rm(knn_Angostura,knn_Arenal,knn_Cachi,knn_Cariblanco,knn_Penas,knn_Pirris,knn_reventazon,knn_Ventanas,knn_Toro1,knn_RioMacho)
rm(data_Angostura,data_Arenal,data_Cachi,data_Cariblanco,data_Penas,data_Pirris,data_Reventazon,data_Ventanas,data_Toro1,data_RioMacho)
rm(data_knn)
rm(Ventanas,Angostura,Pirris,Penas,Cachi.Cariblanco,Toro1,RioMacho,Reventazon,Arenal)



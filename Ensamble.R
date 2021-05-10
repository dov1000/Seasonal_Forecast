#Ensamble de modelos
setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")
library('plyr')
library("tidyverse")
#library('xgboost')
library('caret')
library('SuperLearner')


data_Reventazon <-read.csv('Reventazon.csv') %>% 
  as_tibble()

deciles <- quantile(data_Reventazon$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

data_model <- data_Reventazon %>% 
  select(3:16)

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

#Clasificando un mes siguiente

train <- data_model[1:202,]
test <- data_model[203:226,]




#Inicio de Modelado

#Modelos disponibles
listWrappers()


SL.xgboost.tune <- function(...){
  SL.xgboost(...,
             colsample_bytree=0.8,
             min_child_weight=1,
             subsample=1,
             gamma=0,
             max_depth = 2,
             eta = 0.01)
}

SL.svm.tune <- function(...){
  SL.svm(...,
         gamma=0.001288582,
         cost=3104.188)
}



set.seed(150)

modelos.cv <- CV.SuperLearner(as.numeric(train$ind2)-1,
                           train[,1:20],
                           V=5,
                           family = gaussian(),
                           SL.library = list('SL.xgboost',
                                             'SL.svm',
                                             'SL.xgboost.tune',
                                             'SL.svm.tune'))


  
  
  
  
summary(modelos.cv)
plot(modelos.cv)


#fit

modelos.tune <- SuperLearner(as.numeric(train$ind2)-1,
                                train[,1:20],
                                family = gaussian(),
                                SL.library = list('SL.xgboost',
                                                  'SL.svm',
                                                  'SL.xgboost.tune',
                                                  'SL.svm.tune'))
modelos.tune


predictions.tune <- predict.SuperLearner(modelos.tune, newdata=data_model[,1:20])
conv.preds.tune <- round(predictions.tune$pred, digits = 0)

#convertir a factores
confusionMatrix(as.numeric(conv.preds.tune),as.numeric(data_model$ind2)-1)
                
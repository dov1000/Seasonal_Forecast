library("tidyverse")
library('e1071')
library('caret')


setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")

data_Ventanas <-read.csv('Ventanas.csv') %>% 
  as_tibble()


#Primero comparamos con los deciles de la lluvia total, luego vemos si funcina
#mejor la comparaciÃ³n dividiendo por mes

deciles <- quantile(data_Ventanas$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))


data_model <- data_Ventanas %>% 
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
  mutate(ind = case_when(lluvia < deciles[1] ~ 'n0',
                         between(lluvia,deciles[1],deciles[2])~ 'n1',
                         between(lluvia,deciles[2],deciles[3]) ~ 'n2',
                         between(lluvia,deciles[3],deciles[4]) ~ 'n3',
                         between(lluvia,deciles[4],deciles[5]) ~ 'n4',
                         between(lluvia,deciles[5],deciles[6]) ~ 'n5',
                         between(lluvia,deciles[6],deciles[7]) ~ 'n6',
                         between(lluvia,deciles[7],deciles[8]) ~ 'n7',
                         between(lluvia,deciles[8],deciles[9]) ~ 'n8',
                         lluvia > deciles[9] ~'n9' )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  drop_na()

#Clasificando un mes siguiente

train <- data_model[1:528,]
test <- data_model[529:538,]


#x = trainControl(method = "repeatedcv",
#                 number = 25,
#                 repeats = 10,
#                 classProbs = TRUE)

x = trainControl(method = "LOOCV",
                 classProbs = TRUE)


model <- train(ind2~. , data = train, method = "knn",
                trControl = x,
                preProcess = c("center", "scale"),
                tuneLength =15)

pred <- predict(model,data_model)
table(pred,data_model$ind2)
sum(diag(table(pred,data_model$ind2)))

#0.2168142

#Saquemos la  probabilidades para cada caso 
try <- model$pred %>% filter(k==17)
try$k <- NULL 
try$rowIndex <- NULL


saveRDS(model,file = "C:/Users/daoban/Documents/RelacionIndicesLluvia/Modelos/Ventanas/knn")

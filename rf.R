library("tidyverse")
library('randomForest')

setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")

data_reventazon <-read.csv('Reventazon.csv') %>% 
  as_tibble()


#Primero comparamos con los deciles de la lluvia total, luego vemos si funcina
#mejor la comparaciÃ³n dividiendo por mes

deciles <- quantile(data_reventazon$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))


data_model <- data_reventazon %>% 
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
  mutate(ind = case_when(lluvia < deciles[1] ~ '0',
                         between(lluvia,deciles[1],deciles[2])~ '1',
                         between(lluvia,deciles[2],deciles[3]) ~ '2',
                         between(lluvia,deciles[3],deciles[4]) ~ '3',
                         between(lluvia,deciles[4],deciles[5]) ~ '4',
                         between(lluvia,deciles[5],deciles[6]) ~ '5',
                         between(lluvia,deciles[6],deciles[7]) ~ '6',
                         between(lluvia,deciles[7],deciles[8]) ~ '7',
                         between(lluvia,deciles[8],deciles[9]) ~ '8',
                         lluvia > deciles[9] ~'9' )) 

data_model <- data_model %>%
  mutate(ind2 = lead(data_model$ind,1)) %>% 
  mutate(ind2 = factor(ind2)) %>% 
  drop_na()

#Clasificando un mes siguiente

train <- data_model[1:202,]
test <- data_model[203:226,]



model <- randomForest(ind2 ~., data = train, mtry =11,ntree = 30,importance = TRUE)
#model <- randomForest(ind2 ~., data = train,importance = TRUE)
model

# Predecimos en train
predTrain <- predict(model, train)
# Checking classification accuracy
table(predTrain, train$ind2)  



# Predicting on Validation set
predValid <- predict(model, test, type = "class")
# Checking classification accuracy
mean(predValid == test$ind2)                    
table(predValid,test$ind2)


#Importancia de Features
importance(model)        
varImpPlot(model)    




#Tuning
#solo rl mtr
m2 <- tuneRF(
  x          = train[1:20],
  y          = train$ind2,
  ntreeTry   = 500,
  mtryStart  = 3,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = TRUE      # to not show real-time progress 
)

#Con ranger mas rapido y para todo

hyper_grid <- expand.grid(
  mtry       = seq(1, 20, by = 5),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)


for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = ind2 ~ ., 
    data            = train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}






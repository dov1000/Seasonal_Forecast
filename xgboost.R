library("tidyverse")
library('xgboost')
library('caret')
library('doParallel')
setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")

cls = makeCluster(4)
registerDoParallel(cls)

getDoParWorkers()

data_Reventazon <-read.csv('Reventazon.csv') %>% 
  as_tibble()


#Primero comparamos con los deciles de la lluvia total, luego vemos si funcina
#mejor la comparaciÃ³n dividiendo por mes

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


#dtrain <- xgb.DMatrix(data = train$data, label = train$ind)

dtrain <- xgb.DMatrix(data = as.matrix(train[1:20]), label=as.integer(as.matrix(train[21])))
dtest <- xgb.DMatrix(data = as.matrix(test[1:20]), label=as.integer(as.matrix(test[21])))

ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))

#dtest2 <- xgb.DMatrix(data = as.matrix(test2[1:20]), label=as.integer(as.matrix(test2[21])))

watchlist <- list(train=dtrain, test=dtest)


mod <- xgmodel <- xgboost(data=dtrain,
                          colsample_bytree =0.8,
                          min_child_weight = 1,
                          subsample =1,
                          gamma = 0,
                          max.depth=2,
                          nrounds = 800,
                          num_class =10,
                          eta = 0.01, 
                          objective = "multi:softprob")


bst <- xgb.train(data=dtrain, max.depth=5, 
                 nrounds=10, 
                 watchlist=watchlist,
                 num_class =10, 
                 "eval_metric" = "merror",
                 objective = "multi:softprob",
                 gamma = 1)



bst <- xgb.train(data=dtrain,
                 max.depth=2, 
                 eta = 0.01,
                 nrounds=500, 
                 colsample_bytree = 0.8,
                 min_child_weight = 1,
                 subsample = 0.7,
                 watchlist=watchlist,
                 num_class =10, 
                 "eval_metric" = "merror",
                 objective = "multi:softmax",
                 gamma = 0)


fitted.results <- predict(bst, newdata = ddata_model, reshape = TRUE)
table(pronos=fitted.results,vistos=data_model$ind2)



#Busqueda de mejores parámetros

#Original
grid_default <- expand.grid(nrounds = 100,
                            max_depth = 6,
                            eta = 0.3,
                            gamma = 0,
                            colsample_bytree =1,
                            min_child_weight = 1,
                            subsample =1)


train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE,
  allowParallel = TRUE
)


xgb_base <- caret::train(
  x = as.matrix(train[1:20]),
  y= train$ind2,
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  verbose = TRUE
)


#Siguiente fase

nrounds <- 1000

tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = nrounds, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


tune_control <- caret::trainControl(
  method = "LOOCV", # cross-validation
  #number = 5, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE # FALSE for reproducible results 
)



xgb_tune <- caret::train(
  x = as.matrix(train[1:20]),
  y= train$ind2,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)


# Graficamos
tuneplot <- function(x, probs = .90) {
  
  xx <- x$results
  
  n <- nrow(xx)
  
  xx %>% 
    as_tibble() %>% 
    mutate(iter = 1:n) %>% 
    ggplot() + 
    geom_line(aes(x=iter, 
                   y = Kappa)) + 
    facet_grid(eta~max_depth) +
    theme_bw()
  
  
  # ggplot(x) +
  #   coord_cartesian(ylim = c(quantile(x$results$Accuracy, probs = probs), min(x$results$Accuracy))) +
  #   theme_bw()
}

tuneplot2 <- function(x, probs = .90) {
  xx <- x$results
  
  n <- nrow(xx)
  
  xx %>% 
    as_tibble() %>% 
    mutate(iter = 1:n) %>% 
    ggplot() + 
    geom_line(aes(x=iter, 
                  y = Accuracy, 
                  group = eta, 
                  col = factor(eta))) + 
    facet_wrap(~max_depth, ncol = 3) +
    labs(x = "Número de iteración",
         y = "Accuracy",
         title = "xxxx",
         subtitle = "xxxx",
         caption = "xxxxxx", 
         col = "Eta") +
    theme_bw() 
    # theme(legend.position = "bottom")
}

tuneplot(xgb_tune)
tuneplot2(xgb_tune)


tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                     c(xgb_tune$bestTune$max_depth:4),
                     xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)


xgb_tune2 <- caret::train(
  x = as.matrix(train[1:20]),
  y= train$ind2,
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  verbose = TRUE
)

 tuneplot2(xgb_tune2)

xgb_tune2$bestTune



tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  x = as.matrix(train[1:20]),
  y= train$ind2,
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  verbose = TRUE
)



tuneplot(xgb_tune3, probs = .95)

xgb_tune3$bestTune


tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = as.matrix(train[1:20]),
  y= train$ind2,
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune4)

xgb_tune4$bestTune


tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 10000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = as.matrix(train[1:20]),
  y= train$ind2,
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  verbose = TRUE
)

tuneplot(xgb_tune5)
xgb_tune5$bestTune

#Usamos estos últimos parámetros

(final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
))


(xgb_model2 <- caret::train(
  x = as.matrix(data_model[1:20]),
  y= data_model$ind2,
  trControl = train_control,
  tuneGrid = final_grid,
  method = "xgbTree",
  verbose = TRUE
))


saveRDS(mod,file = "C:/Users/daoban/Documents/RelacionIndicesLluvia/Modelos/Reventazon/xgboost")

model <- readRDS("C:/Users/daoban/Documents/RelacionIndicesLluvia/xgb_model")


fitted.results <- predict(xgb_model2, newdata = test)
table(pronos=fitted.results,vistos=test$observado)


pred <- predict(object = bst, newdata = dtest, reshape = TRUE)


image(t(matrix(pred, ncol = 10, byrow = TRUE)))

#89/101 del test data
#428/503 del completo 
stopCluster(cls)


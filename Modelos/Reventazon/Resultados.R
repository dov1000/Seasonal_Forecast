#https://rstudio-pubs-static.s3.amazonaws.com/228019_f0c39e05758a4a51b435b19dbd321c23.html
#https://github.com/lmcinnes/umap/blob/master/doc/clustering.rst
#https://blog.datascienceheroes.com/playing-with-dimensions-from-clustering-pca-t-sne-to-carl-sagan/
#https://github.com/tkonopka/umap
#https://toscano84.github.io/ 
#Procesamiento de resultados de todos los modelos
#setwd("C:/Users/daoban/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/")
setwd("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/")
library('plyr')
library('xgboost')
library('caret')
library("tidyverse")
library("e1071")
library('MASS')
library('mda')


data_Arenal <-read.csv('Arenal.csv') %>% 
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

#Clasificando un mes siguiente

train <- data_model[1:202,]
test <- data_model[203:226,]

#dtrain <- xgb.DMatrix(data = train$data, label = train$ind)

dtrain <- xgb.DMatrix(data = as.matrix(train[1:20]), label=as.integer(as.matrix(train[21])))
dtest <- xgb.DMatrix(data = as.matrix(test[1:20]), label=as.integer(as.matrix(test[21])))

ddata_model<- xgb.DMatrix(data = as.matrix(data_model[1:20]), label=as.integer(as.matrix(data_model[21])))



xgboost <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/xgboost")
knn <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/knn")
nbayes <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/NBayes")
svm <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/SVM")
md <- readRDS("C:/Users/dov10/OneDrive/TrabajoenCasa27062019/RelacionIndicesLluvia/Modelos/Arenal/MixDiscAna")

#Resultados xgboost
res1 <- predict(xgboost$finalModel, newdata = ddata_model, probability = TRUE , reshape = TRUE)
mc1 <- table(pronos=mapvalues(max.col(res1,ties.method = "last"),c(1,2,3,4,5,6,7,8,9,10),c(0,1,2,3,4,5,6,7,8,9)),
                       vistos=data_model$ind2)

#write.csv(res1,'xgboost_prediction.csv')



#mapvalues(max.col(res1,ties.method = "last"),c(1,2,3,4,5,6,7,8,9,10),c(0,1,2,3,4,5,6,7,8,9)

          
confusionMatrix(data = factor(mapvalues(max.col(res1,ties.method = "last"),c(1,2,3,4,5,6,7,8,9,10),c(0,1,2,3,4,5,6,7,8,9))),
                reference = factor(data_model$ind2), mode = "everything")


#Plot de la matriz de confusión



qplot(ind2, 
      mapvalues(max.col(res1,ties.method = "last"),c(1,2,3,4,5,6,7,8,9,10),c(0,1,2,3,4,5,6,7,8,9)), 
      data=data_model, 
      colour= ind2, 
      geom = c("boxplot", "jitter"),
      main = "Matriz de Confusión XGboost", 
      xlab = "Percentil Observado", 
      ylab = "Percentil Predicho")

n <- nrow(res1)
h <- res1[1,]

probs = seq(1,10,1)


ggplot(data = t(res1) %>% as_tibble(), aes(x = probs,y = V1)) +
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 9),n = 180, se = FALSE)+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 8),n = 180, se = FALSE, colour= 'darkred')+
  geom_area(fill = "#999999",
            color = "#999999",
            alpha = 0.5)



res12 <- gather(data = res1 %>% 
                  as_tibble() %>% 
                  mutate(obs = 1:n),"percenti","proba",-obs) %>% 
  mutate(num = case_when(percenti == "V1" ~ 1,
                         percenti == "V2" ~ 2,
                         percenti == "V3" ~ 3,
                         percenti == "V4" ~ 4,
                         percenti == "V5" ~ 5,
                         percenti == "V6" ~ 6,
                         percenti == "V7" ~ 7,
                         percenti == "V8" ~ 8,
                         percenti == "V9" ~ 9,
                         percenti == "V10" ~ 10))





ggplot(data = res12 %>% as_tibble())+
  geom_area(aes(x = num, y = proba, group = obs),
            fill = "#999999",
            color = "#999999",
            alpha = 0.5)




ggplot(data = res12,aes(x = percenti, y = proba, group = obs)) + 
  geom_line(alpha = 0.4)

resxg <- gather(data = res1 %>% as_tibble() %>% mutate(obs = 1:n),
       'decil','proba',-c(obs)) %>%
  mutate(model = 'xgboost') %>% 
  mutate(decil = case_when(decil == "V1" ~ 1,
                           decil == "V2" ~ 2,
                           decil == "V3" ~ 2,
                           decil == "V4" ~ 4,
                           decil == "V5" ~ 5,
                           decil == "V6" ~ 6,
                           decil == "V7" ~ 7,
                           decil == "V8" ~ 8,
                           decil == "V9" ~ 9,
                           decil == "V10" ~ 10))


#Resultados SVM


res2 <- stats::predict(svm, newdata = data_model, probability = TRUE, reshape = TRUE)
probs_svm <- attr(res2,'probabilities')
mc2 <- table(pronos=res2,
             vistos=data_model$ind2)

confusionMatrix(data = factor(res2), reference = data_model$ind2)

ggplot(data = t(probs_svm) %>% as_tibble(), aes(x = probs,y = `1`)) +
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 9),n = 180, se = FALSE)+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 8),n = 180, se = FALSE, colour= 'darkred')+
  geom_area(fill = "#999999",
            color = "#999999",
            alpha = 0.5)

write.csv(probs_svm,'svm_prediction.csv')


ressvm <- gather(data = probs_svm %>% as_tibble() %>% mutate(obs = 1:n),
                 'decil','proba',-c(obs)) %>% mutate(model = 'svm') %>% 
  mutate(decil = case_when(decil == 0 ~ 1,
                           decil == 1 ~ 2,
                           decil == 2 ~ 3,
                           decil == 3 ~ 4,
                           decil == 4 ~ 5,
                           decil == 5 ~ 6,
                           decil == 6 ~ 7,
                           decil == 7 ~ 8,
                           decil == 8 ~ 9,
                           decil == 9 ~ 10))

#KNN
#data para knn

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

res3 <- predict(knn, newdata = data_knn, reshape = TRUE)



probs_knn <- attr(res3,'probabilities')
mc3 <- table(pronos=res3,
             vistos=data_knn$ind2)

mc3<- tibble(mc3)
colnames(mc3$mc3) <- c('0','1','2','3','4','5','6','7','8','9')
mc3 <- mc3$mc3


write.csv(res3,'knn_prediction.csv')


#replace(res3,c('n0','n1','n2','n3','n4','n5','n6','n7','n8','n9'),c('0','1','2','3','4','5','6','7','8','9'))

#Nbayes
res4 <- predict(nbayes,data_model, type = "raw",reshape = TRUE)
mc4 <- table(pronos=mapvalues(max.col(res4,ties.method = "last"),c(1,2,3,4,5,6,7,8,9,10),c(0,1,2,3,4,5,6,7,8,9)),
                    vistos=data_model$ind2)
write.csv(res4,'nbayes_prediction.csv')



resnbayes <- gather(data = res4 %>% as_tibble() %>% mutate(obs = 1:n),
                 'decil','proba',-c(obs)) %>% mutate(model = 'nbayes') %>% 
  mutate(decil = case_when(decil == 0 ~ 1,
                           decil == 1 ~ 2,
                           decil == 2 ~ 3,
                           decil == 3 ~ 4,
                           decil == 4 ~ 5,
                           decil == 5 ~ 6,
                           decil == 6 ~ 7,
                           decil == 7 ~ 8,
                           decil == 8 ~ 9,
                           decil == 9 ~ 10))



#md
library('mda')
res5 <- predict(md, data_model)
mc5<-table(res5,data_model$ind2)

write.csv(res5,'md_prediction.csv')
#Analisis Total

mc_tot <- (mc1+mc2+mc3+mc4+mc5)/5


#Todas las predicciones de 0 a 9

pred_mod_1 <- max.col(res1,ties.method = "last")-1
pred_mod_2 <- as.numeric(res2)-1
pred_mod_3 <- res3 <- as.numeric(res3)-1
pred_mod_4 <- max.col(res4,ties.method = "last")-1
pred_mod_5 <- as.numeric(res5)-1

#Creación de matriz para input del nuevo modelo
modelos <- tibble(.rows = nrow(data_model))
modelos$xgb <- pred_mod_1
modelos$svm <- pred_mod_2
modelos$knn <- pred_mod_3
modelos$nbayes <- pred_mod_4
modelos$mda <- pred_mod_5
modelos$observado <- as.numeric(data_model$ind2)-1

#Red Neuronal Simple
library(keras)
x_nntrain <- as.matrix(modelos[1:496,1:5]/10)
y_nntrain <- to_categorical(as.matrix(modelos[1:496,6]),10)
x_nntest <- as.matrix(modelos[497:502,1:5]/10)
y_nntest <-to_categorical(as.matrix(modelos[497:502,6]),10)


#Estructura nn secuencial

model_nns <- keras_model_sequential()


model_nns %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(5),
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units =64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units =32,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l = 0.001)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10, activation = 'softmax')


model_nns %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_sgd(),
  metrics = c('accuracy')
)
#Entrenando
history <- model_nns %>% fit(
  x_nntrain, y_nntrain,
  epochs = 250, batch_size = 3,
  validation_split = 0.2
)


plot(history)



#Evaluación en test data
model_nns %>% evaluate(x_nntest,y_nntest)


model_nns %>% predict_classes(x_nntest)
final <- model_nns %>% predict_proba(x_nntest)


write.csv(final,'nn_prediction')

#(file = "nn_prediction", row.names = 1)



save_model_hdf5(model_nns, 'model_nns_ensamble_Arenal.h5')
model <- load_model_hdf5('model_nns_ensamble_Arenal.h5')



ggplot(data = t(final) %>% as_tibble(), aes(x = probs,y = V1)) +
  geom_point()+
  #geom_smooth(method = 'lm',formula = y ~ poly(x, 9),n = 180, se = FALSE)+
  #geom_smooth(method = 'lm',formula = y ~ poly(x, 8),n = 180, se = FALSE, colour= 'darkred')+
  geom_area(fill = "#999999",
            color = "#999999",
            alpha = 0.5)


nn_data = t(final) %>% as_tibble()

decil = c('0%-10%','10%-20%','20%-30%',
          '30%-40%','40%-50%','50%-60%',
          '60%-70%','70%-80%','80%-90%','90%-100%')

data_bar <- data.frame(deciles = decil,
                       valor = c(nn_data$V1))


ggplot(data = data_bar, aes(x = decil, y = 100*valor, fill = 100*valor)) +
  geom_bar(stat = "identity")+
  theme(legend.position = "none")

#ggplotly(ggplot(data = data_bar, aes(x = decil, y = 100*valor, fill = 100*valor)) +
#           geom_bar(stat = "identity")+
#           theme(legend.position = "none"))

ggplot(data = data_bar, aes(x = decil, y = 100*valor )) +
  geom_point()+
  #geom_smooth(method = 'lm',formula = y ~ poly(x, 9),n = 180, se = FALSE)+
  #geom_smooth(method = 'lm',formula = y ~ poly(x, 8),n = 180, se = FALSE, colour= 'darkred')+
  geom_area(fill = "#999999",
            color = "#999999",
            alpha = 0.5)

#barplot(prueba$V1)

#Con plotly

library(plotly)

p <- plot_ly(data_bar, x = ~decil,
             y = ~(100*valor), 
             type = 'bar', 
             name = 'Viridis',
             color = 'Jet') %>% 
  layout(title = 'Resultados de Ensamble',
         yaxis = list(title = 'Probabilidad'),
         xaxis = list(title = 'Decil'))


res1_acomodado = t(res1) %>% as_tibble()


conjunto_modelos <- plot_ly(x = decil, y = res1_acomodado$V1, type = 'scatter',mode='markers+lines')

data_probs <- full_join(resxg,ressvm) %>% full_join(resnbayes)%>% as_tibble() 


write.csv(data_probs,'data_probs')


ggplot(res_probs %>% filter(res_probs$obs==1),
       aes(x=decil,y=proba,group=model))+
  geom_line(aes(linetype=model,color=model))+
  geom_point(aes(shape=model,color = model))+
  scale_x_continuous(breaks = 1:10,labels=deciles_nombre)


res3

#Clustering
#FactoMineR::PCA(res1, scale.unit = FALSE)
#FactoMineR::PCA(res1)
#xxx <- tsne::tsne(res1)


#plotly::ggplotly(bind_cols(data_model %>% drop_na(),as_tibble(xxx)[]) %>% ggplot() + geom_point(aes(v1,v2, col = factor(Mes), shape = factor(Mes))) + scale_color_viridis_d(option = "B") + scale_shape_manual(values = 0:12))

      
#use_python("C:/Users/daoban/AppData/Local/Continuum/anaconda3/python.exe", required = T)
#py_config()  
#Module(pandas)

#library(knitr)
#knit('Interfaz.Rmd')
#rmarkdown::render('Interfaz.Rmd')

res_probs %>% filter(res_probs$obs==1)

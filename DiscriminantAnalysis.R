library("tidyverse")
library('caret')
library('MASS')
library('mda')

setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")

data_reventazon <-read.csv('Ventanas.csv') %>% 
  as_tibble()


#Primero comparamos con los deciles de la lluvia total, luego vemos si funcina
#mejor la comparaciÃ³n dividiendo por mes

deciles <- quantile(data_reventazon$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))


data_model <- data_reventazon %>% 
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
  drop_na()

#Clasificando un mes siguiente

train <- data_model[1:528,]
test <- data_model[529:538,]


#Modelo Lineal

model <- lda(ind2~., data = train)

pred <- predict(model, data_model) 

# Predicted classes
head(pred$class,10)

# Predicted probabilities of class memebership.
head(pred$posterior, 10) 

# Linear discriminants
head(pred$x, 3) 

t1<-table(pred$class,data_model$ind2)
#0.3628319

#Cuadrático 
#Súper overfiteado
model2 <- qda(ind2~., data = data_model)
pred2 <- predict(model2, data_model)
t2<-table(pred2$class,data_model$ind2)

#Mixture discriminant analysis - MDA

model3 <- mda(ind2~., data = train)
pred3 <- predict(model3, data_model)
t3<-table(pred3,data_model$ind2)
#0.5530973
#Flexible discriminant analysis - FDA
#Para no lineales

model4 <- fda(ind2~., data = train)
pred4 <- predict(model4, data_model)
t4<-table(pred4,data_model$ind2)

#0.3672566

saveRDS(model3,file = "C:/Users/daoban/Documents/RelacionIndicesLluvia/Modelos/Ventanas/MixDiscAna")

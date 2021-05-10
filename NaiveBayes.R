library("tidyverse")
library('e1071')
setwd("D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/")



data_Ventanas <- read.csv('Ventanas.csv') %>% 
  as_tibble()


deciles <- quantile(data_Ventanas$lluvia,c(.1,.2,.3,.4,.5,.6,.7,.8,.9))


data_model <- data_Ventanas %>% select(3:16)

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
  mutate(ind2 = lead(data_model$ind,1))%>% 
  mutate(ind2 = factor(ind2))%>% 
  mutate(ind = as.integer(ind)) %>% 
  drop_na()

train <- data_model[1:528,]
test <- data_model[529:538,]


bayes_model <- naiveBayes(ind2 ~., data = train)

pred <- predict(bayes_model,data_model)


#Con probabilidades
pred2 <- predict(bayes_model,data_model, type = "raw",reshape = TRUE)



table(pred, data_model$ind2)
saveRDS(bayes_model,file = "C:/Users/daoban/Documents/RelacionIndicesLluvia/Modelos/Ventanas/NBayes")


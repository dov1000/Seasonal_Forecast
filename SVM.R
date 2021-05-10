library("tidyverse")
library('e1071')
setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")


data_Ventanas <- read.csv('Ventanas.csv') %>% 
  as_tibble()


#Primero comparamos con los deciles de la lluvia total, luego vemos si funcina
#mejor la comparación dividiendo por mes



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

#train2 <- data_model[1:216,]

#svmfit <- svm(ind ~., data = train,kernel = "linear", cost = 10, scale = FALSE)

svmfit <- svm(ind2 ~., 
              data = train, 
              kernel = "radial", 
              probability = TRUE ,
              gamma = 0.00158643, 
              cost = 78.799324)


print(svmfit)
summary(svmfit)





#Buscando los mejores parámetros
x1<- tune(svm, ind2~., 
          data = train,
          tunecontrol = tune.control(nrepeat =3,sampling = "cross",cross = 25), 
          ranges = list(gamma = 2^(seq(-15,-5,0.1)), 
                        cost = 2^(seq(11,13,0.1))),
          performances = TRUE)



x2<- tune(svm, ind2 ~ mei + car + mei_1, 
          data = train,
          tunecontrol = tune.control(sampling = "cross",cross = 144), 
          ranges = list(gamma = 2^(seq(-10,-8,0.1)), 
                        cost = 2^(seq(11,13,0.1))),
          performances = TRUE)



#Agregar Probabilidades a los SVM




best.tune(method = svm, train.x = ind ~ Mes + amo + 
            car + nino12 + tni + lluvia + tna_1 + tni_1 + nao_1, 
          data = train, ranges = list(gamma = 2^(seq(-11, 
                                                     -7, 0.1)), cost = 2^(seq(8, 15, 0.5))), tunecontrol = tune.control(sampling = "cross", 
                                                                                                                         cross = 149), performances = TRUE)


#https://stackoverflow.com/questions/32026436/how-to-optimize-parameters-using-genetic-algorithms

predi <- predict(x1$best.model,test)
library(InformationValue)
confusionMatrix(as.integer(test$ind2), as.integer(predi))

#SVM

predi

print(svmfit)


#fitted.results <- predict(x2, newdata = data_model, probability=TRUE)
fitted.results <- predict(x1$best.model, newdata = data_model)
#fitted.results <- ifelse(fitted.results > 0.799,1,0)
table(pronos=fitted.results,vistos=data_model$ind2)



h <- fitted(svmfit)
h
sum(as.integer(h))


saveRDS(svmfit,file = "C:/Users/daoban/Documents/RelacionIndicesLluvia/Modelos/Ventanas/SVM")


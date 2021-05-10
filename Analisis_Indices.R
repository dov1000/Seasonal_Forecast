library("tidyverse")
setwd("C:/Users/daoban/Documents/RelacionIndicesLluvia/")
data <- read.csv(file = "Enero", header = TRUE, row.names = 1) %>% drop_na()
str(data) #Structure of data
#Análisis Univariado MEI
hist(data$mei, xlab = "MEI")

box <- ggplot(stack(data), aes(x = ind,y = values,color = ind, fill = ind))+
  geom_boxplot(alpha = 0.7)+
  scale_x_discrete(name = "Índices")+
  scale_y_continuous(name = 'Valores',breaks = seq(-4,4,0.5),limits = c(-4,4))
  
box

plot(data)

#Correlación de variables relacionadas con el niño
plot(data[,c(1,2,5,7,8)])
cor(data[,c(1,2,5,7,8)])

#Correlación atlántico norte y ártico
plot(data[,c(1,11,12)])
cor(data[,c(1,11,12)])

cor(data[,c(4,11,12)])

plot(data[,c(3,9)])
cor(data[,c(3,9)])

#tni con niño12 y niño4 (de ahí se calcula)
cor(data[,c(10,6,7)])

#hist(x$nino12)
#hist(x$oni)

#boxplot(x)

#ver cuales tienen mejor correlacion para escogerlos


ggplot(data, aes(x= mei, y = oni))+
  geom_point()+
  geom_smooth(method = lm, fullrange = TRUE,fill = 'lightblue', color = 'purple')+
  scale_color_brewer(palette="Dark2") +
  theme_minimal()


#cor.test(data$mei,data$oni) #correlacion lineal

#Análisis de la monotonía
cor.test(data$mei,data$oni, method = "spearman") #analizo la monotonia

cor.test(data$nao,data$ao, method = "spearman")


cor.test(data$tni,data$nino4, method = "spearman") 
cor.test(data$tni,data$nino12, method = "spearman")


cor.test(data$tna,data$amo, method = "spearman")

cor.test(data$car,data$amo, method = "spearman")


corr_ma <- round(cor(data),digits = 3) #matriz de correlaciones
#corr_ma
#plot(x)  #plot de todos
#library("RColorBrewer")  #Libreria de paletas
#Para observar y comparar el "aporte" de cada variable (eje: niño o niña)

#
palette(rainbow(12, s= 0.6, v = 0.75))
t1 <- stars(data[,c(1,2,5,7,8)],key.loc = c(15,1.5), flip.labels = FALSE,draw.segment = TRUE )
t1

t2 <- stars(data[,c(1,11,12)],key.loc = c(15,1.5), flip.labels = FALSE,draw.segment = TRUE )
t2

t3 <- stars(data[,c(3,9,11)],key.loc = c(15,1.5), flip.labels = FALSE,draw.segment = TRUE )


palette("default") #regresar la paleta

terciles <- quantile(data$nino12,c(.33,.66))

data$nino12
#Creamos categorias de una variable q servirá como base de comparación
data2 <- mutate(data,Clasesnino12 = case_when(data$nino12 < terciles[1] ~ "Menor",
                                 data$nino12 > terciles[1] & data$nino12 < terciles[2] ~ "Normal",
                                 data$nino12 > terciles[2] ~ "Alto"))

coplot(mei ~ oni | as.factor(data2$Clasesnino12), data = data2,panel = panel.smooth, rows=1 )


#Inicio de comparación con la lluvia 

lluvia <- read.csv(file = "Data.csv", header = TRUE)


Arenal <- tibble(Mes = lluvia$Mes,Year = lluvia$Year,precip = lluvia$Arenal)
Arenal

meses <- c('agnos','January','February','March','April','May','June','July','August','September','October','November','December')
#leyendo los datos como tibble

Ventanas <- as.tibble(read.csv(file = "Estaciones/Ventanas.csv"))
colnames(Ventanas)<-meses



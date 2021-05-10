library("tidyverse")
library("reticulate")

setwd("D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia")

#py_run_file("Indices_Mensuales.py")

data_jan <- read.csv(file = "./Mensuales_estaciones/January_Toro1", header = TRUE) %>% drop_na()
data_feb <- read.csv(file = "./Mensuales_estaciones/February_Toro1", header = TRUE) %>% drop_na()
data_mar <- read.csv(file = "./Mensuales_estaciones/March_Toro1", header = TRUE) %>% drop_na()
data_apr <- read.csv(file = "./Mensuales_estaciones/April_Toro1", header = TRUE) %>% drop_na()
data_may <- read.csv(file = "./Mensuales_estaciones/May_Toro1", header = TRUE) %>% drop_na()
data_jun <- read.csv(file = "./Mensuales_estaciones/June_Toro1", header = TRUE) %>% drop_na()
data_jul <- read.csv(file = "./Mensuales_estaciones/July_Toro1", header = TRUE) %>% drop_na()
data_aug <- read.csv(file = "./Mensuales_estaciones/August_Toro1", header = TRUE) %>% drop_na()
data_sep <- read.csv(file = "./Mensuales_estaciones/September_Toro1", header = TRUE) %>% drop_na()
data_oct <- read.csv(file = "./Mensuales_estaciones/October_Toro1", header = TRUE) %>% drop_na()
data_nov <- read.csv(file = "./Mensuales_estaciones/November_Toro1", header = TRUE) %>% drop_na()
data_dec <- read.csv(file = "./Mensuales_estaciones/December_Toro1", header = TRUE) %>% drop_na()


nombre_mes <- c('anno','January','February','March','April','May','June','July','August','September','October','November','December')

#Toro1

lluvia_Toro1 <- read.csv(file = "./Estaciones/Toro1.csv", header = FALSE)#%>% drop_na()
colnames(lluvia_Toro1) <- nombre_mes 

data_rev_enero <- left_join(x = data_jan, y = lluvia_Toro1[c("anno","January")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 1)
colnames(data_rev_enero)[14] <- 'lluvia'
colnames(data_rev_enero)[1] <- 'year'

data_rev_febrero <- left_join(x = data_feb, y = lluvia_Toro1[c("anno","February")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 2)
colnames(data_rev_febrero)[14] <- 'lluvia'
colnames(data_rev_febrero)[1] <- 'year'

data_rev_marzo <- left_join(x = data_mar, y = lluvia_Toro1[c("anno","March")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 3)
colnames(data_rev_marzo)[14] <- 'lluvia'
colnames(data_rev_marzo)[1] <- 'year'
head(data_rev_marzo)

data_rev_abril <- left_join(x = data_apr, y = lluvia_Toro1[c("anno","April")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 4)
colnames(data_rev_abril)[14] <- 'lluvia'
colnames(data_rev_abril)[1] <- 'year'

data_rev_mayo <- left_join(x = data_may, y = lluvia_Toro1[c("anno","May")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 5)
colnames(data_rev_mayo)[14] <- 'lluvia'
colnames(data_rev_mayo)[1] <- 'year'


data_rev_junio <- left_join(x = data_jun, y = lluvia_Toro1[c("anno","June")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 6)
colnames(data_rev_junio)[14] <- 'lluvia'
colnames(data_rev_junio)[1] <- 'year'

data_rev_julio <- left_join(x = data_jul, y = lluvia_Toro1[c("anno","July")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 7)
colnames(data_rev_julio)[14] <- 'lluvia'
colnames(data_rev_julio)[1] <- 'year'

data_rev_agosto <- left_join(x = data_aug, y = lluvia_Toro1[c("anno","August")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 8)
colnames(data_rev_agosto)[14] <- 'lluvia'
colnames(data_rev_agosto)[1] <- 'year'

data_rev_setiembre <- left_join(x = data_sep, y = lluvia_Toro1[c("anno","September")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 9)
colnames(data_rev_setiembre)[14] <- 'lluvia'
colnames(data_rev_setiembre)[1] <- 'year'

data_rev_octubre <- left_join(x = data_oct, y = lluvia_Toro1[c("anno","October")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 10)
colnames(data_rev_octubre)[14] <- 'lluvia'
colnames(data_rev_octubre)[1] <- 'year'

data_rev_noviembre <- left_join(x = data_nov, y = lluvia_Toro1[c("anno","November")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 11)
colnames(data_rev_noviembre)[14] <- 'lluvia'
colnames(data_rev_noviembre)[1] <- 'year'

data_rev_diciembre <- left_join(x = data_dec, y = lluvia_Toro1[c("anno","December")], by = c("X0"="anno")) %>% drop_na() %>% mutate(Mes = 12)
colnames(data_rev_diciembre)[14] <- 'lluvia'
colnames(data_rev_diciembre)[1] <- 'year'

data <- rbind(data_rev_enero,data_rev_febrero, data_rev_marzo, data_rev_abril, data_rev_mayo, data_rev_junio, data_rev_julio, data_rev_agosto, data_rev_setiembre, data_rev_octubre, data_rev_noviembre, data_rev_diciembre)

data <- arrange(data, year, Mes) 
data<- mutate(data, index = paste(data$year, data$Mes, sep = "_") )

data <- data[,c(16,1,15,2,3,4,5,6,7,8,9,10,11,12,13,14)]


data_Toro1 <- data %>% 
  group_by(Mes) %>% 
  mutate(prom = mean(lluvia), ind = factor(ifelse(lluvia < mean(lluvia), 0, 1))) %>% 
  ungroup()

write.csv(data_Toro1, file = 'Toro1.csv', row.names = FALSE)

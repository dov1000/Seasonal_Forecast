#py_run_file("Indices_Mensuales.py")

source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Angostura.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Ventanas.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Arenal.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Cachi.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Cariblanco.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Penas.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Pirris.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Reventazon.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_RioMacho.R')
source('D:/OneDrive - Instituto Costarricense de Electricidad/RelacionIndicesLluvia/Mensuales_estaciones/Data_Toro1.R')

rm(list=ls())

invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))


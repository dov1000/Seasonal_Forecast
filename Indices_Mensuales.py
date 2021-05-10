


import numpy as np
import pandas as pd 
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import datetime as dt
import matplotlib.pyplot as plt


#Ubicación de los datos
path_estaciones = './Estaciones/'
path_ind = './Indices/'
path_save = './Mensuales_estaciones/'
list_estaciones = ['Angostura','Arenal','Cachi','Cariblanco','Penas','Pirris','Reventazon','RioMacho','Toro1','Ventanas']

for estacion in list_estaciones: 
    #Datos de lluvia mensuales de una estación
    p_name= path_estaciones+estacion+'.csv'
    Data = pd.read_csv(p_name, header = None, index_col=0).dropna(axis = 0, how = 'any')

    AMO = pd.read_csv(path_ind+'AMO.txt',na_values = -99.99, index_col = 0, header = None)
    CAR = pd.read_csv(path_ind+'CAR.txt',na_values = -99.99, index_col = 0, header = None)
    MEI = pd.read_csv(path_ind+'MEI.txt',na_values = -99.99, index_col = 0, header = None)
    Nino3 = pd.read_csv(path_ind+'Nino3.txt',na_values = -99.99, index_col = 0, header = None)
    Nino4 = pd.read_csv(path_ind+'Nino4.txt',na_values = -99.99, index_col = 0, header = None)
    Nino12 = pd.read_csv(path_ind+'Nino12.txt',na_values = -99.99, index_col = 0, header = None)
    Nino34 = pd.read_csv(path_ind+'Nino34.txt',na_values = -99.99, index_col = 0, header = None)
    ONI = pd.read_csv(path_ind+'ONI.txt',na_values = -99.99, index_col = 0, header = None)
    TNA = pd.read_csv(path_ind+'TNA.txt',na_values = -99.99, index_col = 0, header = None)
    tni = pd.read_csv(path_ind+'tni.txt',na_values = -99.99, index_col = 0, header = None)
    NAO = pd.read_csv(path_ind+'NAO.txt',na_values = -99.99, index_col = 0, header = None)
    AO = pd.read_csv(path_ind+'AO.txt',na_values = -99.99, index_col = 0, header = None)


    month = []
    for i in range(1,13):
        month.append(dt.date(2008, i, 1).strftime('%B'))


    Data.columns, AMO.columns, CAR.columns, MEI.columns, Nino4.columns, Nino3.columns, Nino12.columns, Nino34.columns, ONI.columns, TNA.columns, tni.columns, NAO.columns, AO.columns = month,month,month,month,month,month,month,month,month,month,month,month,month


    # In[ ]:

    
    #Guardas todos los meses
    for mes in month: 
        indices_matriz = pd.concat([MEI[mes].rename('mei'),ONI[mes].rename('oni'),AMO[mes].rename('amo'),CAR[mes].rename('car'),Nino3[mes].rename('nino3'),Nino12[mes].rename('nino12'),Nino4[mes].rename('nino4'),Nino34[mes].rename('nino34'),TNA[mes].rename('tna'),tni[mes].rename('tni'),NAO[mes].rename('nao'),AO[mes].rename('ao')], axis = 1)
        indices_matriz.to_csv(path_save+str(mes)+'_'+estacion)


        
        
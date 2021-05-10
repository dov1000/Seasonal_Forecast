#!/usr/bin/env python
# coding: utf-8

# In[ ]:


import numpy as np
import pandas as pd 
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import datetime as dt
import matplotlib.pyplot as plt


# In[ ]:


#Ubicación de los datos
path_estaciones = 'C:/Users/daoban/Documents/RelacionIndicesLluvia/Estaciones/'
path_ind = 'C:/Users/daoban/Documents/RelacionIndicesLluvia/Indices/'


# In[ ]:


#Datos de lluvia mensuales de una estación
Data = pd.read_csv(path_estaciones+'Angostura.csv', header = None, index_col=0).dropna(axis = 0, how = 'any')


# In[ ]:


#Cargo Índices
AMO = pd.read_csv(path_ind+'AMO.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
CAR = pd.read_csv(path_ind+'CAR.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
MEI = pd.read_csv(path_ind+'MEI.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
Nino3 = pd.read_csv(path_ind+'Nino3.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
Nino4 = pd.read_csv(path_ind+'Nino4.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
Nino12 = pd.read_csv(path_ind+'Nino12.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
Nino34 = pd.read_csv(path_ind+'Nino34.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
ONI = pd.read_csv(path_ind+'ONI.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
TNA = pd.read_csv(path_ind+'TNA.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
tni = pd.read_csv(path_ind+'tni.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
NAO = pd.read_csv(path_ind+'NAO.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')
AO = pd.read_csv(path_ind+'AO.txt',na_values = -99.99, index_col = 0, header = None).dropna(axis = 1, how = 'all').dropna(axis = 0, how = 'any')


# In[ ]:


month = []
for i in range(1,13):
    month.append(dt.date(2008, i, 1).strftime('%B'))


# In[ ]:


Data.columns, AMO.columns, CAR.columns, MEI.columns, Nino4.columns, Nino3.columns, Nino12.columns, Nino34.columns, ONI.columns, TNA.columns, tni.columns, NAO.columns, AO.columns = month,month,month,month,month,month,month,month,month,month,month,month,month


# In[ ]:


#Guardas todos los meses
for mes in month: 
    indices_matriz = pd.concat([MEI[mes].rename('mei'),ONI[mes].rename('oni'),AMO[mes].rename('amo'),CAR[mes].rename('car'),Nino3[mes].rename('nino3'),Nino12[mes].rename('nino12'),Nino4[mes].rename('nino4'),Nino34[mes].rename('nino34'),TNA[mes].rename('tna'),tni[mes].rename('tni'),NAO[mes].rename('nao'),AO[mes].rename('ao')], axis = 1)
    indices_matriz.to_csv(str(mes))


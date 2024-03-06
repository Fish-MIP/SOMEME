#!/usr/bin/python

#Extracting TCB data for the Southern Ocean
#Author: Denisse Fierro Arcos
#Date: 2024-03-05

#Libraries
import xarray as xr
import pandas as pd
from glob import glob
import os
import re

#Base input folder
base_folder = '/work/bb0820/ISIMIP/ISIMIP3b/OutputData/marine-fishery_global/'
#Get list of TCB files
list_files = glob(os.path.join(base_folder, "*/*/*/*_tcb_*"))

#Base output folder
folder_out = 'SOMEME'
os.makedirs(folder_out, exist_ok = True)

#Loop through each file
for f in list_files:
  #We will use the same file name, but we will save as csv instead of netcdf
  base_out = os.path.basename(f).replace('global', 'southern-ocean').\
  replace('.nc', '.csv')
  #Open dataset
  ds = xr.open_dataset(f).tcb
  #Save dataset attributes as pandas data frame
  attrs = pd.DataFrame(data = ds.attrs, index = [0])
  #Extract data for the Southern Ocean (30-90S)
  ds = ds.sel(lat = slice(-30, -90))
  #Drop time from "time" dimension (i.e., keep date only)
  try:
    t = pd.to_datetime(ds.indexes['time'].to_datetimeindex()).\
    strftime('%Y-%m-%d')
  except:
    t = pd.to_datetime(ds.indexes['time']).strftime('%Y-%m-%d')
  #Change time dimension with newly formatted date
  ds['time'] = t
  #Turn data array to data frame
  ds = ds.to_series().reset_index()
  #Add attributes to data frame
  ds = pd.concat([attrs, ds])
  #Path out
  file_out = os.path.join(folder_out, base_out)
  #Save file
  ds.to_csv(file_out, index = False)
  

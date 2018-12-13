#!/usr/bin/python3
# -*- coding: utf-8 -*-

# practice using converters + pandas to extract and clean data from csv

# Imports
import numpy as np
import pandas as pd

#----------
#Converters to clean missing values
def to_binary(s):
    try:
        if(s == "t"):
            return(float(1.0))
        elif(s == "f"):
            return(float(0.0))
    except ValueError:
        return(np.nan)

def to_price(s):
    s_ = s.replace('$','')
    try:
        return float(s_)
    except ValueError:
        return(np.nan)
    
def to_float(s):
    try:
        return float(s)
    except ValueError:
        return(np.nan)

#----------
# Extract data from csv
file_name="test.csv"
data_col = ['host_is_superhost','host_total_listings_count']
data_converter = {'host_is_superhost':to_binary,
                  'host_total_listings_count':to_float}
target_col = ['price']
target_converter = {'price':to_price}

#data
data = pd.read_csv(file_name,
  header=0,
  usecols=data_col,
  converters=data_converter,
  skipinitialspace=True)

#target
target = pd.read_csv(file_name,
  header=0,
  usecols=target_col,
  converters=target_converter,
  skipinitialspace=True)

print(data)
print(target)
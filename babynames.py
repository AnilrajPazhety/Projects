# -*- coding: utf-8 -*-
"""
Created on Sun Mar 05 08:01:52 2017

@author: AnilNeha
"""

import pandas as pd
import numpy as np

#Read files:
     
train = pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/Python/Projects/Bigmart/train.csv")
train[:5]
train.describe()
train.ix[0]

mean_sales= train.pivot_table('Item_Outlet_Sales',index='Outlet_Type',columns ='Outlet_Size',aggfunc='mean')
mean_sales

train_size=train.groupby('Outlet_Size').size()

amean_sales = train.ix[active_index]

amean_sales.groupby('Outlet_Type').size()

train_top = train.sort_index(by='Item_Outlet_Sales',ascending=False)

train_top[::-1][:5]

##Baby Names

names1880= pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/Python/names/yob1880.txt",names=['name', 'sex', 'births'])


##Total Numnber of births
names1880.groupby('sex').births.sum()

##2015 is last available year 
years =range(1880,2015)

pieces = []
columns = ['name', 'sex', 'births']

for year in years:
     path="C:/Users/AnilNeha/Desktop/MSBAPM/Spring/Python/names/yob%d.txt" % year
     df=pd.read_csv(path,names=columns)
     df['year'] =year
     pieces.append(df)

names= pd.concat(pieces,ignore_index=True)
names.head()

tb_year_sex= names.pivot_table("births",index="year",columns="sex",aggfunc="mean")

tb_year_sex.tail()

def add_grp(group):
     births=group.births.astype(float)
     group['prop'] =births/births.sum()
     return group
     
names= names.groupby(['year','sex']).apply(add_grp)
names.tail()

np.allclose(names.groupby(['year','sex']).prop.sum(),1)

def get_top1000(group):
     return group.sort_index(by='births',ascending='False')[:1000]

grouped = names.groupby(['year','sex'])
top1000 = grouped.apply(get_top1000)
top1000[:10]
boys = top1000[top1000.sex=='M']
girls = top1000[top1000.sex=='F']

total_births_names= top1000.pivot_table('births',index= 'year',columns='name',aggfunc="sum")

subset= total_births_names[['John','Harry','Mary','Marilyn']]
subset.plot(subplots=True,grid=False,title ="Number of births",figsize=(12,10))

table= top1000.pivot_table('prop',index='year',columns='sex',aggfunc=sum)
table.plot()

df= boys[boys.year==2010]

prop_cumsum= df.sort_index(by='prop',ascending = False).prop.cumsum()
prop_cumsum.searchsorted(0.0002)

def get_quantile_count(group,q=0.5):
     group= group.sort_index(by='prop',ascending=False)
     return group.prop.cumsum().searchsorted(q) +1

diversity = top1000.groupby(['year','sex']).apply(get_quantile_count)

diversity = diversity.unstack('sex')































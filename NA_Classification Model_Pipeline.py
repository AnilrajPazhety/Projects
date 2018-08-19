# -*- coding: utf-8 -*-
"""
Created on Fri Jun 08 10:43:20 2018

@author: anilraj.pazhety
"""


##Update the below variable with file path to all the files
path="C:/Users/anilraj.pazhety/Desktop/gcp-2018/q2/clustering/na/final"

##Update the filenae containing all records to score
new_data="GCP_Cluster_Analysis_AllData_2.csv"


##Pipeline for classifying new companies into clusters(A,B,C,D)##

#Import required libraries
from __future__ import print_function
import pandas as pd
import numpy as np
import pylab as pl
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import pyplot
from mpl_toolkits.mplot3d import Axes3D
from sklearn import cross_validation
from sklearn.externals import joblib
from sklearn import preprocessing
from sklearn.cross_validation import  train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.decomposition import PCA
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score, precision_score
import xgboost as xgb
from sklearn.grid_search import GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Imputer
from sklearn.manifold import TSNE
import seaborn as sns
import statsmodels.api as sm
import datetime
import collections
import re


#Import required ML libraries
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.multiclass import OneVsRestClassifier
from sklearn.ensemble import  VotingClassifier


##import xgboost
import os
mingw_path = 'C:\\Program Files\\mingw-w64\\x86_64-5.3.0-posix-seh-rt_v4-rev0\\mingw64\\bin'
os.environ['PATH'] = mingw_path + ';' + os.environ['PATH']

from xgboost import XGBClassifier
import xgboost as xgb


##Helper Functions##
#function that returns a dummified DataFrame of significant dummies in a given column
# This can help us reduce the dimensionality by creating dummy columns for only the most popular categories in a column

def dum_sign(dummy_col):

    # removes the bind
    dummy_col = dummy_col.copy()

    return pd.get_dummies(dummy_col, prefix=dummy_col.name)

  
#Function assign a region based on the State .Reference: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
def state_to_region(state):
    
    if state.lower() in ['maine', 'new hampshire', 'massachusetts', 'rhode island', 'connecticut', 'new york','vermont','new jersey', 'pennsylvania']:
      return 'NorthEast'
    
    elif state.lower() in ['indiana','illinois','michigan','ohio','wisconsin','iowa','kansas','minnesota','missouri','nebraska',\
                   'north dakota','south dakota']:
      return 'MidWest'
    
    elif state.lower() in ['arizona','montana','alaska','colorado','utah','california','idaho','nevada','new mexico','wyoming',\
                   'hawaii','oregon','washington','washington dc']:
      return 'West'
    
    elif state.lower() in ['delaware','district of columbia','florida','georgia','maryland','north carolina','south carolina',\
                   'virginia','west virginia',\
                   'alabama','kentucky','mississippi','tennessee','arkansas','louisiana','oklahoma','texas']:
      return 'South'

    elif state.lower() in ['ontario','quebec','british columbia','manitoba','nova scotia','saskatchewan','new brunswick','alberta','santa catarina',\
                          'prince edward island']:
      return 'CA'
    
    else:
      return 'Others'
    
    
#US 1987 SIC 1 is variable which defines the industry - https://www.census.gov/prod/techdoc/cbp/cbp96/sic-code.pdf
#The function below decodes the industry value using the information from the above link
def sic_to_indus(code):
  
    if str(code)[:2] in ['07','08','09']:
        return 'Agr_Fish_For'
      
    elif str(code)[:2] in ['10','12','13','14']:
        return 'Mining'
      
    elif str(code)[:2] in ['15','16','17']:
        return 'Construction'
      
    elif str(code)[:2] in ['20','21','22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39']:
        return 'Manufacturing'
      
    elif str(code)[:2] in ['41','42','44','45','46','47','48','49']:
        return 'Transport'
      
    elif str(code)[:2] in ['50','51']:
        return 'Wholesale Trade'
      
    elif str(code)[:2] in ['52','53','54','55','56','57','58','59']:
        return 'Retail Trade'
      
    elif str(code)[:2] in ['60','61','62','63','64','65','67']:
        return 'Fin_Ins_RE'
      
    elif str(code)[:2] in ['70','72','73','75','76','78','79','80','81','82','83','84','86','87','89']:
        return 'Services'
    else:
        return 'Unclassified'
    
    

##Read the features set for making predictions
features = pd.ExcelFile(str(path)+"/features_na.xlsx")
features_df=features.parse('Sheet1')

cols=features_df['features'].tolist()


#Read new data
df_load = pd.read_csv(str(path)+"/"+str(new_data))

#For this first analysis only perform the clustering on accounts from the AMER region
df = df_load.loc[df_load['region'] == 'AMER']


#Define a new variable to store the region based on states
df['US_reg'] = df['State'].apply(lambda x: state_to_region(str(x).strip()))

#Filter out regions which are not in NA
df= df [df['US_reg'].isin (['NorthEast','West','South','MidWest','CA'])]
        

#Save company identifier to be appened with cluster information

saved_df=df[['unique_code', 'Domain','Website_LDC_Domain']]

df = df.drop(['Respondents','unique_code','Slug', 'Domain','Website_LDC_Domain',\
                      'Intricately URL','Company Name','Website','LinkedIn URL','City/Region2',\
                      'State/Region1','Country','Country.1','Company Name_LDC_Name','State',\
                      'Postal Code','Website_LDC_Domain','Country_LDC_Country',\
                      'GCP_NA_Score','GCP_NA_Rating','GCP_EMEA_Score','GCP_EMEA_Rating','GCP_APAC_Score','GCP_APAC_Rating',\
                       'region'], axis=1)

#Make transforamtions to SaaS Variables

df['Count of SaaS Providers']= df['SaaS Providers'].apply(lambda x: len(str(x).split(';')))

saas_fin=['Google Tag Manager', 'Google Site Verification', 'Skype for Business', 'Office 365', 'Microsoft Exchange Online', 'Google APIs', 'Facebook', 'Google Analytics', 'DoubleClick', 'Wordpress', 'G Suite']

#Create dummy variable to indicate the presence of SaaS provider (One hot encoding done manually)
for i in saas_fin:
    df[i]=df['SaaS Providers'][df['SaaS Providers'].notnull()].apply(lambda x : 1 if i.lower() in str(x).lower() else 0)

##DNS values

##Read the features set for making predictions
dns = pd.ExcelFile(str(path)+"/dns_na.xlsx")
dns_df=dns.parse('Sheet1')

dns_fin=dns_df[0].tolist()

#Create a empty list to store dns names
dns_provider_lst=[]

#Loop each string in DNS provider column and split using ';' to identify individual dns providers
for i in df.index.tolist():
    try:
        dns_provider_lst = df.loc[i,'DNS Providers'].split(';')
        for j in dns_provider_lst:
            if bool(j.strip() in dns_fin):
                df.loc[i,j]=1
            else:
                df.loc[1,j]=0
    except:
        pass

#Employee Range v value needs to be cleansed


#Some values needs transformations  10-Jan --> 1-10 ,Nov-50 --> 11-50

ind1=df['Employee Range']=='10-Jan'
df.ix[ind1,'Employee Range']='1-10'


ind2=df['Employee Range']=='Nov-50'
df.ix[ind2,'Employee Range']='11-50'

##Some rows have values 0 and 0.0 .Replace 0.0 with 0

ind3=df['Employee Range']=='0.00'
df.ix[ind3,'Employee Range']='0'

##Some rows have values 43110.00  and 18568.00 .Replace these with >10000

ind4=df['Employee Range']=='18568.00'
df.ix[ind4,'Employee Range']='>10,000'


ind5=df['Employee Range']=='43110.00'
df.ix[ind5,'Employee Range']='>10,000'

#Create a new variable to store the decode industry from the sic code
df['sic_industry'] =df['US 1987 SIC 1'].apply(lambda x : sic_to_indus(x))


#Drop columns - 'sic_industry','Line of Business','Industry','Hosting Providers' ,'DNS Providers','SaaS Providers','US 1987 SIC 1' as we have 
#already use their info
df.drop(['sic_industry','Line of Business','Industry','Hosting Providers' ,'DNS Providers','SaaS Providers','US 1987 SIC 1'], axis=1, inplace=True)


#List of categorical column names that we need to create dummy columns for:
cat_list = ['Monthly Spend Total', 'Primary Hosting Provider', 'Primary Hosting Provider Monthly Spend','Secondary Hosting Provider','Secondary Hosting Provider Monthly Spend','Hosting Monthly Spend' ,'Primary DNS Provider','Primary DNS Provider Monthly Spend',\
            'DNS Monthly Spend','Revenue Range','Employee Range','US_reg' ,'Industry_LDC_PrimaryIndustry','Data Theft','Cloud Storage','Security Monitoring','Data Visualization','Employees']

#Loop through the categorical column names
#run each column through the dum_sign function

for column in cat_list:
        dummy_cols = dum_sign(df[column])
        df = pd.concat([df, dummy_cols], axis=1)
        df = df.drop(column, axis=1)

     
#Identify columns which donot exist in new data but exists in training data and will be required for predictions.
#This may happen as we are converting the variables into dummies and some values many not exist in new data.This issue can be fixed by setting values to 0
miss_cols= list(set(cols) - set(df.columns))

#Set each missing col to 0 for all records
for i in miss_cols:
    df[i] = 0

df=df.apply(pd.to_numeric, errors='coerce')

#Subset the required columns into a new dataframe
temp=df[cols]

#Fill missing values with 0
temp= temp.fillna(0)

for i in temp.columns.tolist():
    temp[i].astype('int')

#Rename colummns which contains as xgb doesnot allow symbols like > or <
 
temp.rename(columns={'Monthly Spend Total_G) < $1,000': 'Monthly Spend Total_G) lt $1,000', 'Primary Hosting Provider Monthly Spend_G) < $1,000': 'Primary Hosting Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'Secondary Hosting Provider Monthly Spend_G) < $1,000':  'Secondary Hosting Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'Primary DNS Provider Monthly Spend_G) < $1,000':  'Primary DNS Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'Hosting Monthly Spend_G) < $1,000':  'Hosting Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'DNS Monthly Spend_G) < $1,000':  'DNS Monthly Spend_G) lt $1,000'}, inplace=True)

temp.rename(columns={ 'Primary CDN (Content Delivery) Provider Monthly Spend_G) < $1,000':  'Primary CDN (Content Delivery) Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'GTM (Traffic Management) Monthly Spend_G) < $1,000':  'GTM (Traffic Management) Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'Employee Range_>10,000':  'Employee Range_gt 10,000'}, inplace=True)
temp.rename(columns={ 'Primary GTM (Traffic Management) Provider Monthly Spend_G) < $1,000':  'Primary GTM (Traffic Management) Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'Secondary DNS Provider Monthly Spend_G) < $1,000':  'Secondary DNS Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'Secondary CDN (Content Delivery) Provider Monthly Spend_G) < $1,000':  'Secondary CDN (Content Delivery) Provider Monthly Spend_G) lt $1,000'}, inplace=True)
temp.rename(columns={ 'CDN (Content Delivery) Monthly Spend_G) < $1,000':  'CDN (Content Delivery) Monthly Spend_G) lt $1,000'}, inplace=True)
           

    
##Mod
    
#Load the pickles files 
fin_model = joblib.load(str(path)+'/na_model.pkl')



#Make predictions on test set
z_pred = fin_model.predict(temp)
z_df=pd.DataFrame(z_pred,columns=['Cluster Membership'])

#Append cluster membership to saved_df

saved_df=saved_df.reset_index(drop=True)
z_df=z_df.reset_index(drop=True)


res_df = pd.concat([saved_df, z_df], axis=1,join_axes=[saved_df.index])


##Write to excel
writer = pd.ExcelWriter(str(path)+"/NA_Cluster_Classification_Output.xlsx")
res_df.to_excel(writer,'Sheet1',index=False)
writer.save()



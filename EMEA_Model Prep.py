# -*- coding: utf-8 -*-
"""
Created on Thu Jun 14 08:57:41 2018

@author: anilraj.pazhety
"""

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
from sklearn import preprocessing
from sklearn.cluster import KMeans
from sklearn.cross_validation import  train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.decomposition import PCA
from sklearn.metrics import classification_report, confusion_matrix, accuracy_score, precision_score
import xgboost as xgb
from sklearn.grid_search import GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Imputer
from sklearn.mixture import GMM
from sklearn.manifold import TSNE
import seaborn as sns
import statsmodels.api as sm
import datetime
import collections
import re


#function that returns a dummified DataFrame of significant dummies in a given column
# This can help us reduce the dimensionality by creating dummy columns for only the most popular categories in a column

def dum_sign(dummy_col, threshold=0.1):

    # removes the bind
    dummy_col = dummy_col.copy()

    # what is the ratio of a dummy in whole column
    count = pd.value_counts(dummy_col) / len(dummy_col)

    # cond whether the ratios is higher than the threshold
    mask = dummy_col.isin(count[count > threshold].index)

    # replace the ones which ratio is lower than the threshold by a special name
    dummy_col[~mask] = "others"

    return pd.get_dummies(dummy_col, prefix=dummy_col.name)

  
  
    
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
    
    
def model_metrics(model,X_test,y_test):
  
  #Make predictions using model
  y_pred = model.predict(X_test) 
  preds_df=pd.DataFrame(y_pred)

  #Check the model performance in terms of precision and recall
  conf_mat=confusion_matrix(y_test,preds_df[0])
  
  #print (conf_mat)
  #Print classification report
  print(metrics.classification_report(y_test,preds_df[0]))
  
  #Print Accuracy Score
  print ("Accuracy of the model is : %f"%metrics.accuracy_score(y_test,preds_df[0]))
  
  
#Read data from master data file

df_load = pd.read_csv("C:/Users/anilraj.pazhety/Desktop/Gcp-2018/q2/Clustering/EMEA/Final/GCP_Cluster_Analysis_AllData_2 .csv")

#Filter dataset down to AMER region only

#For this first analysis only perform the clustering on accounts from the AMER region
#We will go back through and run the other regions seperately
df = df_load.loc[df_load['region'] == 'EMEA']
df.shape


#Remove outliers that end up creating their own clusters and therefore not valuable to the analysis
#For example, in the EMEA region we found that Wordpress.com and Wordpress.org were creating their own cluster
outlier_list = ['11P20089', '13S75557', 'Z8YCVDV3B', '184IEDUL', 'KJ8BEJ7F', 'KPHGJE23', '1Z0XPYW3', '18LD80AL', '18G06927', '7LAYGCXN', '4S8VFLE9', 'ECYTDG68', 'RKGPVC', 'NAFQEU', '2UCWAT', 'Y4QNVA', 'MXGA7H', '18187096',
 'ZVAC7GH6', '1ZXUL0GA', '18UYBCNW', 'KDP3XXX3', 'KAS7NIKR', 'KO0IVFAK', 'KIAFGATR', '2Y8HBPFY', 'KG6XWE3C', 'KKZFWSWF', '4GF5DAQW', 'K0ZA23CT', 'ZMC6SQA3', '272X7NF2', 'KOGPQVQV', '5FBEPG2L', '2MCEYBES',
 '25ND9QZG', 'K5RPX4VT', '2T7B69BG', '2YUJ92PE', 'DVZETGDY', 'ECQU9NQA', '6ZSH9LC2', '24TB93EG', '2ZLCYVKF', '184IEDUL', 'KDP3XXX3', '18187096', '6ZSH9LC2', '24TB93EG', '2ZLCYVKF', 'KNFMRHEF', 'KDP3XXX3',
 '273J9XC3', '6ZSH9LC2', '24TB93EG', 'DVZETGDY', '2ZLCYVKF', 'KDP3XXX3', '6ZSH9LC2', '24TB93EG', '2ZLCYVKF']

df = df.drop(df[df['unique_code'].isin(outlier_list)].index)


##Remove duplicates based on unique codes
df=df.drop_duplicates('unique_code',keep='first')

#Check if the state variable needs any transformation to regions.The states which occur once can be treated as outliers

vc=df['State'].value_counts() 

#Creating a list to find out states which occur only once

states_2 =vc[vc>1].index.tolist()

#Filter records with states in list created above

df=df[df['State'].isin(states_2)]

df.shape



#Save the important columns so we can append them later and link back to the original records
saved_cols = df[['Respondents','unique_code','Domain','GCP_NA_Score','GCP_NA_Rating','GCP_EMEA_Score','GCP_EMEA_Rating','GCP_APAC_Score','GCP_APAC_Rating','opp_stage_name',\
                 'pipeline','region']]




#Remove all the unnecessary columns that will negatively affect the analysis

df = df.drop(['Respondents','unique_code','Slug', 'Domain','Website_LDC_Domain',\
                      'Intricately URL','Company Name','Website','LinkedIn URL','City/Region2',\
                      'State/Region1','Country','Country.1','Company Name_LDC_Name','State',\
                      'Postal Code','Website_LDC_Domain','Country_LDC_Country',\
                      'GCP_NA_Score','GCP_NA_Rating','GCP_EMEA_Score','GCP_EMEA_Rating','GCP_APAC_Score','GCP_APAC_Rating',\
                       'region'], axis=1)
        


##Filter out columns with missing values > 40%

cols_without_nas = df.columns[df.isnull().mean() < 0.6]
                             
#Save the features to excel
features_df=pd.DataFrame(cols_without_nas) 
##Write to excel
writer = pd.ExcelWriter("C:/Users/anilraj.pazhety/Desktop/gcp-2018/q2/clustering/emea/final/features_emea.xlsx")
features_df[0].to_excel(writer,'Sheet1')
writer.save()


df= df[cols_without_nas]                   
#Create a copy of the existing  dataframe before making changes
df_nas = df

df_nas.shape


#Handle SaaS Providers##

#SaaS Providers - By examining some records it can be observed that the count of unique values for this variable is pretty high.
#Hence,we can create a new variable which stores the number of SaaS Providers 

df_nas['Count of SaaS Providers']= df_nas['SaaS Providers'].apply(lambda x: len(str(x).split(';')))

#SaaS providers - An list of unique SaaS providers can be generated and it can be converted into a dummy variable

#Create a empty list to store dns names
saas_provider_lst=[]

#Loop each string in DNS provider column and split using ';' to identify individual dns providers
for i in df_nas.index.tolist():
    try:
        saas_provider_lst.extend(df_nas.loc[i,'SaaS Providers'].split(';'))
        
    except:
        pass

#check the number of unique list of Saas providers
saas_uni = list(set(saas_provider_lst))
len(saas_uni)

#Extract the count for each saas provider
saas_dict = {}
for i in saas_uni:
    saas_dict[i]= saas_provider_lst.count(i)
    
saas_fin=[]
for i in saas_dict:
    if saas_dict[i] > 5000:
        saas_fin.append(i.strip())

#Verify the records in saas_fin  
print(saas_fin)

#Save the saaas features to excel
saas_df=pd.DataFrame(saas_fin) 
##Write to excel
writer = pd.ExcelWriter("C:/Users/anilraj.pazhety/Desktop/gcp-2018/q2/clustering/emea/final/saas_emea.xlsx")
saas_df[0].to_excel(writer,'Sheet1')
writer.save()


#Create dummy variable to indicate the presence of SaaS provider (One hot encoding done manually)
for i in saas_fin:
    df_nas[i]=df_nas['SaaS Providers'][df_nas['SaaS Providers'].notnull()].apply(lambda x : 1 if i in x else 0)

##Handle DNS Providers##
#DNS providers - An list of unique DNS providers can be generated and it can be converted into a dummy variable

#Create a empty list to store dns names
dns_provider_lst=[]

#Loop each string in DNS provider column and split using ';' to identify individual dns providers
for i in df_nas.index.tolist():
    try:
        dns_provider_lst.extend(df_nas.loc[i,'DNS Providers'].split(';'))
    except:
        pass
    
#check the number of unique list of dns providers
dns_uni = list(set(dns_provider_lst))
len(dns_uni)


#Extract the count for each dns provider
dns_dict = {}
for i in dns_uni:
    dns_dict[i]= dns_provider_lst.count(i)
    
#Filter DNS providers with more than 50 occurences across the data set
dns_fin=[]
for i in dns_dict:
    if dns_dict[i] > 75:
        dns_fin.append(i.strip())


#Create dummy variable to indicate the presence of DNS provider (One hot encoding done manually)
for i in dns_fin:
    df_nas[i]=df_nas['DNS Providers'][df_nas['DNS Providers'].notnull()].apply(lambda x : 1 if i in x else 0)

    
    #Save the DNS features to excel
dns_df=pd.DataFrame(dns_fin) 

##Write to excel
writer = pd.ExcelWriter("C:/Users/anilraj.pazhety/Desktop/gcp-2018/q2/clustering/emea/final/dns_emea.xlsx")
dns_df[0].to_excel(writer,'Sheet1')
writer.save()

#Create a new variable to store the decode industry from the sic code
df_nas['sic_industry'] =df_nas['US 1987 SIC 1'].apply(lambda x : sic_to_indus(x))

#Employee Range v value needs to be cleansed


#Check for any values which are not correct
df_nas['Employee Range'].value_counts()

#Some values needs transformations  10-Jan --> 1-10 ,Nov-50 --> 11-50

ind1=df_nas['Employee Range']=='10-Jan'
df_nas.ix[ind1,'Employee Range']='1-10'


ind2=df_nas['Employee Range']=='Nov-50'
df_nas.ix[ind2,'Employee Range']='11-50'

##Some rows have values 0 and 0.0 .Replace 0.0 with 0

ind3=df_nas['Employee Range']=='0.00'
df_nas.ix[ind3,'Employee Range']='0'

##Some rows have values 43110.00  and 18568.00 .Replace these with >10000

ind4=df_nas['Employee Range']=='18568.00'
df_nas.ix[ind4,'Employee Range']='>10,000'


ind5=df_nas['Employee Range']=='43110.00'
df_nas.ix[ind5,'Employee Range']='>10,000'

#Drop columns - 'sic_industry','Line of Business','Industry','Hosting Providers' ,'DNS Providers','SaaS Providers','US 1987 SIC 1' as we have 
#already use their info
#df_nas.drop(['sic_industry','Line of Business','Industry','Hosting Providers' ,'DNS Providers','SaaS Providers','US 1987 SIC 1','Google Cloud Platform'], axis=1, inplace=True)
df_nas.drop(['sic_industry','Line of Business','Industry','Hosting Providers' ,'DNS Providers','SaaS Providers','US 1987 SIC 1'], axis=1, inplace=True)

#Drop one of the variables from the above list which show high correlation

cols = df_nas.columns.tolist()

cols.remove('Count of SaaS Providers')
cols.remove('DoubleClick')
cols.remove('1&1 Internet')
cols.remove('Provider Count')
cols.remove('Microsoft Exchange Online')

##Create Dummy Variables for Categorical Columns##

#List of categorical column names that we need to create dummy columns for:
#I went through the CSV and looked at each column, this should be the full list
cat_list = ['Monthly Spend Total', 'Primary Hosting Provider', 'Primary Hosting Provider Monthly Spend','Secondary Hosting Provider','Secondary Hosting Provider Monthly Spend','Hosting Monthly Spend' ,'Primary DNS Provider','Primary DNS Provider Monthly Spend',\
            'DNS Monthly Spend','Revenue Range','Employee Range','US_reg' ,'Industry_LDC_PrimaryIndustry','Data Theft','Cloud Storage','Security Monitoring','Data Visualization','Employees']


for column in cat_list:
        dummy_cols = dum_sign(df_nas[column], threshold=0.03)
        df_nas = pd.concat([df_nas, dummy_cols], axis=1)
        df_nas = df_nas.drop(column, axis=1)


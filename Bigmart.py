# -*- coding: utf-8 -*-
"""
Created on Sat Feb 25 19:33:30 2017

@author: AnilNeha
"""

import pandas as pd
import numpy as np

#Read files:
     
train = pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/Python/Projects/Bigmart/train.csv")
test  = pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/Python/Projects/Bigmart/test.csv")

train['source']='train'
test['source']='test'
data = pd.concat([train, test],ignore_index=True)
print train.shape, test.shape, data.shape

data.apply(lambda x: sum(x.isnull()))

#Filter categorical variables
categorical_columns = [x for x in data.dtypes.index if data.dtypes[x]=='object']
#Exclude ID cols and source:
categorical_columns = [x for x in categorical_columns if x not in ['Item_Identifier','Outlet_Identifier','source']]
#Print frequency of categories
for col in categorical_columns:
    print '\nFrequency of Categories for varible %s'%col
    print data[col].value_counts()
    
data['source']
data.describe()

data.apply(lambda x: len(x.unique()))

#Impute data and check #missing values before and after imputation to confirm    
item_avg_weight = data.pivot_table(values='Item_Weight', index='Item_Identifier')
miss_bool = data['Item_Weight'].isnull()
#miss_bool1=data.Item_Weight.isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'Item_Weight'] = data.loc[miss_bool,'Item_Identifier'].apply(lambda x: item_avg_weight[x])
print 'Final #Missing: %d' %sum(data['Item_Weight'].isnull())

#Import mode function:
from scipy.stats import mode

outlet_size_mode = data.pivot_table(values='Outlet_Size', columns='Outlet_Type',aggfunc=(lambda x:mode(x).mode[0]) )

miss_bool = data.Outlet_Size.isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'Outlet_Size'] = data.loc[miss_bool,'Outlet_Type'].apply(lambda x : outlet_size_mode[x])

print 'Orignal #missing: %d'% sum(miss_bool)
data['Outlet_Size'].value_counts()

print 'Mode for each Outlet_Type:'
print outlet_size_mode


pd.pivot_table(data,index='Outlet_Type',values='Item_Outlet_Sales')
pd.pivot_table(data,index='Item_Type',values='Item_Visibility')


visibility_avg= pd.pivot_table(data,index='Item_Identifier',values='Item_Visibility')
miss_bool= (data['Item_Visibility']==0)

data.loc[miss_bool,'Item_Visibility'] = data.loc[miss_bool,'Item_Identifier'].apply(lambda x: visibility_avg[x])

##Check if imputation is successfull
print 'Sum of zeroes %d' % sum(data['Item_Visibility']==0)

data['Item_Visibility_meanratio'] = data.apply(lambda x: x['Item_Visibility']/visibility_avg[x['Item_Identifier']],axis=1)

##Verify the new variable created
data.Item_Visibility_meanratio.describe()

data['Item_Type_combined'] = data['Item_Identifier'].apply(lambda x: x[0:2])

data['Item_Type_combined']=data['Item_Type_combined'].map({'FD':'Food','NC':'Non-Consumable','DR':'Drinks'})

##Creating a new variable to shows number of years since establishment
data['Outlet_Years'] = 2013- data['Outlet_Establishment_Year']

##The fat content variables has repeating categories

data['Item_Fat_Content'].value_counts()
data['Item_Fat_Content'] = data['Item_Fat_Content'].replace({'LF':'Low Fat',
                                                             'reg':'Regular',
                                                             'low fat':'Low Fat'})
data.loc[data['Item_Type_combined']=='Non-Consumable','Item_Fat_Content'] = "Non Edible"

##scikit requires variables to be numeric.Hence,applying encoding on categorical variables

from sklearn.preprocessing import LabelEncoder
le= LabelEncoder()

##Storing the tranformed target in a new variable during encoding inorder to keep target variable for future comparison
data['Outlet'] = le.fit_transform(data['Outlet_Identifier'])

var_mod = ['Item_Fat_Content','Outlet_Location_Type','Outlet_Size','Item_Type_combined','Outlet_Type','Outlet']
le = LabelEncoder()
for i in var_mod:
    data[i] = le.fit_transform(data[i])

##One hot encoding
data=pd.get_dummies(data,columns=['Item_Fat_Content','Outlet_Location_Type','Outlet_Size','Outlet_Type','Item_Type_combined','Outlet'])
##Verify one hot encoding

#Drop the columns which have been converted to different types:
data.drop(['Item_Type','Outlet_Establishment_Year'],axis=1,inplace=True)

##Create train and test sets

train = data.loc[data['source']=="train"]
test = data.loc[data['source']=="test"]

##Drop unwanted columns

test.drop(['Item_Outlet_Sales','source'],axis=1,inplace=True)
train.drop(['source'],axis=1,inplace=True)

train.to_csv("trainmod.csv",index=False)
test.to_csv("testmod.csv",index=False)

##Saving the files

train.to_csv("trainmod.csv",index=False)
test.to_csv("testmod.csv",index=False)
##Model Building

##Base Model 

mean_sales= train['Item_Outlet_Sales'].mean()


base1= test[['Item_Identifier','Outlet_Identifier']]
base1['Item_Outlet_Sales']=mean_sales

base1.describe


target = 'Item_Outlet_Sales'
IDcol = ['Item_Identifier','Outlet_Identifier']

from sklearn import cross_validation, metrics

def modelfit(alg, dtrain, dtest, predictors, target, IDcol, filename):
    #Fit the algorithm on the data
    alg.fit(dtrain[predictors],dtrain[target])
    
    #Make Predictions
    dtrain_predictions = alg.predict(dtrain[predictors])
    
    #Perform Cross Validation
    cv_score= cross_validation.cross_val_score(alg,dtrain[predictors],dtrain[target],cv=10,scoring='mean_squared_error')
    cv_score=np.sqrt(np.abs(cv_score))
    
    ##Print Report
    print "\nModel Report"
    print "RMSE : %.4g" % np.sqrt(metrics.mean_squared_error(dtrain[target].values, dtrain_predictions))
    print "CV Score : Mean - %.4g | Std - %.4g | Min - %.4g | Max - %.4g" % (np.mean(cv_score),np.std(cv_score),np.min(cv_score),np.max(cv_score))
    
    #Predict on testing data:
    dtest[target] = alg.predict(dtest[predictors])
    #Export submission file:
    IDcol.append(target)
    submission = pd.DataFrame({ x: dtest[x] for x in IDcol})
    submission.to_csv(filename, index=False)

##Linear Regression
from sklearn.linear_model import LinearRegression, Ridge, Lasso
predictors = [x for x in train.columns if x not in [target]+IDcol+['Item_Type']]
# print predictors
alg1 = LinearRegression(normalize=True)
modelfit(alg1, train, test, predictors, target, IDcol, 'alg1.csv')
coef1 = pd.Series(alg1.coef_, predictors).sort_values()
coef1.plot(kind='bar', title='Model Coefficients')
    
  
##Large values of coefficients signify overfitting.Hence,we use Ridge and Lasso

alg2 = Ridge(alpha=0.05,normalize=True)
modelfit(alg2, train, test, predictors, target, IDcol, 'alg2.csv')
coef2 = pd.Series(alg2.coef_, predictors).sort_values()
coef2.plot(kind='bar', title='Model Coefficients')



##Decision Trees

from sklearn.tree import DecisionTreeRegressor
#predictors = [x for x in train.columns if x not in [target]+IDcol]
alg3 = DecisionTreeRegressor(max_depth=15, min_samples_leaf=100)
modelfit(alg3, train, test, predictors, target, IDcol, 'alg3.csv')
coef3 = pd.Series(alg3.feature_importances_, predictors).sort_values(ascending=False)
coef3.plot(kind='bar', title='Feature Importances')

##Here you can see that the RMSE is 1058 and the mean CV error is 1091. This tells us that the model is 
#slightly overfitting. Lets try making a decision tree with just top 4 variables, a max_depth of 8 and min_samples_leaf as 150.
alg4 = DecisionTreeRegressor(max_depth=5, min_samples_leaf=250)
modelfit(alg4, train, test, predictors, target, IDcol, 'alg3.csv')
coef4 = pd.Series(alg4.feature_importances_, predictors).sort_values(ascending=False)
coef4.plot(kind='bar', title='Feature Importances')

##Decision Tree Regressor
##RandomForestRegressor?
from sklearn.ensemble import RandomForestRegressor
alg5 = RandomForestRegressor(n_estimators=200,max_depth=5, min_samples_leaf=100,n_jobs=4)
modelfit(alg5, train, test, predictors, target, IDcol, 'alg5.csv')
coef5 = pd.Series(alg5.feature_importances_, predictors).sort_values(ascending=False)
coef5.plot(kind='bar', title='Feature Importances')

data[['Item_Fat_Content_0','Item_Fat_Content_1','Item_Fat_Content_2']].head(10)
data.dtypes.index
test.dtypes
data.Item_Fat_Content.value_counts()
data.Item_Identifier.describe
data.Item_Type_combined.value_counts()
data.Item_Type.describe
data.dtypes.nam
data['Outlet_Years'].describe()
data['Outlet'].value_counts()

type(data['Item_Identifier'][0:2])





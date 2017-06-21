# -*- coding: utf-8 -*-
"""
Created on Mon Jun 19 16:55:03 2017

@author: anilraj.pazhety
"""

##Importing required libaries
import os
import numpy as np
import pyodbc
import pandas as pd
import jieba
import jieba.analyse


##test mssql connection

cnxn = pyodbc.connect("DRIVER={SQL Server};SERVER=HKGAPP124;PORT=1433;DATABASE=TDB_EricMeeks_Test;Trusted_Connection=True")
cursor = cnxn.cursor()

##pipeline = joblib.load('I:/EricMeeks/gcp_sherlock/us_model_classification.pkl')

sql_query2 = "SELECT content, class FROM [TDB_EricMeeks_Test].[grabby].[log_grabby_grab_CN_model] ;"
        
##Exectue the query and load the data into a dataframe
final_data = pd.read_sql(sql_query2, cnxn)

##Name the columns of the dataframe
final_data.columns = ['text', 'class']

##Verify if the data has been loaded correctly
final_data.head()

###Extract keywords from articles which has class=1 (relevant)###

##Filter out records in the dataframe where class=1
text_ch_1 = final_data[final_data['class']==1]

##Extract the row numbers of the filtered out records
index = final_data[final_data['class']==1].index.tolist()

##Extracting top 5 words from each record

##Initialize lists and dataframes
terms=[]
weights=[]
df_keywords = pd.DataFrame()
df_keywords_final = pd.DataFrame()

##K is the variable which control the top K keywords to be extracted from each article
K=5

##Tokenize each article using jieba and output top K keywords 
for i in index:
    temp=jieba.analyse.extract_tags(text_ch_1['text'][i], topK=K, withWeight=True, allowPOS=())
    for j in range(len(temp)):
        terms.append(temp[j][0])    ##For each article update the list 'terms' with keywords
        weights.append(temp[j][1])  ##For each article update the list ''weights' with weights
    df_keywords['terms']=terms      ##Add the values of terms and weights to a new data frame
    df_keywords['weights']=weights  
    df_keywords_final = df_keywords_final.append(pd.DataFrame(data = df_keywords))##Append the values  of terms and weights  for each article
    df_keywords = pd.DataFrame()
    terms=[]
    weights=[]  

##Sort the keywords in the descending order of the weights
df_keywords_final.sort_index(by='weights', ascending=False)

##Extract top 30 keywords
top_keywords= df_keywords_final.head(30)

###Create a Document Term Matrix###

##Initialize a dataframe
df_dtm= pd.DataFrame()

#final_data1 =final_data

##Dropping records in the dataframe with Null Values
final_data = final_data.dropna(axis=0)

##Extract the row numbers from the data frame
index = final_data.index.tolist()

##Getting the list of stopwords from english language
from stop_words import get_stop_words
stop_words = get_stop_words('en')

##Tokenize each article using jieba and output top K keywords 
for i in index:
    temp=jieba.analyse.extract_tags(final_data['text'][i], topK=K, withWeight=True, allowPOS=())
    for j in range(len(temp)):
        if ~(temp[j][0].isdigit() or temp[j][0] in stop_words):
            df_dtm.loc[i,temp[j][0]]=temp[j][1] ## Create dataframe which simulates the structure of a dtm
        
       
#df_dtm.sort_index(axis=1)
##Creating a transpose matrix to convert NAs to zero
df_dtm_t=df_dtm.T
index= list(df_dtm_t.columns.values)

##Loop through each record to find missing value and converting it to zero
for i in index:
    miss_bool=df_dtm_t[i].isnull()
    df_dtm_t.ix[miss_bool,i]=0

##Converting the dataframe to the original format by creating a new dataframe
df_dtm_na=df_dtm_t.T

##Filter records with high values of weights 
##Initialize a list
lst=[]

##Subsetting columns which have high weightage
index= list(df_dtm_na.columns.values)

##Loop through each record to check if the weight for that particulat keyword is more than the given value.
##The value can be changed based on the number of predictors desired
for i in index:
    if df_dtm_na[i].sum() > 8:
        print (i)
        lst.append(i)
  
##Initialize a new data frame to store the data set to be used for modeling
df_data=pd.DataFrame()

for i in range(len(lst)):
    df_data[lst[i]] = df_dtm_na.ix[:, lst[i]]

##Create a new parameter in the df_data dataframe to hold class (0,1) values
df_data['class']=final_data['class']
 
##Assign the class values to a variable named 'target'
target=df_data['class']

##create a copy of df_data
df_data_copy=df_data

##Drop the class variable from the data frame
df_data.drop(['class'],axis=1,inplace=True)

##Modeling###

##Fit a logistic regression model on the train set
from sklearn import linear_model
clf=linear_model.LogisticRegression(C=1e5)
clf.fit(df_data,target)
train_predictions = clf.predict(df_data)

from sklearn.metrics import confusion_matrix
y_actu = target
y_pred = train_predictions
confusion_matrix(y_pred,y_actu)

##Logistic model classifies all records as 0 due to the unbalanced nature of the dataset.Hence
##create a balanced data set using imblearn

from imblearn import under_sampling, over_sampling
from imblearn.over_sampling import SMOTE
from imblearn.combine import SMOTEENN
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report

##Create balanced data set using SMOTE 
sm = SMOTEENN()
X_resampled, Y_resampled = sm.fit_sample(df_data, target)

##Fit a logistic regression model
clf.fit(X_resampled,Y_resampled)
##Make predictions
train_predictions = clf.predict(X_resampled)

##Create a confusion matrix
y_actu = Y_resampled
y_pred = train_predictions
confusion_matrix(y_pred,y_actu)

##Calculating Accuracy Scores
accuracy_score(y_actu, y_pred)

##Calculating the metrics from confusion matrix
tn, fp, fn, tp = confusion_matrix(y_actu, y_pred).ravel()

##Printing the classification report
print(classification_report(y_actu, y_pred))


##Create a balanced dataset usind ADASYN
from imblearn.over_sampling import ADASYN 
ada = ADASYN(random_state=42)
X_res, y_res = ada.fit_sample(df_data, target)

X_res=pd.DataFrame(X_res)
X_res.columns=df_data.columns.tolist()

from sklearn import linear_model
clf=linear_model.LogisticRegression(C=1e5)
clf.fit(X_res,y_res)
train_predictions = clf.predict(X_res)
clf.score(X_res,y_res)


##Create a confusion matrix
from sklearn.metrics import confusion_matrix
y_actu = y_res
y_pred = train_predictions
conf_mat=confusion_matrix(y_pred,y_actu)

##Extract scores for each record
score_probs=clf.predict_proba(X_res)[:,:]

##Calculating Accuracy Scores
accuracy_score(y_actu, y_pred)

##Calculating the metrics from confusion matrix
tn, fp, fn, tp = confusion_matrix(y_actu, y_pred).ravel()

##Printing the classification report
print(classification_report(y_actu, y_pred))

##Random Forest
from sklearn.ensemble import RandomForestClassifier
from sklearn.cross_validation import cross_val_score
import seaborn as sns

## initialize the paramters
rf = RandomForestClassifier(n_estimators=100) # initialize the paramters

##Fit the random forest model
rf.fit(X_res, y_res)
train_predictions = rf.predict(X_res)
y_actu = y_res
y_pred = train_predictions
conf_mat=confusion_matrix(y_pred,y_actu)

##Perform Cross Validation
val_train=cross_val_score(rf,X_res,y_res,cv=10)

##Initialize a list to store the results of cross validation
scores=[]
scores.append(val_train)

##Cross Validation using the f1 scoring method

val_train_f1=cross_val_score(rf,X_res,y_res,cv=10,scoring="f1")
scores_f1=[]
scores_f1.append(val_train_f1)
score_probs=clf.predict_proba(X_res)[:,:]


##On original imbalamced data set 
rf1 = RandomForestClassifier(n_estimators=100) # initialize
rf1.fit(df_data, target)
train_predictions = rf1.predict(df_data)
y_actu = target
y_pred = train_predictions
conf_mat=confusion_matrix(y_pred,y_actu)
accuracy_score(y_actu, y_pred)

y_newpred=rf1.predict_proba(X_res)[:,0]>0.1
confusion_matrix(y_newpred,y_actu)
##Just for CV
from sklearn.metrics import f1_score

def cutoff_predict(clf,X,cut_off):
    return(clf.predict_proba(X)[:,0]>cut_off).astype(int)

def custom_f1(cutoff):
    def f1_cutoff(clf,X,y):
        ypred = cutoff_predict(clf,X,cutoff)
        return( f1_score(y,ypred))
    return f1_cutoff

for cut_off in np.arange(0.1,0.9,0.1):
    clf=RandomForestClassifier(n_estimators=100)
    validated=cross_val_score(clf,X_res,y_res,cv=10,scoring=custom_f1(cut_off))
    
sns.boxplot(scores,names=np.arange(0.1,0.9,0.1))
##Calculating Accuracy Scores
accuracy_score(y_actu, y_pred)

##Printing the classification report
print(classification_report(y_actu, y_pred))

##SVM
from sklearn import svm
model = svm.SVC(kernel='linear', C=1, gamma=1) 
model.fit(X_res, y_res)
model.score(X_res, y_res)

##kernel=rbf
from sklearn import svm
model = svm.SVC(kernel='rbf', C=100, gamma=1) 
model.fit(X_res, y_res)
model.score(X_res, y_res)

coef3 = pd.Series(rf1.feature_importances_, X_res.columns.values.tolist()).sort_values(ascending=False)
coef3.plot(kind='bar', title='Feature Importances')


##Fine tuning the dataset
from stop_words import get_stop_words

stop_words = get_stop_words('en')
len(stop_words)

stop_words.append("NSE")

test=pd.DataFrame()
test['a'] =["www","http","Anil","2014"]
if test['a'][1] in stop_words:
    print("Present")

test['a'][1].isdigit()
j=0

temp2=[]
temp2[0][0]="www"

temp[0][0]="2016"
if ~(temp[j][0].isdigit() or temp[j][0] in stop_words):
    print("Working")
else:
    print("Not Working")
    
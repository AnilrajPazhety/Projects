# -*- coding: utf-8 -*-
"""
Created on Tue May 02 20:16:10 2017

@author: AnilNeha
"""

##Importing the required packages
import numpy as np
import pandas as pd
from datetime import datetime
import seaborn as sns
import matplotlib.pyplot as plt

##Set options to display max columns
pd.set_option("max_columns", 100)

##read the listings data file
data = pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/listings.csv")

##Analyze the first records
data.head()
data.info()

##Look at the missing values
data.apply(lambda x: sum(x.isnull()))

##Convert price variable from string to a float
prices = data['price'].map(lambda x: int(x[1:-4].replace(",", "")))
data['price'] = prices

##Plot neighbourhood_cleansed vs price
plt.figure(figsize = (12, 6))
sns.boxplot(x = 'neighbourhood_cleansed', y = 'price',  data = data)
xt = plt.xticks(rotation=90)

##Create a sort order for price < 600 to remove outliers
sort_order = data.query('price <= 600')\
                    .groupby('room_type')['price']\
                   .median()\
                   .sort_values(ascending=False)\
                   .index
##Plot room type vs price              
sns.boxplot(y='price', x='room_type', data=data.query('price <= 600'), order=sort_order)

#Create a new variable to define the number of days since the host listed for the first time    
today = datetime.now()
data['presence'] = data['host_since'].apply(lambda x :today-datetime.strptime(x,'%m/%d/%Y'))
 #Converting time delta to integer days
data['presence'] =data['presence'].dt.days

##Looking at host_response_time
data['host_response_time'].value_counts()
#Number of missing values

##Imputing the missing value as Not Available-->creating a new category
miss_bool=data['host_response_time'].isnull()
data.ix[miss_bool,'host_response_time']='Not Available'
data['host_response_time'].isnull().sum()

####Looking at host response rate with imputation
miss_bool = data['host_response_rate'].isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'host_response_rate'] = '0%'
data['host_response_rate']=data['host_response_rate'].map(lambda x: x[0:-1])

####Looking at host acceptance rate with imputation
miss_bool = data['host_acceptance_rate'].isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'host_acceptance_rate'] = '0%'
data['host_acceptance_rate']=data['host_acceptance_rate'].map(lambda x: x[0:-1])


##Verify variables for any missing values

## Host is superhost
data['host_is_superhost'].value_counts()

##host_listings_count
data['host_listings_count'].describe()
data['host_listings_count'].isnull().sum()

##host_total_listings_count
data['host_total_listings_count'].describe()
data['host_total_listings_count'].isnull().sum()

##Street 
street=data['street'].apply(lambda x: list(x.split(',')))
data['streetname']=street.apply(lambda x: x[0])
##Certain streets have high number of listings
data['streetname'].value_counts()


##host_has_profile_pic
data['host_is_superhost'].value_counts()


##host_identity_verified
data['host_identity_verified'].value_counts()


##is_location_exact
data['is_location_exact'].value_counts()

##Looking at the neighbourhood-31 unique neighbourhood
miss_bool=data['neighbourhood'].isnull()

data.ix[miss_bool,'neighbourhood']='Not Available'
data['neighbourhood'].isnull().sum()
len(data['neighbourhood'].unique())

##41 unique zip codes-Needs some work on couple of values
data['zipcode'].value_counts()
data['zipcode'].isnull().sum()
##Find out the location of the zipcodes which are not in the required format
data.zipcode[data.zipcode=='02108 02111']
##Location is 2047
data.ix[2047,43]='2108'

##Performing above operation for the second zipcode with inappropriate value
data.zipcode[data.zipcode=='02134-1704']
data.ix[3391,43]='2134'

##Handling Missing Values in zipcode by assigning it to '0000'
data.ix[data.zipcode.isnull(),43]='0000'

##markets Boston has 3568 values zero variance-drop this variable
data['market'].value_counts()

##property type
data['property_type'].value_counts()
data['property_type'].isnull().sum()
data['property_type'].describe()

##Impute the missing value with "Apartment' as it occurs max number of time
data.ix[data.property_type.isnull(),51]='Apartment'

##room type
data['room_type'].value_counts()
data['room_type'].isnull().sum()
sns.boxplot(data['room_type'],prices)


##accomodates
data['accommodates']
data['accommodates'].isnull().sum()

##bathrooms
data['bathrooms'].value_counts()
data['bathrooms'].isnull().sum()
miss_bt= data.bathrooms.isnull()
data.ix[miss_bt,54]=0

##bedrooms
data['bedrooms'].value_counts()
data['bedrooms'].isnull().sum()
miss_bd=data['bedrooms'].isnull()
data.ix[miss_bd,55]=0

##beds
data['beds'].value_counts()
data['beds'].isnull().sum()
miss_bd=data['beds'].isnull()
data.ix[miss_bd,56]=0

##bed type
data['bed_type'].value_counts()
data['bed_type'].isnull().sum()

##square feet
data['square_feet'].value_counts()

data['square_feet'].isnull().sum()

##security deposit-2243 missing values.missing can be zero
data['security_deposit'].isnull().sum()

##cleaning fee and convert it to a float value

miss_bool = data['cleaning_fee'].isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'cleaning_fee'] = '$0'
cleaning_fee = data['cleaning_fee'].map(lambda x:(x[1:-1]))
sum(data['cleaning_fee'].isnull())
data['cleaning_fee'].describe()

##Guests
data['guests_included'].isnull().sum()

##Price per extra
data['extra_people'].isnull().sum()
data['extra_people'].describe()
data['extra_people']=data['extra_people'].apply(lambda x: int(x[1:-4]))
data['extra_people'].describe()

##minimum_nights
data['minimum_nights'].describe()
data['minimum_nights'].isnull().sum()

##maximum_nights
data['maximum_nights'].describe()
data['maximum_nights'].isnull().sum()


##calendar_updated
data['calendar_updated'].describe()
data['calendar_updated'].unique()
data['calendar_updated'].value_counts()
data['calendar_updated'].isnull().sum()


##availability_30
data['availability_30'].describe()
data['availability_30'].isnull().sum()

##availability_60
data['availability_60'].describe()
data['availability_60'].isnull().sum()

##availability_90
data['availability_90'].describe()
data['availability_90'].isnull().sum()

##number_of_reviews
data['number_of_reviews'].describe()
data['number_of_reviews'].isnull().sum()

##first review and last review. Calculate days between reviews and assign it to a new variable daysbetrev
miss_bool = data['first_review'].isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'first_review'] = '9/6/2016'
data['first_review'] = data['first_review'].apply(lambda x :datetime.strptime(str(x),'%m/%d/%Y'))

miss_bool = data['last_review'].isnull()
print 'Orignal #missing: %d'% sum(miss_bool)
data.loc[miss_bool,'last_review'] = '9/6/2016'

data['last_review'] = data['last_review'].apply(lambda x :datetime.strptime(str(x),'%m/%d/%Y'))
 #Converting time delta to integer days
data['daysbetrevs']=data['last_review']-data['first_review']
data['daysbetrevs'].describe()
data['daysbetrevs'] =data['daysbetrevs'].dt.days

##review scores-Needs imputation
data['review_scores_rating'].describe()
data['review_scores_rating'].isnull().sum()
miss_rs=data['review_scores_rating'].isnull()
data.ix[miss_rs,'review_scores_rating']=np.nanmedian(data['review_scores_rating'])

##review_scores_accuracy-Needs imputation
data['review_scores_accuracy'].describe()
sum(data['review_scores_accuracy'].isnull())
miss_rs=data['review_scores_accuracy'].isnull()
data.ix[miss_rs,'review_scores_accuracy']=np.nanmedian(data['review_scores_accuracy'])

##review_scores_cleanliness-Needs imputation
data['review_scores_cleanliness'].describe()
sum(data['review_scores_cleanliness'].isnull())
miss_rs=data['review_scores_cleanliness'].isnull()
data.ix[miss_rs,'review_scores_cleanliness']=np.nanmedian(data['review_scores_cleanliness'])


##review_scores_checkin_Needs Imputation
data['review_scores_checkin'].describe()

sum(data['review_scores_checkin'].isnull())
miss_rs=data['review_scores_checkin'].isnull()
data.ix[miss_rs,'review_scores_checkin']=np.nanmedian(data['review_scores_checkin'])

##review_scores_communication Imputation
data['review_scores_communication'].describe()
sum(data['review_scores_communication'].isnull())
miss_rs=data['review_scores_communication'].isnull()
data.ix[miss_rs,'review_scores_communication']=np.nanmedian(data['review_scores_communication'])

##review_scores_location Imputation
data['review_scores_location'].describe()
sum(data['review_scores_location'].isnull())
miss_rs=data['review_scores_location'].isnull()
data.ix[miss_rs,'review_scores_location']=np.nanmedian(data['review_scores_location'])


##review_scores_value Imputation
data['review_scores_value'].describe()
sum(data['review_scores_value'].isnull())
miss_rs=data['review_scores_value'].isnull()
data.ix[miss_rs,'review_scores_value']=np.nanmedian(data['review_scores_value'])

##require license
sum(data['requires_license'].isnull())

##instant_bookable

sum(data['instant_bookable'].isnull())


##cancellation_policy
sum(data['cancellation_policy'].isnull())

##require_guest_profile_picture
sum(data['require_guest_profile_picture'].isnull())
data['require_guest_profile_picture'].value_counts()


##require_guest_phone_verification
sum(data['require_guest_phone_verification'].isnull())
data['require_guest_phone_verification'].value_counts()


##calculated_host_listings_count
sum(data['calculated_host_listings_count'].isnull())
data['calculated_host_listings_count'].value_counts()

##reviews_per_month-Imputation required
sum(data['reviews_per_month'].isnull())
data['reviews_per_month'].value_counts()
miss_rs=data['reviews_per_month'].isnull()
data.ix[miss_rs,'reviews_per_month']=np.nanmedian(data['review_scores_value'])


##Train and Test Data

msk = np.random.rand(len(data)) < 0.8
train = data[msk]
test = data[~msk]

##dropping  variables
##train
train.drop(['id','listing_url','scrape_id','last_scraped','name','summary','space','description','experiences_offered','neighborhood_overview'],axis=1,inplace=True)
train.drop(['notes','transit','access','interaction','house_rules','thumbnail_url','medium_url','picture_url','xl_picture_url','host_id'],axis=1,inplace=True)
train.drop(['host_url','host_name','host_location','host_about','host_thumbnail_url','host_picture_url','market','country_code','country','latitude','longitude','amenities'],axis=1,inplace=True)
train.drop(['square_feet','weekly_price','monthly_price','calendar_last_scraped','first_review','last_review','license','jurisdiction_names'],axis=1,inplace=True)

##test
test.drop(['id','listing_url','scrape_id','last_scraped','name','summary','space','description','experiences_offered','neighborhood_overview'],axis=1,inplace=True)
test.drop(['notes','transit','access','interaction','house_rules','thumbnail_url','medium_url','picture_url','xl_picture_url','host_id'],axis=1,inplace=True)
test.drop(['host_url','host_name','host_location','host_about','host_thumbnail_url','host_picture_url','market','country_code','country','latitude','longitude','amenities'],axis=1,inplace=True)
test.drop(['square_feet','weekly_price','monthly_price','calendar_last_scraped','first_review','last_review','license','jurisdiction_names'],axis=1,inplace=True)

##Save transformed variables into a new file
train.to_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/trainmodrev.csv",index=False)
test.to_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/testmodrev.csv",index=False)



train.drop(['host_since','host_neighbourhood','neighbourhood_cleansed','neighbourhood_group_cleansed','city','state','has_availability'],axis=1,inplace=True)
test.drop(['host_since','host_neighbourhood','neighbourhood_cleansed','neighbourhood_group_cleansed','city','state','has_availability'],axis=1,inplace=True)
#train.drop(['neighbourhood_cleansed','neighbourhood_group_cleansed','city','state','has_availability'],axis=1,inplace=True)

##Convert categorical variables to numerical through label encoding
from sklearn.preprocessing import LabelEncoder
le= LabelEncoder()

var_mod = ['host_response_time','host_is_superhost','host_has_profile_pic','host_identity_verified','street','neighbourhood','smart_location','is_location_exact','property_type','room_type','bed_type','calendar_updated','requires_license','instant_bookable','cancellation_policy','require_guest_profile_picture','require_guest_phone_verification','streetname']
le = LabelEncoder()
for i in var_mod:
    train[i] = le.fit_transform(train[i])
    
    
for i in var_mod:
    test[i] = le.fit_transform(test[i])
    
train.to_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/trainmodrev2.csv",index=False)
test.to_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/testmodrev2.csv",index=False)

train= pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/trainmodrev2.csv")
train.drop(['price'],axis=1,inplace=True)
test.drop(['price'],axis=1,inplace=True)

test= pd.read_csv("C:/Users/AnilNeha/Desktop/MSBAPM/Spring/DMBI/Project/dmprojectdatapreprocessing/testmodrev2.csv")
test['host_response_time']=le.fit_transform(test['host_response_time'])
test['host_has_profile_pic']=le.fit_transform(test['host_has_profile_pic'])

train.drop(['host_verifications'],axis=1,inplace=True)

##Approach 1- Treating it as a classification problem
##Create bins for target price

bins = [0,100,200,300,400,500,600]
group_names = ['C1', 'C2','C3','C4','C5','C6']

categories = pd.cut(train['price'], bins, labels=group_names)
train['price1'] = pd.cut(train['price'], bins, labels=group_names)

##Fit a logistic regression model on the train set
from sklearn import linear_model
clf=linear_model.LogisticRegression(C=1e5)
clf.fit(train[predictors],train['price1'])
train_predictions = clf.predict(train[predictors])

from sklearn.metrics import confusion_matrix
y_actu = train['price1']
y_pred = train_predictions
confusion_matrix(y_actu, y_pred)

##Fit a SVM model
from sklearn import svm
X=train[predictors]
y= train['price1'] 
sets=svm.SVC(C=1.0, kernel='rbf', degree=3, gamma=0.0, coef0=0.0, shrinking=True, probability=False,tol=0.001, cache_size=200, class_weight=None, verbose=False, max_iter=-1, random_state=None)
svc = svm.SVC(kernel='linear', C=1,gamma=0).fit(X, y)
svc2 = svm.SVC(kernel='rbf', C=1,gamma=0).fit(X, y)
svc3 = svm.SVC(kernel='rbf', C=1,gamma=10).fit(X, y)
svc2.fit(X,y)
svc2.score(X,y)

##Run the model on test set

categories = pd.cut(test['price'], bins, labels=group_names)
test['price'] = pd.cut(test['price'], bins, labels=group_names)
test_predictions = clf.predict(test[predictors])
y_actu = test['price']
y_pred = test_predictions
confusion_matrix(y_actu, y_pred)


##Approach 2-Treating price as a continous variable
##Filter records with price less 600 to remove outliers
train1=train
train=train1[train.price<=600]
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn import cross_validation, metrics

##Create variable to store the features to be used in the model
predictors = ['room_type','bathrooms','zipcode','reviews_per_month','daysbetrevs','availability_60','availability_365','bedrooms','beds','availability_90','neighbourhood','accommodates','host_acceptance_rate','Amenities_AC','number_of_reviews','extra_people','instant_bookable','availability_30','host_is_superhost','street','Email','facebook','Phone','reviews','security_deposit','cleaning_fee']


##Decison Tree
from sklearn.tree import DecisionTreeRegressor
alg1 = DecisionTreeRegressor(max_depth=10, min_samples_leaf=50)
alg1 = DecisionTreeRegressor(max_depth=15, min_samples_leaf=35)
alg1.fit(train[predictors],train['price'])
train_predictions = alg1.predict(train[predictors])

##Random Forest
from sklearn.ensemble import RandomForestRegressor
alg1 = RandomForestRegressor(n_estimators=200,max_depth=5, min_samples_leaf=100,n_jobs=4)
alg1.fit(train[predictors],train['price'])
train_predictions = alg1.predict(train[predictors])

##Linear Regression Model
alg1 = LinearRegression(normalize=True)
alg1.fit(train[predictors],y_train)
train_predictions = alg1.predict(train[predictors])

##Variable Importance
coef3 = pd.Series(alg1.feature_importances_, train.columns.values.tolist()).sort_values(ascending=False)
coef3.plot(kind='bar', title='Feature Importances')

#Perform Cross Validation
cv_score= cross_validation.cross_val_score(alg1,train[predictors],train['price'],cv=20,scoring='mean_squared_error')
cv_score=np.sqrt(np.abs(cv_score))
    
##Print Report
print "\nModel Report"
print "RMSE : %.4g" % np.sqrt(metrics.mean_squared_error(train['price'].values, train_predictions))
print "CV Score : Mean - %.4g | Std - %.4g | Min - %.4g | Max - %.4g" % (np.mean(cv_score),np.std(cv_score),np.min(cv_score),np.max(cv_score))

##Print Rsquare
import sklearn.metrics
r_squared = sklearn.metrics.r2_score(y_train, train_predictions)
r_squared


##Boosting
from sklearn.ensemble import GradientBoostingRegressor
from sklearn import ensemble
from sklearn.metrics import mean_squared_error
from sklearn.metrics import r2_score
params = {'n_estimators': 1000, 'max_depth': 2, 'min_samples_split': 10,
          'min_samples_leaf':30, 'learning_rate': 0.01}
clf = ensemble.GradientBoostingRegressor(**params)
X_train=train[predictors]
clf.fit(X_train, train['price'])
mse = mean_squared_error(train['price'], clf.predict(X_train))
r2=r2_score(y_train, clf.predict(X_train))
print("MSE: %.4f" % mse)



##Fit the algorithm on test set
X_test=test[predictors]
mse = mean_squared_error(y_test, clf.predict(X_test))
r2=r2_score(y_test, clf.predict(X_test))
print("MSE: %.4f" % mse)


##Test Predictions
#test['price'] = alg1.predict(test[predictors])
test_pred=alg1.predict(test[predictors])
r_squared = sklearn.metrics.r2_score(test['price'], test_pred)
print "RMSE : %.4g" % np.sqrt(metrics.mean_squared_error(test['price'].values, test_pred))

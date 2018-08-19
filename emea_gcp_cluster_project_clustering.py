# -*- coding: utf-8 -*-
"""EMEA_GCP Cluster Project - Clustering

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1TpwcLuE5Vq_vyfwsW6CaT86saW32ptuo

# **Import Statements**

You must run the code blocks to generate the key to authorize the GDrive download
"""

#Pip installs
# %%capture 
!pip install -U ggplot
!pip install -U -q PyDrive

#Import required libraries
from __future__ import print_function

import io
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
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import Imputer
from sklearn import mixture
from sklearn.mixture import GMM
from ggplot import *
from sklearn.manifold import TSNE
import seaborn as sns
import statsmodels.api as sm
import datetime
import collections
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix

from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
from oauth2client.client import GoogleCredentials
from googleapiclient.http import MediaIoBaseDownload

from google.colab import auth
auth.authenticate_user()

from googleapiclient.discovery import build
drive_service = build('drive', 'v3')

# %matplotlib inline

"""# Helper Functions

### Downloader from GDrive
"""

# functions to download the data frames created from the data preparation stage
def downloader(file_id):
    request = drive_service.files().get_media(fileId=file_id)
    downloaded = io.BytesIO()
    downloader = MediaIoBaseDownload(downloaded, request)
    done = False
    while done is False:
        _, done = downloader.next_chunk()
    downloaded.seek(0)
    return downloaded

def delete_existing_file(file_name,file_list):
    for file1 in file_list:
        if file1['title'] == file_name:
            file1.Delete() 
            
def upload_to_drive(file_name,file_list,folder_id):
    uploaded = drive.CreateFile({'title': file_name, "parents": [{"kind": "drive#fileLink", "id": folder_id}]})
    uploaded.SetContentFile(file_name)
    uploaded.Upload()
    print('Uploaded %s with ID {}'.format(uploaded.get('id')) % file_name)

"""# Load DataFrames From File"""

#Load a file by ID and load pickle into dataframes

df_nas_id = '1gj7Zdd4_-_RXtPqNhAf_as17vY69zkMx'
df_pca_2_id = '1IRGRjGGPbwh5rMio0yQqeXUr1RVVyCVS'
df_pca_3_id = '1aVGOpNWhzj4xxStKP-qKmNVMub4rKPpR'

saved_cols_id = '1CX68W3ewGPzlpgJ_59GtV8wYa77e6dCj'
df_org_id = '1VwzDbgh4ePRzIOhBp_EwJIT8iReAca2a'
df_copy_nas_id='1zYep9Co2__vL-zTXF66GVZarit3jO3kn'

#Load each dataframe

df_nas = pd.read_pickle(downloader(df_nas_id))
df_pca_2 = pd.read_pickle(downloader(df_pca_2_id))
df_pca_3 = pd.read_pickle(downloader(df_pca_3_id))
#df_pca_4 = pd.read_pickle(downloader(df_pca_4_id))
saved_cols = pd.read_pickle(downloader(saved_cols_id))
df_org = pd.read_pickle(downloader(df_org_id))

df_copy_nas = pd.read_pickle(downloader(df_copy_nas_id))

print("Files downloaded and dataframes ready for use.")

"""# Clustering: Fit various algorithms (GMM, KMeans, DBScan)

## GMM Clustering
"""

#Predict the labels and save them as GMM_labels_frame

gmm = mixture.GaussianMixture(n_components=7).fit(df_nas)
gmm_labels = gmm.predict(df_nas)
labels_series = pd.Series(list(gmm_labels))
GMM_labels_frame = labels_series.to_frame()
GMM_labels_frame.columns = ['GMM_Cluster_Membership']


GMM_labels_frame['GMM_Cluster_Membership'].value_counts()

#Perform cross validation on different cv_types for n_components (1,7) to find out optimal values for n_components and co-variance type

lowest_bic = np.infty
bic = []
n_components_range = range(1, 7)
cv_types = ['spherical', 'tied', 'diag', 'full']
for cv_type in cv_types:
    for n_components in n_components_range:
        # Fit a Gaussian mixture with EM
        gmm = mixture.GaussianMixture(n_components=n_components,
                                      covariance_type=cv_type)
        gmm.fit(df_nas)
        bic.append(gmm.bic(df_nas))
        if bic[-1] < lowest_bic:
            lowest_bic = bic[-1]
            best_gmm = gmm

#Plot the results for cross validation

import itertools
bic = np.array(bic)
color_iter = itertools.cycle(['navy', 'turquoise', 'cornflowerblue','darkorange'])
clf = best_gmm
bars = []

# Plot the BIC scores
spl = plt.subplot(1, 1, 1)
for i, (cv_type, color) in enumerate(zip(cv_types, color_iter)):
    xpos = np.array(n_components_range) + .2 * (i - 2)
    bars.append(plt.bar(xpos, bic[i * len(n_components_range):
                                  (i + 1) * len(n_components_range)],
                        width=.2, color=color))
plt.xticks(n_components_range)
plt.ylim([bic.min() * 1.01 - .01 * bic.max(), bic.max()])
plt.title('BIC score per model')
xpos = np.mod(bic.argmin(), len(n_components_range)) + .65 +\
        .2 * np.floor(bic.argmin() / len(n_components_range))
plt.text(xpos, bic.min() * 0.97 + .03 * bic.max(), '*', fontsize=14)
spl.set_xlabel('Number of components')

spl.legend([b[0] for b in bars], cv_types,bbox_to_anchor=(1.05, 1))

##From this plot it can be obersved that cv type = FUll and n_components=6 offers the lowest BIC score
##But when on n_components as 6, one cluster only had 34 data points hence it makes sense to use 5 clusters with CV type =full

#Trying the CV Type Full on the GMM Model
np.random.seed(8888)
gmf = mixture.GaussianMixture(n_components=7, covariance_type='full').fit(df_nas)
##Predict the labels and save them as GMM_labels_frame
gmf_labels = gmf.predict(df_nas)
labels_series = pd.Series(list(gmf_labels))
GMF_labels_frame = labels_series.to_frame()
GMF_labels_frame.columns = ['GMF_Cluster_Membership']

GMF_labels_frame['GMF_Cluster_Membership'].value_counts()

#Trying the CV Type Diag on the GMM Model
np.random.seed(8888)
gmd = mixture.GaussianMixture(n_components=8, covariance_type='diag').fit(df_nas)
##Predict the labels and save them as GMM_labels_frame
gmd_labels = gmf.predict(df_nas)
labels_series = pd.Series(list(gmd_labels))
GMD_labels_frame = labels_series.to_frame()
GMD_labels_frame.columns = ['GMD_Cluster_Membership']

GMD_labels_frame['GMD_Cluster_Membership'].value_counts()

#Trying the CV Type Spherical on the GMM Model
np.random.seed(8888)
gms = mixture.GaussianMixture(n_components=8, covariance_type='spherical').fit(df_nas)
##Predict the labels and save them as GMM_labels_frame
gms_labels = gms.predict(df_nas)
labels_series = pd.Series(list(gms_labels))
GMS_labels_frame = labels_series.to_frame()
GMS_labels_frame.columns = ['GMS_Cluster_Membership']

GMS_labels_frame['GMS_Cluster_Membership'].value_counts()

"""## KMeans Clustering"""

#Run KMeans with 7 clusters and return the membership
kmeans_7_clusters = KMeans(7)
kmeans_7_clusters.fit(df_nas)

kmeans_7_labels = kmeans_7_clusters.labels_

KM7_labels_series = pd.Series(list(kmeans_7_labels))
KM7_labels_frame = KM7_labels_series.to_frame()
KM7_labels_frame.columns = ['KM7_Cluster_Membership']

KM7_labels_frame['KM7_Cluster_Membership'].value_counts()

#Run KMeans with 8 clusters and return the membership
kmeans_8_clusters = KMeans(8)
kmeans_8_clusters.fit(df_nas)

kmeans_8_labels = kmeans_8_clusters.labels_

KM8_labels_series = pd.Series(list(kmeans_8_labels))
KM8_labels_frame = KM8_labels_series.to_frame()
KM8_labels_frame.columns = ['KM8_Cluster_Membership']

KM8_labels_frame['KM8_Cluster_Membership'].value_counts()

"""## DBSCAN Clustering"""

#Run DBScan clustering to see if it produces a better fit that K-Means
from sklearn.cluster import DBSCAN
dbscan = DBSCAN(eps=1, min_samples=50)

#Running with a random sample of 5,000 just to see the output of the algo
dbscan.fit(df_nas.sample(5000))

dbs_labels = dbscan.labels_

DBS_labels_series = pd.Series(list(dbs_labels))
DBS_labels_frame = DBS_labels_series.to_frame()
DBS_labels_frame.columns = ['DBS_Cluster_Membership']

"""# Appending Cluster Membership Labels back to the data frames"""

#Append the cluster membership from all algorithms back to saved_cols which 
#will allow us to link the unique code with the original data

#Not sure if we need to reset the index here. --> We need to reset it here as in the next step we merging two dataframes.saved_cols index doesnot follow sequential order
#whereas GMM_labels_frame,GMD_labels_frame,GMF_labels_frame are sequential which leads to a lot of NAN values for membership

saved_cols=saved_cols.reset_index(drop=True)



saved_cols_2 = pd.concat([saved_cols, GMM_labels_frame,  GMD_labels_frame, GMF_labels_frame,GMS_labels_frame], axis=1, join_axes=[saved_cols.index])

#For creating a 2d and 3d plot
cluster_plot_df = pd.concat([df_pca_2, GMM_labels_frame, GMD_labels_frame, GMF_labels_frame,GMS_labels_frame], axis=1, join_axes=[df_pca_2.index])
cluster_plot_3d = pd.concat([df_pca_3, GMM_labels_frame, GMD_labels_frame, GMF_labels_frame,GMS_labels_frame], axis=1, join_axes=[df_pca_3.index])
#cluster_plot_4d = pd.concat([df_pca_4, GMM_labels_frame, GMD_labels_frame, GMF_labels_frame,GMS_labels_frame], axis=1, join_axes=[df_pca_4.index])

'''

saved_cols_2 = pd.concat([saved_cols, GMM_labels_frame, KM4_labels_frame, KM5_labels_frame, DBS_labels_frame, GMD_labels_frame, GMF_labels_frame], axis=1, join_axes=[saved_cols.index])

#For creating a 2d and 3d plot
cluster_plot_df = pd.concat([df_pca_2, GMM_labels_frame, KM4_labels_frame, KM5_labels_frame, DBS_labels_frame, GMD_labels_frame, GMF_labels_frame], axis=1, join_axes=[df_pca_2.index])
cluster_plot_3d = pd.concat([df_pca_3, GMM_labels_frame, KM4_labels_frame, KM5_labels_frame, DBS_labels_frame, GMD_labels_frame, GMF_labels_frame], axis=1, join_axes=[df_pca_3.index])
'''
saved_cols_2.shape

"""# Save Cluster Membership Data as CSV to Drive"""

#Append cluster membership back to the df_nas and saved_cols dataframe and then upload the results as a CSV back to the GDrive

#Authenticate and create the PyDrive client.
auth.authenticate_user()
gauth = GoogleAuth()
gauth.credentials = GoogleCredentials.get_application_default()
drive = GoogleDrive(gauth)

#Create CSV file on CoLaboratory VM directory
final_results_df = pd.concat([saved_cols_2,df_nas], axis=1, join_axes=[df_nas.index])
final_results_df.to_csv('final_cluster_results.csv', encoding='utf-8')



#Shared Data Science folder ID: 1pSJZgJ4LJp6VcRk68_mkPWxyMsUXoAF2
fid = '1pSJZgJ4LJp6VcRk68_mkPWxyMsUXoAF2'

file_list = drive.ListFile({'q': "'%s' in parents and trashed=false" % fid}).GetList()
            
uploaded = drive.CreateFile({'title': 'final_cluster_results.csv', "parents": [{"kind": "drive#fileLink", "id": fid}]})
#uses function created earlier in file
delete_existing_file('final_cluster_results.csv', file_list)  
uploaded.SetContentFile('final_cluster_results.csv')
uploaded.Upload()
print('Uploaded final_cluster_results.csv with ID {}'.format(uploaded.get('id')))

"""# Visualizing the Cluster Fit Across Algorithms

## GMM Visualization
"""

##Plot the counts of cluster membership
#cluster_plot_df.hist(['GMM_Cluster_Membership'])
cluster_cnt = cluster_plot_df[cluster_plot_df['GMS_Cluster_Membership'].isin([0,5,7,1,6])]

cluster_plot_df['GMS_Cluster_Membership'].value_counts()
arr=plt.hist(cluster_cnt['GMS_Cluster_Membership'])
for i in range(10):
    plt.text(arr[1][i],arr[0][i],str(arr[0][i]))

df_pca_2.head()

#Plot the clusters in 2d
plt.scatter(cluster_plot_df[1],cluster_plot_df[0], c=cluster_plot_df['GMS_Cluster_Membership'], cmap='Set1')
plt.xlim(-10, 40)
plt.ylim(-15, 200)

#Plot the clusters in 3d
fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(cluster_plot_3d[0],cluster_plot_3d[1],cluster_plot_3d[2], c=cluster_plot_3d['GMS_Cluster_Membership'], cmap='Set1')

ax.set_xlim3d(-60, 20)
ax.set_ylim3d(30,70)
ax.set_zlim3d(-30,10)

pyplot.show()



#violin plot which gives some insights about the GCP_NA_Scores on the clusters.Scores for Cluster 1 and 2 are more likely to be near 50. 
  #Cluster 0 score more likely to be more than 60 and cluster 3 between 50 and 80.Only cluster 4 shows more scores less than 40
  ggplot(saved_cols_2, aes('GMM_Cluster_Membership', 'GCP_NA_Score')) + geom_violin() + geom_jitter(alpha=0.1)

"""## **GMM Covariance Type Full**"""

##Plot the counts of cluster membership
#cluster_plot_df.hist(['GMF_Cluster_Membership'])

arr=plt.hist(cluster_plot_df['GMM_Cluster_Membership'])
for i in range(10):
    plt.text(arr[1][i],arr[0][i],str(arr[0][i]))

arr=plt.hist(cluster_plot_df['GMF_Cluster_Membership'])
for i in range(10):
    plt.text(arr[1][i],arr[0][i],str(arr[0][i]))

cluster_plot_df.head(1)

#Plot the clusters in 2d
plt.scatter(cluster_plot_df[1],cluster_plot_df[0], c=cluster_plot_df['GMF_Cluster_Membership'], cmap='Set1')
plt.xlim(-30, 20)
plt.ylim(-30, 200)

fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(cluster_plot_3d[0],cluster_plot_3d[1],cluster_plot_3d[2], c=cluster_plot_3d['GMF_Cluster_Membership'], cmap='Set1')

ax.set_xlim3d(-30, 20)
ax.set_ylim3d(-20,60)
ax.set_zlim3d(-20,0)

pyplot.show()

"""## GMM Covariance Type Diagonal"""

#Plot the counts of cluster membership
#cluster_plot_df.hist(['GMD_Cluster_Membership'])


arr=plt.hist(cluster_plot_df['GMD_Cluster_Membership'])
for i in range(10):
    plt.text(arr[1][i],arr[0][i],str(arr[0][i]))

#Plot the clusters in 2d
plt.scatter(cluster_plot_df[1],cluster_plot_df[0], c=cluster_plot_df['GMD_Cluster_Membership'], cmap='Set1')
plt.xlim(-30, 20)
plt.ylim(-30, 200)

fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(cluster_plot_3d[0],cluster_plot_3d[1],cluster_plot_3d[2], c=cluster_plot_3d['GMD_Cluster_Membership'], cmap='Set3')

ax.set_xlim3d(-30, 20)
ax.set_ylim3d(-20,60)
ax.set_zlim3d(-20,0)

pyplot.show()

#violin plot which gives some insights about the GCP_NA_Scores on the clusters.Scores for Cluster 1 and 2 are more likely to be near 50. 
  #Cluster 0 score more likely to be more than 60 and cluster 3 between 50 and 80.Only cluster 4 shows more scores less than 40
  from ggplot import *
  ggplot(saved_cols_2, aes('GMD_Cluster_Membership', 'GCP_NA_Score')) + geom_violin() + geom_jitter(alpha=0.1)

"""## KMeans 8"""

#Plot the counts of cluster membership
cluster_plot_df.hist(['KM8_Cluster_Membership'])

#Plot the clusters in 2d
plt.scatter(cluster_plot_df[1],cluster_plot_df[0], c=cluster_plot_df['KM8_Cluster_Membership'], cmap='Set1')
plt.xlim(-30, 20)
plt.ylim(-30, 200)

fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(cluster_plot_3d[0],cluster_plot_3d[1],cluster_plot_3d[2], c=cluster_plot_3d['KM8_Cluster_Membership'], cmap='Set1')

ax.set_xlim3d(-30, 20)
ax.set_ylim3d(20,60)
ax.set_zlim3d(-20,0)

pyplot.show()

"""### Checking for outliers"""

#Find the outlier rows and identify their index
df_outliers = cluster_plot_df.loc[cluster_plot_df['GMD_Cluster_Membership'] == 3]
df_outliers.head(10)

#Inspect the data a specific index in case we want to remove it from the dataset
saved_cols_2.iloc[[31201]]

"""## KMeans 4"""

#Plot the counts of cluster membership
cluster_plot_df.hist(['KM4_Cluster_Membership'])

#Plot the clusters in 2d
plt.scatter(cluster_plot_df[1],cluster_plot_df[0], c=cluster_plot_df['KM4_Cluster_Membership'], cmap='Set1')
plt.xlim(-30, 20)
plt.ylim(-30, 200)

#Plot the clusters in 3d
fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(cluster_plot_3d[0],cluster_plot_3d[1],cluster_plot_3d[2], c=cluster_plot_3d['KM4_Cluster_Membership'], cmap='Set1')

ax.set_xlim3d(-30, 20)
ax.set_ylim3d(-20,60)
ax.set_zlim3d(-20,0)

pyplot.show()

"""## DBScan Clustering"""

#Plot the counts of cluster membership
cluster_plot_df.hist(['DBS_Cluster_Membership'])

#Plot the clusters in 2d
plt.scatter(cluster_plot_df[1],cluster_plot_df[0], c=cluster_plot_df['DBS_Cluster_Membership'], cmap='Set1')
plt.xlim(-30, 20)
plt.ylim(-30, 200)

fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(cluster_plot_3d[0],cluster_plot_3d[1],cluster_plot_3d[2], c=cluster_plot_3d['DBS_Cluster_Membership'], cmap='Set1')
pyplot.show()

"""# Verify Cluster Accuracy"""

#Using RandomForest classifier
#Make dummies from final cluster algorithm chosen
#Fit classifier to each of the N dummy columns
#Plot eigenvalues descending to reveal feature importance of each cluster
#Plot comparison of each cluster against top features

df_X = df_nas #create a new df to work from for classification
df_y = pd.get_dummies(cluster_plot_df['GMS_Cluster_Membership'])

df_y.head()

df_y.sum()

#Fit the random foresrt classifier

clf = RandomForestClassifier(n_estimators=10)

#Initialize a dataframe to store the feature importances
cluster_importances = pd.DataFrame()

for i in range(df_y.shape[1]): #loop through each of the dummies
    clf_pr = clf.fit(df_X, df_y[i]).predict(df_X)
    print("Accuracy Report for Cluster %d:" % (i + 1))
    print(confusion_matrix(df_y[i], clf_pr))
    print(accuracy_score(df_y[i], clf_pr))
    importances = clf.feature_importances_
    import_series = pd.Series(importances.tolist())
    col_name = "Cluster_" + str(i+1)
    cluster_importances[col_name] = import_series

cluster_importances["Feature_Names"] = ""

#Loop through each of the columns
  
for j in range(df_X.shape[1]):
    feature_name = df_X.columns[j]
    cluster_importances.at[j, 'Feature_Names'] = feature_name

cluster_importances.head()

#Save cluster importances back to  the GDrive

#Authenticate and create the PyDrive client.
auth.authenticate_user()
gauth = GoogleAuth()
gauth.credentials = GoogleCredentials.get_application_default()
drive = GoogleDrive(gauth)

#Create CSV file on CoLaboratory VM directory
cluster_importances.to_pickle('cluster_importances_20_4.pkl')
cluster_plot_df.to_pickle('cluster_plot_df_20_4.pkl')
cluster_plot_3d.to_pickle('cluster_plot_3d_20_4.pkl')
saved_cols_2.to_pickle('saved_cols_2_20_4.pkl')
df_y.to_pickle('df_y_20_4.pkl')

#Shared EMEA folder in Data Science folder ID: 1pSJZgJ4LJp6VcRk68_mkPWxyMsUXoAF2
fid = '1pSJZgJ4LJp6VcRk68_mkPWxyMsUXoAF2'
#fid = '1NymTBD-2ZDvMx6MucO4ZPVJ0mCfHPmhr'
file_list = drive.ListFile({'q': "'%s' in parents and trashed=false" % fid}).GetList()

files_to_upload = ['cluster_importances_20_4.pkl','cluster_plot_df_20_4.pkl','cluster_plot_3d_20_4.pkl','saved_cols_2_20_4.pkl','df_y_20_4.pkl']

#Upload each file
for file in files_to_upload:
    delete_existing_file(file, file_list) 
    upload_to_drive(file,file_list,fid)
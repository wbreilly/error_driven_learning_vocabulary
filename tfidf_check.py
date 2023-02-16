#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  5 14:32:24 2022

@author: WBR

Identify duplicate subjects in EDL experiments who used multiple mturk accounts.
Use tfidf to identify participants with strong overlap in incorrect responses.
"""

import nltk
import pandas as pd
import re
import numpy as np
import itertools
import glob

nltk.download('stopwords')
from nltk.corpus import stopwords

from nltk.stem.porter import PorterStemmer
stemmer = PorterStemmer()

from nltk.tokenize import word_tokenize as wt 

#%% Read in participant database and load all part 2 file names

db_dir = '/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/'
participant_db = pd.read_csv(db_dir + "participant_db_8_5_22.csv")
 
home_dir = '/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/'

files = participant_db.sona_id.tolist()
files_found = list(itertools.chain.from_iterable([glob.glob(f'{home_dir}/**/*{f}.csv', recursive=True) for f in files]))

fdf = pd.DataFrame({'files':files_found})

fdf = fdf[fdf["files"].str.contains("old_results")==False]
fdf = fdf[fdf["files"].str.contains("part1")==False]

ffiles = fdf.files.tolist()

#%% Read csvs from part 2 file names
dfs=[]
for counter,file in enumerate(ffiles):
    # get some metadata
    id_df = pd.read_csv(file,header=0,nrows=1)
    link = id_df['link'][0]
    sona_id = id_df['id'][0]
    comp_code = id_df['completionCode']
    timestamp = id_df['GMT_timestamp'].tolist()[0]
    
    # import df and attach metadata
    df = pd.read_csv(file,header=2,dtype={'pair_idx': str})
    df[['sona_id','comp_code','link','timestamp']] = [sona_id,comp_code,link,timestamp]
    
    dfs.append(df)
metadf = pd.concat(dfs, ignore_index = True)

#%% prepare cued recall data

# remove subjects who are not in "good subs" of any experiment, because they have already been weeded out
csvs = ['/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp3_mturk/exp3_n=104_final.csv',
'/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4a_mturk/exp4a_n=44_final.csv',
'/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp4b_mturk/exp4b_n=50_final.csv',
'/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp5_mturk/exp51_n=56_final.csv']
   
out=[]
for csv in csvs:
    tmp = pd.read_csv(csv)
    subs = pd.DataFrame(tmp.sona_id.unique())
    out.append(subs)
good_subs =  pd.concat(out, ignore_index = True)[0].tolist()

# select response to cued-recall only
rdf = metadf[metadf['stim2'] == '???'].copy()
rdf = rdf[rdf['sona_id'].isin(good_subs)]
rdf = rdf[rdf['response'] != '???']
rdf['response'] = rdf['response'].str.replace('\;',' ')
rdf['response'] = rdf['response'].str.replace('timeout',' ')
rdf = rdf[~rdf.response.isnull()]

# score responses, only repeat incorrect responses will be informative
score_key = pd.read_csv('/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/exp2/' + 'exp2_scoring_key.csv',header=0)
score_dict = score_key.set_index(['swahili','english'])['correct'].to_dict()
rdf['correct'] = rdf.set_index(['stim1','response']).index.map(score_dict)
# mark zeros where there was no match aka INCORRECT
rdf['correct'] = rdf['correct'].fillna(0)
#  incorrect only
rdf = rdf[rdf['correct'] == 0]


# gather responses from same subject so that n rows equals n subjects
rdf = rdf.groupby('sona_id', as_index=False).agg({'sona_id' : 'first', 'response' : ' '.join})

#%% tokenize

data = []

for i in range(rdf.shape[0]):
    sms = rdf.iloc[i, 1]

    # remove non alphabatic characters
    sms = re.sub('[^A-Za-z]', ' ', sms)

    # make words lowercase, because Go and go will be considered as two words
    sms = sms.lower()

    # tokenising
    tokenized_sms = wt(sms)

    # remove stop words and stemming
 
    sms_processed = []
    for word in tokenized_sms:
        if word not in set(stopwords.words('english')):
            sms_processed.append(stemmer.stem(word))

    sms_text = " ".join(sms_processed)
    data.append(sms_text)


#%% create the feature matrix 

from sklearn.feature_extraction.text import CountVectorizer
matrix = CountVectorizer(max_features=1000)
X = matrix.fit_transform(data).toarray()
y = rdf.iloc[:, 0] # ids

#%% 

# cross correlate feature vectors
df = pd.DataFrame(X.transpose())
cdf = df.corr()
cdf = cdf.to_numpy()
# nans on diag
cdf[np.diag_indices_from(cdf)] /= np.nan

# mask for correlations about .8 and plot
from numpy import ma 
mask = ma.masked_inside(cdf,-1,.80)
import matplotlib.pyplot as plt
plt.imshow(mask)

# extract indices associated with extreme values/participants
idx = pd.DataFrame(np.transpose(mask.nonzero()))
idx = idx.sort_values(0)
idx = idx[idx[0]!= idx[1]]

# count id appearances greater than .8
idx['count'] = idx.groupby(0).transform("count")
idx.columns= ['idx','idx2','count']

# convert from indices to mturk ids
rdf['idx'] = np.arange(0,252)
check = rdf.merge(idx,on='idx')
check = check.merge(rdf[['sona_id','idx']],left_on='idx2',right_on='idx')
check = check.drop_duplicates(subset='sona_id_x')

# final step was to manually interrogate files from subjects with high counts
# of extreme correlations with other ids

#%%


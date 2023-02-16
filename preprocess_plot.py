#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 11 16:28:14 2021

@author: WBR

This script was used to preprocess and plot dat from all EDL experiments that included a one day retention interval.
Subjects are excluded who completed the learning phase more than once, were manually identified as outliers, or who failed exclusion criteria.
Finally, data are plotted and exported for inferential statistics in R.
"""

#%%
import pandas as pd
import glob
import numpy as np
import seaborn as sns
import re
import matplotlib.ticker as plticker
import matplotlib
import matplotlib.pyplot as plt

#%%

base_dir = '/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/'

#%% 

# convert dates to something more managable
def clean_timestamp(df): 
    df['timestamp'] = pd.to_datetime(df['timestamp'].str.replace('_',' '))
    df['timestamp_since_start'] =  df['timestamp'] - df['timestamp'].min()
    df['time_diff_days'] = df['timestamp_since_start'].astype("timedelta64[D]")
            
    return df

def add_title(g,title):
    g.fig.subplots_adjust(top=0.9)
    g.fig.suptitle(title, fontsize=24)

def score_response(exp_flag,df):
    
    # cleanup response
    df.loc['response'] = df['response'].apply(lambda x: re.sub(';timeout','',x))
    df.loc['response'] = df['response'].str.lower()

    if exp_flag == 1:
        score_key = pd.read_csv(base_dir + 'exp1_scoring_key.csv',header=0)
    else:
        score_key = pd.read_csv(base_dir + 'exp2/' + 'exp2_scoring_key.csv',header=0)
    
    score_dict = score_key.set_index(['swahili','english'])['correct'].to_dict()
    # map onto part2
    df['correct'] = df.set_index(['stim1','response']).index.map(score_dict)
    # mark zeros where there was no match i.e. incorrect
    df['correct'] = df['correct'].fillna(0)
    return df

def get_sona_dfs(part_path):
    files = glob.glob(part_path + '*.csv')
    
    dfs=[]
    for counter,file in enumerate(files):
        # get some metadata
        id_df = pd.read_csv(file,header=0,nrows=1)
        sona_info = id_df['link'].str.split('id=').tolist()
        link = sona_info[0][0]
        sona_id = sona_info[0][1]
        timestamp = id_df['local_timestamp'].tolist()[0]
        
        # import df and attach metadata
        df = pd.read_csv(file,header=2,dtype={'pair_idx': str})
        df[['sona_id','link','timestamp']] = [sona_id,link,timestamp]
        
        dfs.append(df)
        
    return pd.concat(dfs, ignore_index = True)
                
def get_mturk_dfs(part_path):
    files = glob.glob(part_path + '*.csv')
    
    dfs=[]
    for counter,file in enumerate(files):
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
        
    return pd.concat(dfs, ignore_index = True)

def count_subs(df):
    print(df.sona_id.nunique())      
    
def map_questionaire_responses(part2):
    rowNos = np.arange(33,39)
    filt = part2['rowNo'].isin(rowNos)
    q_aire = part2[filt].copy()
    q_aire = q_aire[['sona_id','response','rowNo','trialText']]
    # clean up response
    q_aire['response'] = q_aire['response'].apply(lambda x: x.split(';')[0])
    
    piv = q_aire.pivot(index='sona_id',columns = 'rowNo',values='response')
    piv = piv.reset_index()
    piv.columns = ['sona_id','fam','eff','acc_answers','use_data','p1_notes','p2_notes']
    
    # remove na
    piv['fam'] = piv.fam.apply(lambda x: 0 if x =='na' else x )
    
    # to numeric
    piv = piv.apply(lambda x: pd.to_numeric(x) if x.name not in ['sona_id'] else x)
    # recode response of 'no' (coded as 2) to 0
    recode = {2:0,1:1}
    piv = piv.apply(lambda x:  x.map(recode) if x.name in ['p1_notes','p2_notes','acc_answers','use_data'] else x)
    # peek at correlations 
    # piv_cor  = piv.corr()  
    return part2.merge(piv,on='sona_id')

def instructions_acc(part1):
    filt = part1[(~part1['trialText'].isnull()) & (part1['trialText'].str.contains('Instructions Check'))].copy() 
    filt['p1_instructions_acc'] = filt.groupby('sona_id')['correct'].transform('mean')
    filt = filt[['sona_id','p1_instructions_acc']].drop_duplicates()
    part1= part1.merge(filt,on='sona_id')
    return part1

def presTime_cleanup(data):
    data['Proportion Correct'] = data['correct']
    data['presTime'] = data['presTime'].astype(int)
    data['presTime'] = data['presTime'].replace(10,0)
    data['presTime'] = data.apply(lambda x: "{:,}".format(x['presTime']), axis=1)
    data['Stimulus Onset Asynchrony'] = data['presTime']
    return data

def category_factor(df,var,ordered=False,categories=None):
    # df[var] = df[var].astype('category')
    if categories:
        cat_type = pd.CategoricalDtype(categories=categories, ordered=ordered)
    else:
        cat_type = pd.CategoricalDtype(categories=df[var].unique(), ordered=ordered)   
    df[var] = df[var].astype(cat_type)
    return df

#%% plotting functions

# main plot, mean correct
def plot_main(data,exp_flag):      
    data = data.groupby(['sona_id','presTime'])['correct'].mean().reset_index()  
    data = presTime_cleanup(data)
    
    if exp_flag ==3:
        data = category_factor(data, var='Stimulus Onset Asynchrony',categories=['0','200','400','600'])
    
    sns.set_style("ticks")
    g = sns.catplot(data=data,x='Stimulus Onset Asynchrony',y='Proportion Correct',kind='bar',ci=68)
    
    params = {'axes.facecolor':'white', 'figure.facecolor':'white','figure.dpi': 300,'axes.labelsize': 18,'axes.titlesize':20, 'legend.fontsize': 20, 'xtick.labelsize': 20, 'ytick.labelsize': 20}
    matplotlib.rcParams.update(params)

    loc = plticker.MultipleLocator(base=.1) # this locator puts ticks at regular intervals
    for ax in g.axes.flat:
        ax.set(ylim=(0, .5))
        ax.yaxis.set_major_locator(loc)
        ax.yaxis.set_major_formatter(plticker.ScalarFormatter())
        ax.set_xlabel("Stimulus Onset Asynchrony (ms)",labelpad=5)
        ax.tick_params(bottom=False)
        
    title ='Experiment ' + str(exp_flag) #+': 24 Hour Delay'
    add_title(g,title)
    
    plt.tick_params(axis='both', which='major', labelsize=14)

    g.savefig(home_dir + '/figures/' + title + '.png')
    return data 

# exp3 has additional block factor
def plot_exp3_cb(data,home_dir):
    # plot presTime 10 next to presTime particpants had (subjects only had 2 distinct presTimes)
    cb_data = data.copy().groupby(['sona_id','presTime','gap_presTime','cb_condition'])['correct'].mean().reset_index()
    cb_data['Difference from 0 ms'] = cb_data.groupby(['sona_id'])['correct'].diff()
    cb_data = cb_data.dropna()
    cb_data = presTime_cleanup(cb_data)
    cb_data = category_factor(cb_data, var='Stimulus Onset Asynchrony',categories=['200','400','600'],ordered=True)
    cb_data['Block Order'] = cb_data['cb_condition'].map({'nogap_gap':'No gap first','gap_nogap':'No gap second'}) 
    
    sns.set_style("ticks")
    g = sns.catplot(data=cb_data,x='Stimulus Onset Asynchrony',y='Difference from 0 ms',hue='Block Order',kind='bar',ci=68,legend=None,
                         palette={'No gap first':sns.color_palette()[6], 'No gap second':sns.color_palette()[9]})
    
    params = {'axes.facecolor':'white', 'figure.facecolor':'white','figure.dpi': 300,'axes.labelsize': 18,'axes.titlesize':20, 'legend.fontsize': 18, 'xtick.labelsize': 20, 'ytick.labelsize': 20}
    matplotlib.rcParams.update(params)
    
    loc = plticker.MultipleLocator(base=.1) # this locator puts ticks at regular intervals
    for ax in g.axes.flat:
        ax.set(ylim=(-.1, .2))
        ax.yaxis.set_major_locator(loc)
        ax.yaxis.set_major_formatter(plticker.ScalarFormatter())
        ax.set_xlabel("Stimulus Onset Asynchrony (ms)",labelpad=5)
        ax.tick_params(bottom=False)
        ax.axhline(0,c='black')
        
    title='Experiment 3'
    g.fig.subplots_adjust(top=.75)
    g.fig.suptitle(title, fontsize=24)
    plt.legend(bbox_to_anchor=(.5, 1.2),ncol=2, loc='upper center', borderaxespad=0,frameon=False)
    g.savefig(home_dir + '/figures/' + title + '_cbgroup.png')
    # end plot_exp3_cb
    
#%% munge data for plots and export

def preprocess(base_dir,exp_flag,debug_flag,merge_dfs=0):
        
    if exp_flag == 1:
        home_dir = base_dir +'exp1/'
    if exp_flag == 2:
        home_dir = base_dir + 'exp2/data/' # this script needs work to run exp 2, for stats 
    if exp_flag == 3:
        home_dir = base_dir + 'exp3_mturk/'
    if exp_flag == 31:
        home_dir = base_dir + 'exp3_sonacredit/'
    if exp_flag == 4:
        home_dir = base_dir + 'exp4a_mturk/'
    if exp_flag == 41:
        home_dir = base_dir + 'exp4a_mturk/rerun/'
    
    # sona version
    if exp_flag in [1,2,31]:
        part1 = get_sona_dfs(home_dir +  '/part1/results/')
        part2 = get_sona_dfs(home_dir +  '/part2/results/') 
    
    if exp_flag == 31:
        part1s = get_sona_dfs(home_dir +  '/part1/results/')
        part2s = get_sona_dfs(home_dir +  '/part2/results/')
        # first 6 subs to complete sona credit in low N groups
        keepers = []
        part1s = part1s[part1s['sona_id'].isin(keepers)]
        part2s = part2s[part2s['sona_id'].isin(keepers)] 
        merge_dfs = 1
        exp_flag = 3
        home_dir = base_dir + 'exp3_mturk/'
    
    # mturk version
    if exp_flag in [3,4,41]:
        part1 = get_mturk_dfs(home_dir +  '/part1/results/')
        part2 = get_mturk_dfs(home_dir +  '/part2/results/') 
            
    if debug_flag:
        breakpoint()
        
    count_subs(part1)
    count_subs(part2)
        
    #% drop subjects that participated in previous EDL experiment
    # first load doubles
    Warning('\n have you updated participant db? \n')
    db_dir = '/Volumes/GoogleDrive/My Drive/grad_school/DML_WBR/dissertation_drive/EDL/paid_EDL/'
    participant_db = pd.read_csv(db_dir + "participant_db_8_5_22.csv",)
    doubles = participant_db[participant_db.duplicated(subset='sona_id',keep=False)]
    # filter duplicates to keep earliest participation
    firsts = doubles.sort_values('GMT_timestamp').drop_duplicates(subset = 'sona_id',keep='first')

    if exp_flag == 1:
        exp_key = '1'
    elif exp_flag == 2:
        exp_key = '2'
    elif exp_flag == 3:
        exp_key = '3'
    elif exp_flag == 4:
        exp_key = '4a'
    elif exp_flag == 41:
        exp_key = '41a'
        
    # drop list contains all duplicates subjects, unless their first was current experiment        
    drop_subs = firsts[firsts['exp'] != exp_key]['sona_id'].tolist()

    part1 = part1[~part1.sona_id.isin(drop_subs)].copy()
    part2 = part2[~part2.sona_id.isin(drop_subs)].copy()

    count_subs(part1)
    count_subs(part2)
    
    # drop participants who learned twice 
    part1 = clean_timestamp(part1)
    drop_df = part1.drop_duplicates(subset =['sona_id','timestamp'])[['sona_id','timestamp']]
    drop_subs = drop_df[drop_df.duplicated(subset='sona_id',keep=False)]['sona_id'].tolist()
    part1 = part1[~part1.sona_id.isin(drop_subs)].copy()
    part2 = part2[~part2.sona_id.isin(drop_subs)].copy()
    
    count_subs(part1)
    count_subs(part2)
    
    # drop second part 2 attempts
    part2 = clean_timestamp(part2)
    unique_ids = part2.drop_duplicates(subset=['sona_id','timestamp'])[['sona_id','timestamp']]
    # drop second entry for a subject
    keep_ids = unique_ids.sort_values('timestamp').drop_duplicates(subset=['sona_id'],keep='first')
    part2 = part2[part2.timestamp.isin(keep_ids.timestamp.tolist())].copy() 

    count_subs(part1)
    count_subs(part2)  
     
    if merge_dfs:
        part1 = pd.concat([part1s,part1])
        part2 = pd.concat([part2s,part2])

    #% drop subjects that have any NaNs in part2 response. 
    NaNsubs = part2[part2['response'].isnull()]['sona_id'].unique()
    part2 = part2[~part2['sona_id'].isin(NaNsubs)].copy()

    count_subs(part1)
    count_subs(part2)    
    
    # get the presTime for part1 rep 1 and map onto part2 items. These are the experimental conditions.
    if exp_flag == 3: # add counterbalancing factor to part2 from part1, and condition label  
        part1_bins = part1[['sona_id','presTime','pair_idx','chunk']]    
        # part1 dict 
        condition_dict = part1_bins.set_index(['sona_id','pair_idx'])[['presTime','chunk']].to_dict()
        # map onto part2
        part2['presTime'] = part2.set_index(['sona_id','pair_idx']).index.map(condition_dict.get('presTime'))
        part2['block'] = part2.set_index(['sona_id','pair_idx']).index.map(condition_dict.get('chunk'))
        
        # create column that indicates the counterbalancing condition at the participant level
        cb_condition = part2[['sona_id','presTime','block']].drop_duplicates()
        cb_condition = cb_condition[~cb_condition['presTime'].isnull()]
        cb_condition = cb_condition[cb_condition.block == 1]
        # if presTime is 10 and block is one, nogap_gap, else gap_nogap
        def cond_func(row):
            if (row.presTime == 10) and (row.block == 1):
                return "nogap_gap"
            else:
                return "gap_nogap"
        cb_condition['cb_condition'] = cb_condition.apply(cond_func,axis=1)
        cb_condition = cb_condition[['sona_id','cb_condition']]
        part2 = part2.merge(cb_condition)
        
        # create column that indicates the "gap" presTime at the participant level
        cb_condition = part2[['sona_id','presTime']].drop_duplicates()
        cb_condition = cb_condition[~cb_condition['presTime'].isnull()]
        cb_condition = cb_condition[cb_condition.presTime > 10]
        cb_condition['gap_presTime'] = cb_condition['presTime']
        cb_condition = cb_condition[['sona_id','gap_presTime']]  
        part2 = part2.merge(cb_condition)
        
        part1.sona_id.nunique()
        part2.sona_id.nunique()  
    else:
        part1_bins = part1[part1['rep_idx'] ==1]     
        condition_dict = part1_bins.set_index(['sona_id','pair_idx'])['presTime'].to_dict()
        part2['presTime'] = part2.set_index(['sona_id','pair_idx']).index.map(condition_dict.get)
        
    # get first key press
    filt = part1.RTkeys.notnull()
    part1.loc[filt,'first_press'] = part1[filt]['RTkeys'].str.split(';').str[0]
    part1['first_press'] = pd.to_numeric(part1['first_press'])
    
    # keep only cued-recall rows
    raw_data = part2[part2['stim2'] == '???'].copy()
    
    # score it
    raw_data  = score_response(exp_flag,raw_data)
    
    part1.sona_id.nunique()
    part2.sona_id.nunique()
    
    if exp_flag in [1,2]:
        filt = part2[part2.rowNo == 24]
        filt = filt[['sona_id','response']]
        filt['fam'] = pd.to_numeric(filt['response'],errors='coerce')
        filt.drop('response',inplace=True,axis=1)
        part2 = part2.merge(filt,on='sona_id')
    
    count_subs(part1)
    count_subs(part2)
    
    #% add meta and look at data
    part1 = instructions_acc(part1)
    part2 = part2.merge(part1[['sona_id','p1_instructions_acc']].drop_duplicates(),on='sona_id')
    
    count_subs(part1)
    count_subs(part2)
    
    if debug_flag:
        breakpoint()
        check = part2[part2.time_diff_days > 313].sona_id.nunique()
            
    def drop_subs(df,drop_these):
        return df[~df.sona_id.isin(drop_these)]
        
    if exp_flag in [3,4,41]:
        if debug_flag:
            breakpoint()
            part2[part2.time_diff_days > 313].sona_id.nunique()
        part2 = map_questionaire_responses(part2)
        
        if exp_flag == 4:
            # this subs were contaminated by participating in the single session EDL already 
            drop4a = []
            look = part2[['sona_id','fam','eff','acc_answers','use_data','p1_notes','p2_notes','p1_instructions_acc']].drop_duplicates()
            look = drop_subs(look,drop4a)
            # time of day outlier
            tod_outlier = []
            look = drop_subs(look,tod_outlier)
        
        if exp_flag ==3:
            # first 3 were repeats from different experiment, next 13 were id'd as same individual 
           dropfrom3 =  []

           look = part2[['sona_id','fam','eff','acc_answers','use_data','p1_notes','p2_notes','p1_instructions_acc','time_diff_days']].drop_duplicates()
           look = drop_subs(look,dropfrom3)
           
        if exp_flag == 41:
            look = part2[['sona_id','fam','eff','acc_answers','use_data','p1_notes','p2_notes','p1_instructions_acc']].drop_duplicates()

        if debug_flag:
            breakpoint()
            check = look[look.time_diff_days>313] #80
        look.sona_id.nunique()
        
        high_fam = look[look.fam > 1]['sona_id'].tolist()
        low_eff = look[look.eff < 60]['sona_id'].tolist()
        instr_fail = look[look['p1_instructions_acc'] < .75]['sona_id'].tolist() 
        took_notes = look[(look['p1_notes'] ==1) | (look['p2_notes'] ==1)]['sona_id'].tolist() 
        inacc_ans = look[look['acc_answers'] ==0]['sona_id'].tolist() 

    if exp_flag in [1,2]:
        sub_data = raw_data.groupby(['sona_id'])['correct'].agg(['mean', 'count']).reset_index().drop_duplicates()
        sub_data = sub_data.merge(part2[['sona_id','fam','p1_instructions_acc']].drop_duplicates())
        high_fam = sub_data[sub_data.fam > 1]['sona_id'].drop_duplicates().tolist()    
        sub_data = drop_subs(sub_data, high_fam)
        sub_data['z_correct']=(sub_data['mean']-sub_data['mean'].mean())/sub_data['mean'].std() 
    else: 
        sub_data = raw_data.groupby(['sona_id'])['correct'].agg(['mean', 'count']).reset_index()
        sub_data['z_correct']=(sub_data['mean']-sub_data['mean'].mean())/sub_data['mean'].std()
        
        if exp_flag == 4:
            sub_data = drop_subs(sub_data, drop4afrom4b)
        sub_data = drop_subs(sub_data, took_notes)
        sub_data = drop_subs(sub_data, inacc_ans)
        sub_data = drop_subs(sub_data, instr_fail)
        sub_data = drop_subs(sub_data, high_fam)
        sub_data = drop_subs(sub_data, low_eff) 
         
        sub_data = sub_data.merge(look,on='sona_id') # add meta
            
    if exp_flag ==3:
        sub_data = drop_subs(sub_data,dropfrom3)
    
    good_subs = sub_data.sona_id.unique().tolist()
    data = raw_data[raw_data['sona_id'].isin(good_subs)]
    
    return data,part1,good_subs,raw_data,home_dir
    # end preprocess
    
#%% run prepeprocess and plot_main

data,part1,good_subs,raw_data,home_dir = preprocess(base_dir,exp_flag)
plot_main(data,exp_flag)

#%% Learning Data. PLot and export csv.

def learn_data(part1, good_subs):    
    # only subs used in part 2 plot
    p1_good_subs = part1[part1.sona_id.isin(good_subs)]
    
    # first press
    learn_data = p1_good_subs.groupby(['sona_id','presTime','rep_idx'])['first_press'].mean().reset_index()
    
    # plot
    g= sns.catplot(data=learn_data,x='presTime',y='first_press',hue='rep_idx',kind='bar',ci=68)
    title ='Experiment ' + str(exp_flag) + ' Learning First Press x Rep' 
    add_title(g,title)
    g.savefig(home_dir + '/figures/' + title + '.png') 
    
    # first press mean over reps
    submean_firstpress = p1_good_subs.groupby(['sona_id','presTime'])['first_press'].mean().reset_index()
    g = sns.catplot(data=submean_firstpress,x='presTime',y='first_press',kind='bar',ci=68)
    title ='Experiment ' + str(exp_flag) + ' Learning First Press' 
    add_title(g,title)
    g.savefig(home_dir + '/figures/' + title + '.png') 
    
    # learning accuracy 
    p1_good_subs = part1[part1.sona_id.isin(good_subs)].copy()
    # p1_good_subs = clean_timestamp(p1_good_subs)
    # drop practice trials (only experimental trials have numeric pair_idx)
    p1_good_subs = p1_good_subs[pd.to_numeric(p1_good_subs['pair_idx'], errors='coerce').notnull()]
    
    # clean responses
    # remove timeout
    p1_good_subs.loc['response'] = p1_good_subs['response'].apply(lambda x: re.sub(';timeout','',x))
    # lowercase
    p1_good_subs.loc['response'] = p1_good_subs['response'].str.lower()
    # remove NaN
    p1_good_subs = p1_good_subs[~p1_good_subs['response'].isnull()]
    p1_acc  = score_response(exp_flag,p1_good_subs)
        
    learn_acc = p1_acc.groupby(['sona_id','presTime','rep_idx'])['correct'].mean().reset_index()
    p1_acc.groupby(['presTime','rep_idx'])['correct'].mean().reset_index()
    p1_acc.groupby(['stim4'])['correct'].mean().reset_index()
    
    g = sns.catplot(data=learn_acc,x='presTime',y='correct',hue='rep_idx',kind='bar',ci=68)
    title ='Experiment ' + str(exp_flag) + ' Learning Accuracy x Rep'
    add_title(g,title)
    g.savefig(home_dir + '/figures/' + title + '.png') 
    
    submean_learn_acc = p1_acc.groupby(['sona_id','presTime'])['correct'].mean().reset_index()
    g = sns.catplot(data=submean_learn_acc,x='presTime',y='correct',kind='bar',ci=68)
    title ='Experiment ' + str(exp_flag) + ' Learning Accuracy'
    add_title(g,title)
    g.savefig(home_dir + '/figures/' + title + '.png') 
    
    #% learning data to csv
    learn_forr = learn_data.merge(learn_acc)
    learn_forr.to_csv(home_dir + 'exp' + str(exp_flag) + '_learning_data.csv',index=False)
    # end learn_data

#%% compute rt accerlation across repititions

def rt_acceleration(raw_data, good_subs, exp_flag):
    part2data = raw_data[raw_data.sona_id.isin(good_subs)].copy()
    
    if exp_flag == 1: # number of repetition difference argh
        reps = np.arange(1,6) # list of repetitions 
        for rep in reps:
            part1_rt = part1[part1['rep_idx'] ==rep]    
            condition_dict = part1_rt.set_index(['sona_id','pair_idx'])['first_press'].to_dict()
            part2data['first_press_' + str(rep)] = part2data.set_index(['sona_id','pair_idx']).index.map(condition_dict.get)
            
        reps = np.arange(1,6) # list of repetitions 
        for rep in reps:
            part2data['fp_diff_' + str(rep)] = part2data['first_press_1'] -  part2data['first_press_' + str(rep)]
        
        # gather first press difference data
        fp_diffs = pd.melt(part2data,id_vars=['sona_id','stim1','correct','presTime'],value_vars=['fp_diff_1','fp_diff_2','fp_diff_3','fp_diff_4','fp_diff_5'], value_name='fp_diff',var_name='repetition') 
    else:
        reps = np.arange(1,5) # list of repetitions 
        for rep in reps:
            part1_rt = part1[part1['rep_idx'] ==rep]    
            condition_dict = part1_rt.set_index(['sona_id','pair_idx'])['first_press'].to_dict()
            part2data['first_press_' + str(rep)] = part2data.set_index(['sona_id','pair_idx']).index.map(condition_dict.get)
            
        reps = np.arange(1,5) # list of repetitions 
        for rep in reps:
            part2data['fp_diff_' + str(rep)] = part2data['first_press_1'] -  part2data['first_press_' + str(rep)]
        
        # gather first press difference data
        fp_diffs = pd.melt(part2data,id_vars=['sona_id','stim1','correct','presTime'],value_vars=['fp_diff_1','fp_diff_2','fp_diff_3','fp_diff_4'], value_name='fp_diff',var_name='repetition') 
    g = sns.catplot(data=fp_diffs,x='presTime',y='fp_diff',hue='repetition',kind='bar',ci=68)
    add_title(g,title='Learning RT acceleration Experiment ' + str(exp_flag))
    # end rt_acceleration

#%% output data to R

def export_data(raw_data, good_subs, exp_flag,filename):
    forr = raw_data[raw_data['sona_id'].isin(good_subs)]
    
    # add first press
    if exp_flag == 1:
        fp_df = part1[part1['rep_idx'] == 5]
    else:
        fp_df = part1[part1['rep_idx'] == 4]
    fp_df = fp_df[fp_df['sona_id'].isin(good_subs)]
    fp_df = fp_df.copy()[['sona_id','stim1','first_press']].drop_duplicates()
    
    forr = forr.merge(fp_df,on=['sona_id','stim1'])
    forr = forr.merge(fp_diffs,on=['sona_id','stim1','correct','presTime'])
    
    forr.to_csv(home_dir + filename + '.csv',index=False)

#%% conditionalize recall accuracy on first_press

def plot_acc_firstpress(raw_dta,good_subs,part1,exp_flag):
    part2data = raw_data[raw_data.sona_id.isin(good_subs)].copy()
    part1_rt = part1[part1['rep_idx'] ==4]     
    condition_dict = part1_rt.set_index(['sona_id','pair_idx'])['first_press'].to_dict()
    part2data['first_press'] = part2data.set_index(['sona_id','pair_idx']).index.map(condition_dict.get)
    
    def qcut_func(x):
        return pd.qcut(x,q=4,labels=range(1,5))
            
    part2data['first_press_qtile'] = part2data.groupby('presTime')['first_press'].transform(lambda x: qcut_func(x))
    
    g = sns.catplot(data=part2data,x='first_press_qtile',y='correct',col='presTime',kind='bar',ci=68)
    add_title(g,title='Effect of Learning RT Experiment' + str(exp_flag))

#%% difference plot

def difference_plot(raw_data,good_subs,exp_flag):
    if exp_flag == 3:
        data = raw_data[raw_data['sona_id'].isin(good_subs)]
        data = data.groupby(['sona_id','presTime','timestamp'])['correct'].mean().reset_index()  
        data = data[['sona_id','presTime','correct','timestamp']]
        
        data['diff'] = data.groupby('sona_id')['correct'].diff() # diff function does row to row difference!
        data = data.dropna()
        
        sns.catplot(data=data,x='presTime',y='diff',kind='bar',ci=68)
            
        # by block
        data = raw_data[raw_data['sona_id'].isin(good_subs)]
        data = data.groupby(['sona_id','presTime','cb_condition','timestamp'])['correct'].mean().reset_index() 
        data['difference'] = data.groupby(['sona_id'])['correct'].diff()
        data = data.dropna()
        
        sns.catplot(data=data,x='presTime',y='difference',kind='bar',hue='cb_condition',ci=68)
    else:          
        data = raw_data[raw_data['sona_id'].isin(good_subs)]
        data = data.groupby(['sona_id','presTime','timestamp'])['correct'].mean().reset_index()  
        data = data[['sona_id','presTime','correct','timestamp']]
        
        # compute diff between study(presTime = 10) and each test condition within subject
        # make a df of study accuracies
        studydf = data[data.presTime == 10].copy()
        studydf.drop(['presTime','timestamp'],axis=1,inplace=True)
        studydf.columns = ['sona_id','study_acc']
        # merge creating new column "study_acc"
        data = data.merge(studydf,on='sona_id')
        data['difference'] = data['correct'] - data['study_acc']
        # zeros for study presTime indicate that it worked, now drop them
        data = data[data['presTime'] != 10]
        
        sns.catplot(data=data,x='presTime',y='difference',kind='bar',ci=68)
    # end difference_plot
    
#%%


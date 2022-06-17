#!/usr/bin/env python
# coding: utf-8

# ## Data Analysis code working with June 2022 data 

# In[1]:


from sklearn.ensemble import ExtraTreesClassifier, ExtraTreesRegressor
import numpy as np
import pandas as pd 
import os
import matplotlib.pyplot as plt
import seaborn as sns
import geopandas as gpd

def get_category(v, threshold) : 
    for i in range(0, len(threshold)) :
        if v<=threshold[i] : 
            return i+1
    return len(threshold)+1

def z_score(df):
    # copy the dataframe
    df_std = df.copy()
    # apply the z-score method
    for column in df_std.columns:
        df_std[column] = (df_std[column] - df_std[column].mean()) / df_std[column].std()
        
    return df_std


# In[2]:


#def feature sets realted fucntions and constants
label_col='COUNT'
violation_features=['Animal' , 'Health hazard', 'NoGrouper', 'Property maintenance', 'Substandard building', 'Vehicle', 'Waste violation']
crime_features=['AGGRAVATED','ANIMAL','ASSAULT','CDS','FAMILY','MISCHIEF','OVERDOSE','RAPE','ROBBERY','SEX']

# model A: include 8361; model B: exclude 8361
#factor_features= list(map(str, [7231, 7538, 7241, 8351, 5411, 8661, 7011, 5921, 5912, 8021, 7542, 8011, 8322, 5813, 5812, 5541, 7215, 8361, 5932]))
#risk_features=list(map(str, [7231, 7538, 7241, 5411, 7011, 5921,  7542,  5813, 5812, 5541, 7215, 8361,  5932]))
factor_features= list(map(str, [7231, 7538, 7241, 8351, 5411, 8661, 7011, 5921, 5912, 8021, 7542, 8011, 8322, 5813, 5812, 5541, 7215, 5932]))
risk_features=list(map(str, [7231, 7538, 7241, 5411, 7011, 5921,  7542,  5813, 5812, 5541, 7215,  5932]))

protect_features=list(map(str,[5912, 8351, 8661, 8021, 8011, 8322]))
census_features=[
    'EP_BLACK', 'EP_NON_WHITE', 'EP_HISPANIC_LATINO', 'EP_AGE17', 'EP_COLLEGE_ED',
    'EP_NOHSDP', 'EP_POV', 'EP_SNGPNT', 'EP_HH_FEMALE_HEADED', 'EP_NON_MARRIED', 'EP_PUBLIC_INSURANCE',
    'EP_UNINSUR', 'EP_HU_OWNER_OCCUPIED', 'EP_RENTER_OCCUPIED'
]

def sum_f(li) : return [ x+"_SUM" for x in li]
def NN_f(li) : return [ x+"_NN" for x in li]
def kNN_f(li) : return [ x+"_5NN" for x in li]

featuresDF=pd.read_csv(os.path.join('./', 'consolidated_features_with_census.csv'))
print(featuresDF.shape, featuresDF.columns)
f_dict=featuresDF.set_index('code').T.to_dict('list')


# In[3]:


def get_feature_description (f) :
    if not f in f_dict.keys():
        _fs=f.split('_')
        if len(_fs) == 2 : 
            label = f_dict.get(_fs[0])[1]+'(' + _fs[1]+')'
        elif _fs[0]=='No' : #temproray branch, need change the name 
            label = f_dict.get("No_Grouper")[1]+'(' + _fs[1]+')'
        else : 
            label = f_dict.get(_fs[0])[1]
    else:
        label = (f_dict[f])[1]
    #print(f'Label for feature {f} is {label}.')
    return label

def get_feature_type (f) :
    
    if not f in f_dict.keys():
        _fs=f.split('_')
        if len(_fs) == 2 : 
            label = f_dict.get(_fs[0])[0]+'(' + _fs[1]+')'
        elif _fs[0]=='No' : #temproray branch, need change the name 
            label = f_dict.get("No_Grouper")[0]+'(' + _fs[1]+')'
        else :
            label = f_dict.get(_fs[0])[0]  
    else:
        label = (f_dict[f])[0]  # only census variables are direct matches to keys
        
    #print(f'Label for feature {f} is {label}.')
    return label


# In[4]:


#print(get_feature_description('265_SUM'))
#print(get_feature_description('5541_NN'), get_feature_description('CDS_5NN'), get_feature_description('61-55'))

all_v_f=sum_f(violation_features)+NN_f(violation_features)+kNN_f(violation_features)
all_c_f=sum_f(crime_features)+NN_f(crime_features)+kNN_f(crime_features)
all_f_f=sum_f(factor_features)+NN_f(factor_features)+kNN_f(factor_features)
risk_f=sum_f(risk_features)+NN_f(risk_features)+kNN_f(risk_features)

all_f= all_v_f+all_c_f+all_f_f+census_features
riskonly_f = all_v_f+all_c_f+risk_f+census_features

all_sum=sum_f(violation_features)+sum_f(crime_features)+sum_f(factor_features)
all_f_description=[get_feature_description(f) for f in all_f]
riskonly_f_description= [get_feature_description(f) for f in riskonly_f]


# ## 0. read the data and features file generated previously  

# In[5]:


model_version = 'A'
_path=f'/Users/kpierce/PredictAlignFishnets/Model{model_version}_0.5PLANAR/'
_fn='all_features_grid.csv'

data=pd.read_csv(os.path.join(_path, _fn))

qs=[0.3, 0.5, 0.7, 0.9]
count100_threshold=[data[data['COUNT']>0]['COUNT100'].quantile(q) for q in qs ]
data['risk_category_count100']=data['COUNT100'].apply(lambda x: get_category(x, count100_threshold))
count_threshold=[data[data['COUNT']>0]['COUNT'].quantile(q) for q in qs ]
data['risk_category_count']=data['COUNT'].apply(lambda x: get_category(x, count_threshold))

print(data.shape, data.columns)
#data.head(3)


# ## 1. Feature Importances

# In[5]:


from sklearn.ensemble import ExtraTreesClassifier, ExtraTreesRegressor

label_col='COUNT'
features=riskonly_f_description
data = data.dropna() # input data has zeros for excluded grid cells

X=data[riskonly_f]#[all_f]
Y=data[label_col]
X_standardized = z_score(X)


# In[6]:


X_standardized.shape


# In[7]:


# feature extraction
model = ExtraTreesRegressor(n_estimators=50)
model.fit(X_standardized , Y)
print(model.feature_importances_)

std = np.std([tree.feature_importances_ for tree in model.estimators_],
             axis=0)
data_dict={'n': features, 'i': model.feature_importances_, 'e': std }
feat_importances = pd.DataFrame(data_dict).sort_values(by="i")
feat_importances.set_index("n",drop=True,inplace=True)
feat_importances.head()

plot=feat_importances.plot(kind='barh', y='i', xerr='e', figsize=(10,50), legend=False)

feat_importances.sort_values(by="i", ascending=False).to_csv(os.path.join(_path, 'feat_importances.csv'))
plot.get_figure().savefig(os.path.join(_path, "feat_importances.png"), dpi=240)


# In[8]:


risks=data[[label_col]+riskonly_f].copy()
risks[riskonly_f]=z_score(data[riskonly_f])

corrMatrix = risks.corr()

corr=corrMatrix['COUNT'].reset_index().iloc[1:]
corr.columns=['var_name', 'corr_score']
corr['var_desc']=corr['var_name'].apply(lambda x : get_feature_description(x) )
corr.to_csv(os.path.join(_path, 'correlation_score.csv'))

labels=[get_feature_description(x) for x in list(corrMatrix.index[1:])]
labels=['COUNT']+labels
plt.figure(figsize=(30, 30), dpi=240)
plot=sns.heatmap(corrMatrix, 
                xticklabels=labels, yticklabels=labels, 
                annot=True, annot_kws={"fontsize":4})
plot.get_figure().savefig(os.path.join(_path, "correlation_score.png"), dpi=240)


# ## 2. Model Testing 
# ### 2a CV testing with different models 

# In[9]:


from sklearn.ensemble import ExtraTreesClassifier, ExtraTreesRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import PoissonRegressor
from sklearn.model_selection import cross_validate
import math

label_col='COUNT'
features=riskonly_f_description

X=data[riskonly_f+['NETPOP']]
#_data['LOGNETPOP']=np.log(_data['NETPOP']+1)
#X=
#X=data[risk_f]
Y=data[label_col]

X_standardized = z_score(X)

# feature extraction
etr = ExtraTreesRegressor(n_estimators=300)
regr = RandomForestRegressor(max_depth=10,  n_estimators=100)
glm=PoissonRegressor(alpha=0.001, max_iter=3000)

cv_results = cross_validate(etr, X_standardized, Y, cv=5,
               scoring=('r2', 'neg_mean_absolute_error', 'neg_mean_squared_error' ),
               return_train_score=True)
MAE=-cv_results['test_neg_mean_absolute_error'].mean()
RMSE=math.sqrt(-cv_results['test_neg_mean_squared_error'].mean())
R2=cv_results['test_r2'].mean()
print(R2, MAE, RMSE)
print(cv_results.keys())
print(cv_results)

cv_results = cross_validate(regr, X_standardized, Y, cv=5,
               scoring=('r2', 'neg_mean_absolute_error', 'neg_mean_squared_error' ),
               return_train_score=True)
MAE=-cv_results['test_neg_mean_absolute_error'].mean()
RMSE=math.sqrt(-cv_results['test_neg_mean_squared_error'].mean())
R2=cv_results['test_r2'].mean()
print(R2, MAE, RMSE)
print(cv_results.keys())
print(cv_results)

cv_results = cross_validate(glm, X_standardized, Y, cv=5,
               scoring=('r2', 'neg_mean_absolute_error', 'neg_mean_squared_error' ),
               return_train_score=True)
MAE=-cv_results['test_neg_mean_absolute_error'].mean()
RMSE=math.sqrt(-cv_results['test_neg_mean_squared_error'].mean())
R2=cv_results['test_r2'].mean()
print(R2, MAE, RMSE)
print(cv_results.keys())
print(cv_results)


# ## 2b building Extratree submodels

# In[10]:


from sklearn.ensemble import ExtraTreesClassifier, ExtraTreesRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import PoissonRegressor
from sklearn.model_selection import cross_validate, train_test_split
from sklearn.metrics import mean_absolute_error, r2_score
 
import pickle

#label_col='COUNT100'
_vars=riskonly_f
features=_vars+['LOGNETPOP']
features_description=riskonly_f_description

#_data=data[data.area.isin(['Laurel Lake, NJ Urban Cluster','rural'])].copy()
#_data=data[data.area.isin(['rural'])].copy()
#_data=data[data.area.isin(['Bridgeton, NJ Urban Cluster'])].copy()
_data=data.copy()
#_data=data[data.year.isin([2017, 2018, 2019])].copy()
_data[_vars]=z_score(_data[_vars]).fillna(0)
_data['LOGNETPOP']=np.log(_data['NETPOP']+1)
#_data['LOGCOUNT']=np.log(10*data['COUNT']+0.0000001)
#label_col='LOGCOUNT'


label_col='COUNT'

#X=_data[features]
#X_standardized = z_score(X).fillna(0)
X_standardized = _data[features]

#X=data[risk_f]

Y=_data[label_col]

#X=data[data.area=='Bridgeton, NJ Urban Cluster'][features]
#Y=data[data.area=='Bridgeton, NJ Urban Cluster'][label_col]



# feature extraction
etr = ExtraTreesRegressor(n_estimators=300)
#regr = RandomForestRegressor(max_depth=10,  n_estimators=300)
#glm=PoissonRegressor(alpha=0.01, max_iter=10000)

# run five iteratation and choose best models out. 
mae=10000000
model=etr #glm #regr #etr
best_model=model
training_mae=10000000
training_R2=0
for i in range (0, 5) :
    X_train, X_test, y_train, y_test = train_test_split(X_standardized, Y)
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    _mae=mean_absolute_error(y_test, y_pred)
    coefficient_of_dermination = r2_score(y_test, y_pred)
    print(_mae, coefficient_of_dermination)
    print("validation MAE: %.3f" % _mae)
    #if _mae < mae :
    if coefficient_of_dermination > training_R2  :
        best_model=model
        mae=_mae
        training_mae= _mae
        training_R2= coefficient_of_dermination

test=_data.copy()
preds_col=label_col+'_preds'
#loaded_model = pickle.load(open(filename, 'rb'))
test[preds_col]=best_model.predict(X_standardized)
test['abs_error']=abs(test[label_col]-test[preds_col])

_mae=mean_absolute_error(test[label_col], test[preds_col])
coefficient_of_dermination = r2_score(test[label_col], test[preds_col])
#test['predictsC']=test['predictsC100']*test['NETPOP']
#test['predictsC']=best_model.predict(X_standardized)
#_mae=mean_absolute_error(test['COUNT'], test['predictsC'])

print("coefficient_of_dermination and MAE for entire data sets on {}: {:.3f}, {:.3f}".format(label_col, coefficient_of_dermination, _mae))
print("total error as percentage to total values: %.3f" % (test['abs_error'].sum()/test[label_col].sum()))
#print("MAE for entire data sets on COUNT100: %.3f" % mean_absolute_error(test['COUNT100'], test['predictsC100']))

results={'training MAE':training_mae, 'trainnig R2': training_R2, 
         'total MAE': _mae, 'total R2': coefficient_of_dermination, 'Total Error Percentage': test['abs_error'].sum()/test[label_col].sum() }
print(results)


# ### 2b.1 Save models and testing data. 
#      save model in pickle file specified by model_name, 
#           training performance in train_reuslt.json, 
#           testing data in csv format

# In[11]:


import json 
import pickle
model_name='extratree_riskonly_f_NETPOP_COUNT.0.5.allyears.model'
#model_name='extratree_riskonly_f_NETPOP_COUNT.landscan.2019.model'
train_stat='train_result.json'
final_pred='total_allyears_predictions.csv'

pickle.dump(best_model, open(os.path.join(_path, model_name), 'wb'))

with open(os.path.join(_path, train_stat), 'w') as outfile:
    json.dump(results, outfile)
    
test.to_csv(os.path.join(_path, final_pred), index=False)

print('Model, stats and predicitons are saved in: ', _path)


# ## 3. Load saved model and run test with different data sets. 
# 3a test with yearly data

# In[12]:


# test on all data

years=[2017, 2018, 2019]
#years=[2019]
test=data[data.year.isin(years)].copy()
#test=data.copy()
#label_col='COUNT100'
_vars=riskonly_f
features=_vars+['LOGNETPOP']
features_description=riskonly_f_description

_data=test.copy()
#_data=data[data.year.isin([2017, 2018])].copy()
_data[_vars]=z_score(_data[_vars]).fillna(0)
_data['LOGNETPOP']=np.log(_data['NETPOP']+1)
#_data['LOGCOUNT']=np.log(10*data['COUNT']+0.0000001)
#label_col='LOGCOUNT'

label_col='COUNT'
#X=_data[features]
#X_standardized = z_score(X).fillna(0)
X_standardized = _data[features]


preds_col=label_col+'_preds'

model_name='extratree_riskonly_f_NETPOP_COUNT.0.5.allyears.model'
#model_name='extratree_riskonly_f_NETPOP_COUNT.landscan.2019.model'
loaded_model = pickle.load(open(os.path.join(_path, model_name), 'rb'))

test[preds_col]=loaded_model.predict(X_standardized)
test['abs_error']=abs(test[label_col]-test[preds_col])

_mae=mean_absolute_error(test[label_col], test[preds_col])

for y in years : 
    _df=test[test.year==y].copy()
    _mae=mean_absolute_error(_df[label_col], _df[preds_col])
    print("MAE for year {}, {}".format(y, _mae))

#test['predictsC']=test['predictsC100']*test['NETPOP']
#test['predictsC']=best_model.predict(X_standardized)
#_mae=mean_absolute_error(test['COUNT'], test['predictsC'])

print("MAE for entire data sets on {}: {:.3f}".format(label_col, _mae))
print("total error as percentage to total values: %.3f" % (test['abs_error'].sum()/test[label_col].sum()))
#print("MAE for entire data sets on COUNT100: %.3f" % mean_absolute_error(test['COUNT100'], test['predictsC100']))

_rs=test[['NETPOP', 'COUNT', 'COUNT_preds', 'abs_error', 'year']].groupby(['year']).sum().reset_index()
_rs['accuracy (1-error/count)']=1-_rs['abs_error']/_rs['COUNT']
print(test[['COUNT100', 'COUNT']].mean())

_ry=_rs.groupby('year')[['COUNT_preds', 'abs_error']].sum().reset_index()
_ry['acc']=1- _ry['abs_error']/_ry['COUNT_preds']
print(_ry[['year','acc']])
_rs.to_csv(os.path.join(_path, 'model_performance.csv'), index=False)


# In[13]:


final_results_save=test[['NETID', 'NETPOP', 'lat', 'long', 'year', 'COUNT', 'COUNT100', 'risk_category_count', 'risk_category_count100', 'COUNT_preds', 'abs_error']].copy()
final_results_save.columns=['NETID', 'NETPOP', 'latitude', 'longitude', 'year', 'COUNT', 'COUNT100', 'risk_by_count', 'risk_by_count100', 'COUNT_preds', 'abs_error']
final_results_save['COUNT100_preds']=100*final_results_save['COUNT_preds']/final_results_save['NETPOP']

qs=[0.3, 0.5, 0.7, 0.9]
count_threshold=[final_results_save[final_results_save['COUNT']>0]['COUNT'].quantile(q) for q in qs ]
count100_threshold=[final_results_save[final_results_save['COUNT']>0]['COUNT100'].quantile(q) for q in qs ]

final_results_save['risk_by_count']=final_results_save['COUNT'].apply(lambda x: get_category(x, count_threshold))
final_results_save['risk_by_count100']=final_results_save['COUNT100'].apply(lambda x: get_category(x, count100_threshold) )

final_results_save['risk_by_count_preds']=final_results_save['COUNT_preds'].apply(lambda x: get_category(x, count_threshold))
final_results_save['risk_by_count100_preds']=final_results_save['COUNT100_preds'].apply(lambda x: get_category(x, count100_threshold) )

final_results_save.to_csv(os.path.join(_path, 'count_predictions_yearly.csv'), index=False)
final_results_save.head()


# ### 3b test with average yearly data

# In[14]:


#prediction with average data from past three years. 
test=_data.copy()
label_col='COUNT'
preds_col=label_col+'_preds'
#loaded_model = pickle.load(open(os.path.join(_path, 'extratree_riskonly_f_NETPOP_COUNT.5.sav'), 'rb'))
#loaded_model = pickle.load(open(os.path.join(_path, 'extratree_riskonly_f_NETPOP_COUNT.landscan.sav'), 'rb'))
loaded_model = pickle.load(open(os.path.join(_path, 'extratree_riskonly_f_NETPOP_COUNT.0.5.allyears.model'), 'rb'))

_vars=riskonly_f
features=_vars+['LOGNETPOP']
features_description=riskonly_f_description

#_data=data[data.area.isin(['Laurel Lake, NJ Urban Cluster','rural'])].copy()
#_data=data[data.area.isin(['rural'])].copy()
#_data=data[data.area.isin(['Bridgeton, NJ Urban Cluster'])].copy()

test[_vars]=z_score(test[_vars]).fillna(0)
test['LOGNETPOP']=np.log(test['NETPOP']+1)
#_data['LOGCOUNT']=np.log(10*data['COUNT']+0.0000001)
#label_col='LOGCOUNT'
label_col='COUNT'

#X_standardized = test[features]
averaged=test[['NETID', 'lat', 'long', 'COUNT', 'NETPOP']+features].groupby(['NETID']).mean().reset_index()

X_standardized = averaged[features]

averaged[preds_col]=loaded_model.predict(X_standardized)


#Y=test[['NETID', label_col]].groupby('NETID').mean()
averaged['abs_error']=abs(averaged[label_col]-averaged[preds_col])

_mae=mean_absolute_error(averaged[label_col], averaged[preds_col])

print(_mae, 1 - averaged['abs_error'].sum()/averaged[label_col].sum() )
averaged['risk_by_count_preds']=averaged['COUNT_preds'].apply(lambda x: get_category(x, count_threshold))
averaged[['NETID', 'NETPOP', 'lat', 'long', preds_col, 'risk_by_count_preds']].to_csv(os.path.join(_path, 'predictions_basedon_yearly_average_full.csv'), index=False)

averaged.to_csv(os.path.join(_path, 'predictions_basedon_yearly_average_full_with_inputs.csv'), index=False)


# ## 4 generate final varaible statistics and output spreadsheet

# In[5]:


#variable statistics using predicted category and yearly average. 

#_fn="predictions_basedon_yearly_average_full.csv"
_fn="count_predictions_yearly.csv"
_res=pd.read_csv(os.path.join(_path, _fn))

results=data.merge(_res[['NETID', 'year','COUNT_preds', 'risk_by_count_preds']], on=['NETID', 'year'], how='left')
print(results.shape, results.columns)

def alltime_variable_stats(colname, df, group_col=['risk_by_count_preds', 'year']) :
    base_col=['NETPOP', 'COUNT']
    col_base =base_col +[colname]
    cols= group_col + col_base
    _df1=df[df[colname]>0][cols]
    agg=_df1.groupby(group_col)[col_base].agg(['sum', 'count']).reset_index()
    return agg


# In[16]:


dfs=[]
all_var_base=factor_features+violation_features+crime_features

years=3
for v in all_var_base : 
    _df=alltime_variable_stats(v+'_SUM', results, group_col=['risk_by_count_preds', 'year'])
    _df.columns=['risk', 'year', 'total affected population', 'total affected cells', 'total incidents occurances', 'count_drop', 
                  'total var occurances', 'count_drop2']
    _df['var_name']=v
    #_df['avg. affected cells per year'] = _df['total affected cells'] / years
    dfs.append(_df[['var_name', 'year', 'risk', 'total affected population', 'total affected cells', 'total incidents occurances',  
                  'total var occurances']])
    
for c in census_features:
    _df=alltime_variable_stats(c, results, group_col=['risk_by_count_preds', 'year'])
    _df.columns=['risk', 'year', 'total affected population', 'total affected cells', 'total incidents occurances', 'count_drop', 
                  'total var occurances', 'count_drop2']
    _df['var_name']=v
    #_df['avg. affected cells per year'] = _df['total affected cells'] / years
    dfs.append(_df[['var_name', 'year', 'risk', 'total affected population', 'total affected cells', 'total incidents occurances',  
                  'total var occurances']])
    
all_var_stats=pd.concat(dfs)
print(all_var_stats.shape, all_var_stats.columns)

total_grid=results['NETID'].nunique()
total_pop=results.groupby('year')['NETPOP'].sum().mean()
all_var_stats['var_desc']=all_var_stats['var_name'].apply(lambda x : get_feature_description(x))
all_var_stats['var_type']=all_var_stats['var_name'].apply(lambda x : get_feature_type(x))
print(total_grid, total_pop, all_var_stats.columns)
all_var_stats.to_csv(os.path.join(_path, 'all_var_grids_0615.csv'), index=False)


# In[17]:


all_var_stats.head()


# In[18]:


all_cat_stats=all_var_stats.groupby(['var_name', 'var_desc', 'var_type', 'year']).sum().reset_index()
all_cat_stats['%grid cell affected']=100*all_cat_stats['total affected cells'] / total_grid
all_cat_stats['%population affected']=100*all_cat_stats['total affected population'] / total_pop
#all_cat_stats
all_cat_stats[['var_desc', 'var_type', 'var_name', 'year', 'total var occurances', 
               'total incidents occurances', 'total affected population', '%population affected', 
               'total affected cells', '%grid cell affected']
             ].to_csv(os.path.join(_path, 'all_cat_stats_0615.csv'), index=False)
print(all_cat_stats.shape, all_cat_stats.columns)


# In[19]:


yearly_cat_stats=all_cat_stats.groupby(['var_name', 'var_desc', 'var_type'])[
    ['total affected population', 'total affected cells', 'total incidents occurances', 'total var occurances']].mean()
yearly_cat_stats.columns=['avg. affected population per year', 'avg. affected cells per year', 'avg. incidents occurances per year', 'avg. var occurances per year']
print(yearly_cat_stats.shape)


# In[20]:


cat5_stats=all_var_stats[all_var_stats.risk>4].groupby(['var_name', 'var_desc', 'var_type', 'year']).sum().reset_index()
cat5_stats['%grid cell affected']=100*all_cat_stats['total affected cells'] / total_grid
cat5_stats['%population affected']=100*all_cat_stats['total affected population'] / total_pop
cat5_yearly_stats=cat5_stats.groupby(['var_name', 'var_desc', 
                                      'var_type'])[['total affected population', 'total affected cells', 
                                                    'total incidents occurances', 'total var occurances']].mean()
cat5_yearly_stats.columns=['avg. affected population per year', 'avg. affected cells per year', 'avg. incidents occurances per year', 'avg. var occurances per year']
cat5_yearly_stats
print(cat5_stats.shape, cat5_yearly_stats.shape)

cat4_5_stats=all_var_stats[all_var_stats.risk>3].groupby(['var_name', 'var_desc', 'var_type', 'year']).sum().reset_index()
cat4_5_stats['%grid cell affected']=100*all_cat_stats['total affected cells'] / total_grid
cat4_5_stats['%population affected']=100*all_cat_stats['total affected population'] / total_pop
cat4_5_yearly_stats=cat4_5_stats.groupby(['var_name', 'var_desc', 
                                      'var_type'])[['total affected population', 'total affected cells', 
                                                    'total incidents occurances', 'total var occurances']].mean()
cat4_5_yearly_stats.columns=['avg. affected population per year', 'avg. affected cells per year', 'avg. incidents occurances per year', 'avg. var occurances per year']
cat4_5_yearly_stats
print(cat4_5_stats.shape, cat4_5_yearly_stats.shape)


# In[21]:


_df=_res[['NETPOP', 'risk_by_count_preds', 'year']].groupby(
    ['risk_by_count_preds', 'year']).agg(['sum', 'count']).reset_index().sort_values(by=['year', 'risk_by_count_preds'])

_df.columns=['risk_by_count_preds',	'year',	'pop_sum', 'cell_count']
popsum=_df.groupby('year').sum()['pop_sum'].to_dict()
popcount= _df.groupby('year').sum()['cell_count'].to_dict()


print(popsum, popcount)
_df['pop_percentage']= _df[['year', 'pop_sum']].apply(lambda x : 100*x[1] / popsum.get(x[0]), axis=1) #
_df['cell_percentage']=_df[['year', 'cell_count']].apply(lambda x : 100*x[1] / popcount.get(x[0]), axis=1)
_df.to_csv(os.path.join(_path, 'grid_year_summary.csv'), index=False)


# In[22]:


## assemble the final spreadsheet files 
# Create a Pandas Excel writer using XlsxWriter as the engine.
writer = pd.ExcelWriter(os.path.join(_path, 'variable_grid_coverage.xlsx'))

yearlysummary=pd.read_csv(os.path.join(_path, "grid_year_summary.csv"))
yearlysummary.to_excel(writer, sheet_name='grid_year_summary')

model_performance=pd.read_csv(os.path.join(_path,"model_performance.csv"))
model_performance.to_excel(writer, sheet_name='Model Performance')

# Write each dataframe to a different worksheet.
all_cat_stats.to_excel(writer, sheet_name='All cells all year')
yearly_cat_stats.to_excel(writer, sheet_name='All cells per year')
cat5_stats.to_excel(writer, sheet_name='Cat5 cells all year')
cat5_yearly_stats.to_excel(writer, sheet_name='Cat5 cells per year')
cat4_5_stats.to_excel(writer, sheet_name='Cat4+5 cells all year')
cat4_5_yearly_stats.to_excel(writer, sheet_name='Cat4+5 cells per year')

feat_importance=pd.read_csv(os.path.join(_path,"feat_importances.csv"))
feat_importance.to_excel(writer, sheet_name='feat_importances')

#predictions_by_average=pd.read_csv(os.path.join(_path, "predictions_basedon_yearly_average_full.csv"))
#predictions_by_average.to_excel(writer, sheet_name='Prediction_by_average')

predictions_all_years=pd.read_csv(os.path.join(_path,"count_predictions_yearly.csv"))
predictions_all_years.to_excel(writer, sheet_name='Prediction_yearly')


# Close the Pandas Excel writer and output the Excel file.
writer.save()


# ## updated output Feb 2022

# In[6]:


import os
import pandas as pd


#_basePath='./ModelA_0.5PLANAR/'
#_basePath='./ModelB_0.5PLANAR/'
_basePath=_path
_features_fn='all_features_grid.csv'
_predictions_fn='count_predictions_yearly.csv'
_predictions_avg_fn='predictions_basedon_yearly_average_full.csv'

#1. get all features and reshape them into netID, vairable. 
fdf=pd.read_csv(os.path.join(_basePath, _features_fn))
print(fdf.shape, fdf.columns)
sum_vars = [i for i in fdf.columns if 'NN' not in i and i not in ['NETID', 'NETPOP', 'COUNT', 'COUNT100', 'year', 'lat', 'long']]

# sum by year and NETID
fdf_sum=fdf[['NETID', 'year']+sum_vars].copy().melt(id_vars=['NETID', 'year']).reset_index(drop=True)
print(fdf_sum.shape, fdf_sum.columns)
print(fdf_sum.head())

# mean by NETID
yearly_mean=fdf.groupby(['NETID']).mean().reset_index()[['NETID']+sum_vars].fillna(0)
ysm=yearly_mean.melt(id_vars='NETID')
ysm['year'] = 'average'


# In[7]:


fdf_sum_mean = pd.concat([fdf_sum, ysm])


# In[8]:


#2. generate yearly sum and average for all variables 
yearly_sum=fdf.groupby(['year']).sum().reset_index()[['year']+sum_vars]

ysd=yearly_sum.set_index('year').unstack().reset_index()
ysd.columns=['variable', 'year', 'N_All']
print(ysd.shape, ysd.columns)
print(ysd.head())

ysd_grand_total = ysd.groupby(['variable']).sum().reset_index()
ysd_grand_total['year'] = 'average'
ysd = pd.concat([ysd, ysd_grand_total])


# In[9]:


_df=pd.merge(fdf_sum_mean, ysd, on=['year', 'variable'], how='outer')


# In[10]:


#3. read and join with the prediction file 
pdf=pd.read_csv(os.path.join(_basePath, _predictions_fn))[['NETID', 'NETPOP', 'year', 'risk_by_count_preds']]
pdf_avg=pd.read_csv(os.path.join(_basePath, _predictions_avg_fn))[['NETID', 'NETPOP', 'risk_by_count_preds']]
pdf_avg['year'] = 'average'
pdf = pd.concat([pdf, pdf_avg])
print(pdf.shape, pdf.columns)


# In[11]:


_df=pd.merge(_df, pdf, on=['NETID', 'year'], how='left')

_df['area']=0.25
_df['Proportion']=_df['value']/_df['N_All']
_df['VarDescr']=_df['variable'].apply(lambda x : get_feature_description(x))#, axis=1 )


# In[12]:


_df.columns=['NETID', 'year', 'VarName', 'N_Grid', 'N_All', 'Pop', 'Category', 'Area', 'Proportion', 'VarDescr']
print(_df.shape, _df.columns) 
print(_df.head())

_df.to_csv(os.path.join(_basePath, 'grid_var_clean.csv'))


# ## 5 create joined shapefile 

# In[13]:


from geopandas import GeoDataFrame
from shapely.geometry import Point
import os 
import geopandas

fn_width='0.5'
yearly_model = True

_path_shp="/Users/kpierce/PredictAlign/PredictAlignFishnets/"
_fn_shp="cumberland_fishnets_urban_areas_with_landscan_NJ_PLANAR.shp"


_path_pred=f"/Users/kpierce/PredictAlignFishnets/Model{model_version}_0.5PLANAR/"
_fn_pred="count_predictions_yearly.csv" # including results for 2017, 2018, 2019
#_fn_pred="predictions_basedon_yearly_average_full.csv" # including results using average data from 2017, 2018, 2019

output_folder=_path_pred+"/shape_average"
if not os.path.exists(output_folder): # Create a new directory because it does not exist 
    os.makedirs(output_folder)
    print("The new directory {} is created!".format(output_folder))

#1. read in the generic shape files. 

_shp=gpd.read_file(os.path.join(_path_shp, _fn_shp))
shp=_shp[_shp['fn_wdth']==fn_width].copy()
print('Generic shape file from ', _path_shp+_fn_shp)
print(shp.shape, shp.columns)
print(shp.head())


#2. read in the predcition file. 
_res=pd.read_csv(os.path.join(_path_pred, _fn_pred))

if yearly_model:
    output_fn=f"Model{model_version}_0.5_a_years.shp"
    _df=_res[['NETID', 'year', 'COUNT_preds', 'risk_by_count_preds']].copy()
    _df.columns=['net_id', 'intk_yr', 'COUNT_preds', 'risk_by_count_preds']
    _df['intk_yr']=_df['intk_yr'].astype(str)
    df  = shp.merge(_df, on=['net_id', 'intk_yr'], how='left')
    print(df.shape, df.columns)
    print(df.head())
else:
    output_fn=f"Model{model_version}_0.5_a_avg.shp"
    _df=_res[['NETID', 'COUNT_preds', 'risk_by_count_preds']].copy()
    _df.columns=['net_id', 'COUNT_preds', 'risk_by_count_preds']
    df  = shp.merge(_df, on=['net_id'], how='left')
    print(df.shape, df.columns)
    print(df.head())
    
print('prediction file from ', _path_pred+_fn_pred)
print(_df.shape, _df.columns)
print(_df.head())

#4 write it out as new shape files 
df.to_file(os.path.join(output_folder, output_fn))
print("new shape files are wrote in: ",  output_folder)


# In[ ]:





# In[ ]:





# ## 2C Spatial Regression Model (under development)

# import libpysal 
# 
# import spreg 
# 
# _vars=sum_f(risk_features)+sum_f(crime_features)+sum_f(violation_features) \
#     + NN_f(violation_features)+NN_f(crime_features)+ NN_f(risk_features)
# #riskonly_f
# 
# features=_vars+['NETPOP']
# 
# #_data=data[data.area.isin(['rural'])].copy()
# _data=data[data.year==2017].copy()
# _data[_vars]=z_score(_data[_vars]).fillna(0)
# #_data['LOGNETPOP']=np.log(100*_data['NETPOP']+0.000001)
# label_col='COUNT'
# 
# #X=_data[features]
# #X_standardized = z_score(X).fillna(0)
# X_standardized = _data[features]
# Y=_data[label_col]
# 
# #x = ['host_listings_count', 'bathrooms', 'bedrooms', 'beds', 'guests_included']
# 
# #yxs = data.loc[:, x + ['pool', 'price']].dropna()
# #y = np.log(yxs['price'].apply(lambda x: float(x.strip('$').replace(',', ''))) + 0.000001)
# 
# points=_data.loc[ :, ['long', 'lat']].values
# kd = libpysal.cg.KDTree(np.array(points))
# wnn5 = libpysal.weights.KNN(kd, 8)
# #w = wnn2.from_array(lst.loc[ yxs.index, ['longitude', 'latitude']].values)
# #w.transform = 'R'
# #wnn5.transform = 'R'
# 
# m1 = spreg.OLS(Y.values[:, None], X_standardized.values, \
#                   w=wnn5, spat_diag=True, \
#                   name_x=[get_feature_description(f) for f in _vars]+['NETPOP'], name_y=label_col)
# print(m1.summary)

# m1.intercept = m1.betas[0]  # Get the intercept from the betas array
# m1.coefficients = m1.betas[1:len(m1.betas)] # Get the coefficients from the betas array
# preds = m1.intercept + X_standardized.values.dot(  m1.coefficients) 
# error=np.abs(preds-Y.values)

# m3 = spreg.GM_Lag(Y.values[:, None], X_standardized.values, \
#                   w=wnn5, spat_diag=True, \
#                   name_x=[get_feature_description(f) for f in _vars]+['NETPOP'], name_y=label_col)
# print(m3.summary)

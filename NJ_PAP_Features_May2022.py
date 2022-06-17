#!/usr/bin/env python
# coding: utf-8

# # June 2022 version 
# 
# + Maltreatment data: raster data filtered for all calls (model A) or only substantiated/established calls (model B)
#     - base directory: `/Users/kpierce/PredictAlignFishnets/`
#     - cumberland_fishnets_urban_areas_and_maltreatment_aggregations_with_landscan_modelA_NJ_PLANAR.csv
#     - cumberland_fishnets_urban_areas_and_maltreatment_aggregations_with_landscan_modelB_NJ_PLANAR.csv
# 
# + Risk and protective data: point data with grid cell ID assigned to either half-mile fishnet grid or landscan grid (see column `fn_width` for grid resolution)
#     - base directory: `/Users/kpierce/PredictAlignFishnets/cleaned_data`
#     - Millville_Vineland_St_Pol_crime_with_netid_nj_planar_esri_102311.csv
#     - St_Pol_crime_with_netid_nj_planar_esri_102311.csv
#     - risk_protect_factors_with_netid_nj_planar_esri_102311.csv
#     - Bridgeton_violations_all_geocoded_with_netid_nj_planar_esri_102311.csv
#     - Millville_violations_all_geocoded_with_netid_nj_planar_esri_102311.csv
#     - Bridgeton_Crime_all_geocoded_with_netid_nj_planar_esri_102311.csv
#     - wic_snap_locations_with_netid_nj_planar_esri_102311.csv
# 
# + Census data: raster data from 5-year American Community Survey for 2017, 2018 and 2019
#     - base directory: `/Users/kpierce/PredictAlignFishnets/`
#     - census_variable_fishnets_half_mi_ESRI_102311.shp
#     - census_variable_fishnets_landscan_ESRI_102311.shp
# 

# In[1]:


import geopandas as gpd
import numpy as np 
import pandas as pd
import os 
#from scipy.spatial import distance
import scipy.spatial.distance

# column analyzer 
def column_analyzer(df) : 
    print("input data frame shape", df.shape)
    _results=[]
    rows=df.shape[0]
    cols=df.shape[1]
    for col in df.columns : 
        empty=df[col].isna().sum()
        #empty=empty+df[col].isnull.sum()
        unique=df[col].nunique()
        _results.append((col, rows, empty, unique))
        #print("{0} has {1} records, {2} empty ({3:.2f}%), {4} uniques ({5:.2f}%)"
        #     .format(col, rows, empty, 100*empty/rows, unique, 100*unique/rows))
    return _results

# compute distance from longitude and latidue 
from math import sin, cos, sqrt, atan2, radians

def WGS84_dist(from_lat, from_lon, to_lat, to_lon) :
    # approximate radius of earth in km
    R = 6373.0
    lat1 = radians(from_lat)
    lon1 = radians(from_lon)
    lat2 = radians(to_lat)
    lon2 = radians(to_lon)

    dlon = lon2 - lon1
    dlat = lat2 - lat1

    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    return R * c


def ll_dist(from_lat, from_lon, to_lat, to_lon, crs='WGS84') :
    if crs=='WGS84' : 
        return WGS84_dist(from_lat, from_lon, to_lat, to_lon)
    elif crs=='PLANAR' : 
        return scipy.spatial.distance.euclidean((from_lat, from_lon), (to_lat, to_lon))

    
# given a lat lon compute nearest distace to each factor in riskDF from list of categories (SIC4) in cats
risk_code_4=[5813, 5812, 5411, 5541, 7215, 6141, 7231, 7241, 5932, 7011, 7542, 7538, 5921]
def compute_NN(lat, lon, riskDF, cats=risk_code_4, 
               cat_col='Primary SIC 4', lat_col='Latitude', lon_col='Longitude', crs='WGS84') :
    dists=[]
    for cat in cats : 
        _d=riskDF[riskDF[cat_col]==cat]
        if _d.empty: #print(_d.shape, _d.columns)
            dists.append((cat, float('inf')))
        else :
            _dists=_d.apply(lambda row : ll_dist(lat, lon, row[lat_col], row[lon_col], crs=crs), axis=1)
            dists.append((cat, np.nanmin(_dists.to_numpy())))#np.nanmin(_dists.to_numpy()))
    return dists
    
# compute distance for each netID to features. 
def get_features_dists(lat, lon, riskDF, cat, k=5,
               cat_col='Primary SIC 4', lat_col='Latitude', lon_col='Longitude', crs='WGS84' ) :
    dists=[]
    _d=riskDF[riskDF[cat_col]==cat]
    if _d.empty: #print(_d.shape, _d.columns)
        dists.append((cat, float('inf')))
    else :
        _dists=_d.apply(lambda row : ll_dist(lat, lon, row[lat_col], row[lon_col], crs=crs), axis=1).to_numpy()
        #dists.append(cat, _dists.to_numpy()[:5]) #np.nanmin(_dists.to_numpy())))#np.nanmin(_dists.to_numpy()))
    _dists=np.sort(_dists)
    return _dists[:k]

# compute distance for each netID center and each yearto features. 
def get_yearly_features_dists(lat, lon, year, riskDF, cat, k=5,
               cat_col='crime_type', lat_col='Latitude', lon_col='Longitude', year_col='year', crs='WGS84' ) :
    dists=[]
    _d=riskDF[(riskDF[cat_col]==cat) & (riskDF[year_col]==year)]
    if _d.empty: #print(_d.shape, _d.columns)
        dists.append((cat, float('inf')))
        return []
    else :
        _dists=_d.apply(lambda row : ll_dist(lat, lon, row[lat_col], row[lon_col], crs=crs), axis=1).to_numpy()
        #dists.append(cat, _dists.to_numpy()[:5]) #np.nanmin(_dists.to_numpy())))#np.nanmin(_dists.to_numpy()))
        _dists=np.sort(_dists)
        return _dists[:k]

def yearly_sum(year, df, cats, cat_col='crime_type', net_col='NETID', year_col='year') :
    #_data=df[df[year_col]==year]
    grouped=df[(df[cat_col].isin(cats)) & (df[year_col]==year)].groupby([net_col, cat_col]).size().reset_index()
    sumDF=grouped.pivot(index=net_col, columns=cat_col, values=0).fillna(0)
    sumDF.columns=[ str(n)+"_SUM" for n in sumDF.columns]
    sumDF.reset_index(level=0, inplace=True)
    sumDF['year']=year
    print(sumDF.shape, sumDF.columns )
    return sumDF

crime_code_type_dict={'2C:12-1': 'ASSAULT',
 '2C:12-3': 'ASSAULT',
 '2C:14-3': 'SEX',
 '2C:15-1': 'ROBBERY',
 '2C:17-3': 'MISCHIEF',
 '2C:18-2': 'BURG',
 '2C:18-3': 'THEFT',
 '2C:29-9': 'CONTEMPT',
 '2C:33-4': 'PROSTITUTION',
 '2C:35-10': 'CDS',
 '2C:35-10.5': 'CDS',
 '2C:35-10.5A(3)': 'CDS',
 '2C:35-10.5E(2)': 'CDS',
 '2C:35-10A(1)': 'CDS',
 '2C:35-10A(3)': 'CDS',
 '2C:35-10A(4)': 'CDS',
 '2C:35-5': 'CDS',
 '2C:35-5A(1)': 'CDS'}

vcat=pd.read_csv("violations_category.csv")
vio_cat_dict=dict(zip(vcat.ori_type, vcat.Category))
#print(vcat.shape, vcat.columns)
#vio_cat_dict

_distance=ll_dist(52.2296756, 21.0122287,  52.406374, 16.9251681)
print("Result:", _distance)
print("Should be:", 278.546, "km")
ll_dist(52.2296756, 21.0122287,  52.406374, 16.9251681, crs='PLANAR')


# # May 2022 Main code block to prepare all features. 

# In[2]:


_path='/Users/kpierce/PredictAlignFishnets/'
#agg_model='ModelA'
agg_model='ModelB'
#fn_width='landscan'
fn_width='0.5'
#count_type='raw_count'
years=[2017, 2018, 2019]
#years=[2019]

_crs='PLANAR'
feature_path=os.path.join(_path, '/Users/kpierce/PredictAlign/PredictAlignFishnets/cleaned_data')
output_path=os.path.join(_path, agg_model+'_'+fn_width+_crs)
os.makedirs(output_path, exist_ok=True) 


# # 1. read Grid information and centroid 

# In[3]:


grid_fn=""
if agg_model=='ModelA' :
    grid_fn='/Users/kpierce/PredictAlign/PredictAlignFishnets/cumberland_fishnets_urban_areas_and_maltreatment_aggregations_with_landscan_modelA_NJ_PLANAR.csv'
elif agg_model=='ModelB' : 
    grid_fn='/Users/kpierce/PredictAlign/PredictAlignFishnets/cumberland_fishnets_urban_areas_and_maltreatment_aggregations_with_landscan_modelB_NJ_PLANAR.csv'

grid=pd.read_csv(os.path.join(_path, grid_fn))
#gridDF=grid[(grid.type==count_type) & (grid.fn_width==fn_width)][
#    ['net_id', 'net_pop', 'count', 'count100', 'planar_lat', 'planar_lon', 'year', 'area']].copy().fillna(0)
gridDF=grid[(grid.fn_width==fn_width) & (grid.area=='include')][
    ['net_id', 'net_pop', 'count', 'count100', 'fishnet_centroid_lat', 'fishnet_centroid_lon', 'year']].copy().fillna(0)
gridDF.columns=['NETID', 'NETPOP', 'COUNT', 'COUNT100', 'lat', 'long', 'year']
print("Data file {} with shape: {} and columns {}".format(grid_fn, gridDF.shape, gridDF.columns))
#gridDF.head()


# In[4]:


grid.groupby(['fn_width', 'area'])['net_id'].count()


# In[5]:


gridDF.head()


# # 2. read protect and risk factors

# In[6]:


_rp_fn='risk_protect_factors_with_netid_nj_planar_esri_102311.csv'
_rp_out="risk_protect_grid."+fn_width+".csv"

_data=pd.read_csv(os.path.join(feature_path, _rp_fn))
data=_data[_data.fn_width==fn_width].copy().fillna(0)
data['NETID']=data['net_id']


# In[7]:


data['include_area'].value_counts()


# # 2.1 create SUMDF per netID.

# In[8]:


grouped=data.groupby(['NETID', 'Primary.SIC.4']).size().reset_index()
sumDF=grouped.pivot(index='NETID', columns='Primary.SIC.4', values=0).fillna(0)
sumDF.columns=[ str(n)+"_SUM" for n in sumDF.columns]
sumDF.reset_index(level=0, inplace=True)
print(sumDF.shape, sumDF.columns )


# In[9]:


grouped.head()


# In[10]:


#2.2 get feature distances
cat_col='Primary.SIC.4'
cats=data[cat_col].unique()
gf=gridDF.copy()
#gf['lat']=gf['lat'].astype(float)
#gf['long']=gf['long'].astype(float)
for cat in cats :
    print(cat)
    _name=str(cat)+"_dists"
    gf[_name]=gf[['lat', 'long']].apply(lambda row : get_features_dists(
        row[0], row[1], data, cat, 5, cat_col, 'data_lat', 'data_lon', crs=_crs), axis=1)
gf.head() 


# In[11]:


gf.shape


# In[12]:


#2.3 create dist feautres. 
for cat in cats : 
    _ori=str(cat)+"_dists"
    _name1=str(cat)+"_NN"
    _name2=str(cat)+"_5NN"
    gf[_name1]=gf[_ori].apply(lambda x : np.min(x))
    gf[_name2]=gf[_ori].apply(lambda x : np.average(x))

features=['NETID', 'NETPOP', 'COUNT', 'COUNT100', 'lat', 'long', 'year'] + [str(n)+"_NN" for n in cats] + [str(n)+"_5NN" for n in cats]
final=pd.merge(gf[features], sumDF, how='left', on='NETID').fillna(0)
print(os.path.join(output_path, _rp_out), final.shape, final.columns)
final.to_csv(os.path.join(output_path, _rp_out))
 


# In[13]:


#3. read and combine all crime data.
_crime_fn='Bridgeton_Crime_all_geocoded_with_netid_nj_planar_esri_102311.csv'
_crime_out="crime_grid."+fn_width+".csv"

_df=pd.read_csv(os.path.join(feature_path, _crime_fn))

bdata=_df[(_df.fn_width==fn_width) & (_df.include_area=='include')][['net_id', 'data_lon', 'data_lat', 'crime_type', 'Agency.Incident...Actual.CFS.Type', 'year']].copy()
bdata.columns=['NETID', 'lon', 'lat', 'crime_type', 'ori_type', 'year']

_fn='St_Pol_crime_with_netid_nj_planar_esri_102311.csv'
_df=pd.read_csv(os.path.join(feature_path, _fn))
sdata=_df[(_df.fn_width==fn_width) & (_df.include_area=='include')][['net_id', 'data_lon', 'data_lat', 'Crime.Code', 'year']].copy()
sdata['crime_type']=sdata['Crime.Code'].apply(lambda x : crime_code_type_dict.get(x))
sdata.columns=['NETID', 'lon', 'lat', 'ori_type', 'year', 'crime_type']


# In[14]:


data=pd.concat([bdata, sdata])
print(bdata.shape, bdata.columns)
print(sdata.shape, sdata.columns)
print(data.shape, data.columns)


# In[15]:


data.head()


# In[16]:


#3.1 get categories 
cat_col='crime_type'
cat_year=[cat_col, 'year']
sizes=data.groupby(cat_year).size().reset_index()
#select crime at least 5 times a year all 3 years
_cats=sizes[sizes[0]>4][cat_year].groupby('crime_type').count().reset_index()
cats=_cats[_cats.year>2][cat_col]

# revision june 2022: include all crime regardless of infrequency
#cats = data['crime_type'].unique()


# In[17]:


cats


# In[18]:


_cats


# In[19]:


sizes


# In[20]:


#3.2 create sumDF
_dfs=[]
for y in years : 
    _dfs.append(yearly_sum(y, data, cats))
    
sumDF=pd.concat(_dfs)    
print("sumDF for crime", sumDF.shape, sumDF.columns )

#3.3 get feature distances: 
gf=gridDF.copy()
#gf['lat']=gf['lat'].astype(float)
#gf['long']=gf['long'].astype(float)

for cat in cats :
    print(cat)
    _name=str(cat)+"_dists"
    gf[_name]=gf[['lat', 'long', 'year']].apply(lambda row : get_yearly_features_dists(
        row[0], row[1], row[2], data, cat, 5, cat_col, 'lat', 'lon', crs=_crs), axis=1)

print("crime_feature distances: ", gf.shape, gf.columns)

#3.4 create crime distance features: 
for cat in cats : 
    _ori=str(cat)+"_dists"
    _name1=str(cat)+"_NN"
    _name2=str(cat)+"_5NN"
    gf[_name1]=gf[_ori].apply(lambda x : np.min(x))
    gf[_name2]=gf[_ori].apply(lambda x : np.average(x))
print("crime with distance features:", gf.shape, gf.columns)


# In[21]:


gf.head()


# In[22]:


#3.5 save all crime features
features=['NETID', 'NETPOP', 'COUNT', 'COUNT100', 'lat', 'long', 'year'] + [str(n)+"_NN" for n in cats] + [str(n)+"_5NN" for n in cats]
final=pd.merge(gf[features], sumDF, how='left', on=['NETID', 'year']).fillna(0)
print(os.path.join(output_path, _crime_out), final.shape, final.columns)
final.to_csv(os.path.join(output_path, _crime_out))

#4. create violation features. 
#_fn='Bridgeton_violations_all_geocoded_with_netid_wgs84.csv'
_vio_fn='Bridgeton_violations_all_geocoded_with_netid_nj_planar_esri_102311.csv'
_vio_out='violation_grid.'+fn_width+'.csv'

#bdata=pd.read_csv(os.path.join(_path, _fn))[['net_id', 'data_lon', 'data_lat', 'violation_type', 'Statute', 'year', 'urban_area']]
_df=pd.read_csv(os.path.join(feature_path, _vio_fn))
bdata=_df[_df.fn_width==fn_width][['net_id', 'data_lon', 'data_lat', 'violation_type', 'Statute', 'year']].copy()
bdata.columns=['NETID', 'lon', 'lat', 'violation_type', 'ori_type', 'year']
bdata['violation_type']=bdata['ori_type'].apply(lambda x : vio_cat_dict.get(x.strip()))

#_fn='Millville_violations_all_geocoded_with_netid_wgs84.csv'
_fn='Millville_violations_all_geocoded_with_netid_nj_planar_esri_102311.csv'
_df=pd.read_csv(os.path.join(feature_path, _fn))
#sdata=pd.read_csv(os.path.join(_path, _fn))[['net_id', 'data_lon', 'data_lat', 'violation_type', 'Ordinance.Id.1', 'report_year', 'urban_area']]
sdata=_df[_df.fn_width==fn_width][['net_id', 'data_lon', 'data_lat', 'violation_type', 'Ordinance.Id.1', 'year']].copy()
sdata.columns=['NETID', 'lon', 'lat', 'violation_type', 'ori_type', 'year']
sdata['violation_type']=sdata['ori_type'].apply(lambda x : vio_cat_dict.get(x.strip()))


print("Violation Bridgeton: ", bdata.shape, bdata.columns)
print("Violation Millville: ", sdata.shape, sdata.columns)

data=pd.concat([bdata, sdata])
print('violation feature table: ', data.shape, data.columns)

#4.1 get categories 
cat_col='violation_type'
cat_year=[cat_col, 'year']
sizes=data.groupby(cat_year).size().reset_index()
#select violations at least 5 times a year 
_cats=sizes[sizes[0]>4][cat_year].groupby(cat_col).count().reset_index()
cats=_cats[_cats.year>2][cat_col]
print(cats)

#4.2 get sumDf
_dfs=[]
for y in years : 
    _dfs.append(yearly_sum(y, data, cats, cat_col=cat_col))
    
sumDF=pd.concat(_dfs)    
print(sumDF.shape, sumDF.columns )

#4.3 get feature distances 
gf=gridDF.copy()
#gf['lat']=gf['lat'].astype(float)
#gf['long']=gf['long'].astype(float)
for cat in cats :
    print(cat)
    _name=str(cat)+"_dists"
    gf[_name]=gf[['lat', 'long', 'year']].apply(lambda row : get_yearly_features_dists(
        row[0], row[1], row[2], data, cat, 5, cat_col, 'lat', 'lon', crs=_crs), axis=1)

print("comupte violation distances", gf.shape, gf.columns)

#4.4 get distance features. 
for cat in cats : 
    _ori=str(cat)+"_dists"
    _name1=str(cat)+"_NN"
    _name2=str(cat)+"_5NN"
    gf[_name1]=gf[_ori].apply(lambda x : np.min(x))
    gf[_name2]=gf[_ori].apply(lambda x : np.average(x))
print('compute violation distance features', gf.shape, gf.columns)

features=['NETID', 'NETPOP', 'COUNT', 'COUNT100', 'lat', 'long', 'year'] + [str(n)+"_NN" for n in cats] + [str(n)+"_5NN" for n in cats]
final=pd.merge(gf[features], sumDF, how='left', on=['NETID', 'year']).fillna(0)
print("violation features ", os.path.join(output_path, _vio_out), final.shape, final.columns)
final.to_csv(os.path.join(output_path, _vio_out), index=False)

#5 combine all features 
df1=pd.read_csv(os.path.join(output_path, _rp_out))
df2=pd.read_csv(os.path.join(output_path, _crime_out))
df3=pd.read_csv(os.path.join(output_path, _vio_out))
#df2['year']=df2['year_x']

cols_to_use =  df2.columns.difference(df1.columns).to_list()
#cols_to_use.append(['NETID', 'year', 'area'])
cols_to_use = cols_to_use + ['NETID', 'year']
finalDF = pd.merge(df1, df2[cols_to_use], on=['NETID', 'year'], how='outer')

cols_to_use =  df3.columns.difference(finalDF.columns).to_list()
#cols_to_use.append(['NETID', 'year', 'area'])
cols_to_use = cols_to_use + ['NETID', 'year']
finalDF = pd.merge(finalDF, df3[cols_to_use], on=['NETID', 'year'], how='outer')

# census data
if fn_width=='0.5':
    census_fp = '/Users/kpierce/PredictAlign/PredictAlignFishnets/census_variable_fishnets_half_mi_ESRI_102311.csv'
elif fn_width=='landscan':
    census_fp = '/Users/kpierce/PredictAlign/PredictAlignFishnets/census_variable_fishnets_landscan_ESRI_102311.csv'

census_data = pd.read_csv(census_fp).rename(columns={'net_id': 'NETID'})


finalDF_ = pd.merge(finalDF, census_data, on=['NETID', 'year'], how='outer')

finalDF_.drop(columns=['Unnamed: 0_x', 'Unnamed: 0_y']).to_csv(os.path.join(output_path, "all_features_grid.csv"), index= False)
print("combined features: ", df1.shape, df2.shape, df3.shape, finalDF_.shape)
print(os.path.join(output_path, "all_features_grid.csv"), finalDF_.columns)
print("ALL DONE!")


# In[23]:


print([i for i in finalDF_.columns])


# In[ ]:





#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 19 11:32:11 2020

@author: saptarshimaiti
"""

import pandas as pd

import gc

import re


#pip install requests

import requests

#pip install reverse_geocoder
import reverse_geocoder as rg  



#pip install pandas_profiling

#import pandas_profiling as pp



pd.set_option('display.max_columns', 500)

#---------------------------Need to change the api_key------------------------#
api_key = "GOOGLE_API_LEY"
#---------------------------Need to change the api_key------------------------#

class CountyCityMissingValue:
    
    def __init__(self, dataFrame, api_key):
        self.data = dataFrame
        self.google_api_key = api_key
        
    def extract_lat_long_via_address(self, address_or_zipcode):
        lat, lng = None, None
        api_key = self.google_api_key
        base_url = "https://maps.googleapis.com/maps/api/geocode/json"
        endpoint = f"{base_url}?address={address_or_zipcode}&key={api_key}"
        # see how our endpoint includes our API key? Yes this is yet another reason to restrict the key
        r = requests.get(endpoint)
        if r.status_code not in range(200, 299):
            return None, None
        try:
            '''
            This try block incase any of our inputs are invalid. This is done instead
            of actually writing out handlers for all kinds of responses.
            '''
            results = r.json()['results'][0]
            lat = results['geometry']['location']['lat']
            lng = results['geometry']['location']['lng']
        except:
            pass
        return 'POINT (' + str(lng) + ' ' + str(lat) + ')'
        


        
df = pd.read_csv('/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/Iowa_Liquor_Sales.csv', low_memory = False)
#***** Number of data points = 17926603 *****#

#report = pp.ProfileReport(df)

#report.to_file = ("Report.html")

df['County'] = df['County'].str.upper()
df['City'] = df['City'].str.upper()
df['Address'] = df['Address'].str.upper()

#** Total Missing County = 156605 and Total Missing City = 79802 **#

#** 79802 data points are missing all address related values **#

#---Dropping 79802 data points that are missing all address related values--#

#** Number of data points = 17846801 after dropping 79802 data points **#
#** Total Missing County = 76803 **# 



df_null_values = df[df['County'].isnull() & df['Store Location'].isnull() & df['Address'].isnull() & df['City'].isnull() & df['Zip Code'].isnull() & df['County Number'].isnull()][['Store Number', 'Store Name', 'Address', 'City', 'County', 'Store Location']].drop_duplicates()

store_nums = df_null_values['Store Number'].unique()



for store_num in store_nums:
    store =  df[(df['Store Number'] == store_num) & df['County'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()].groupby(['Store Number'])['Date'].max()
    try:
        df.loc[(df['Store Number'] == store_num) & df['County'].isnull() & df['Address'].isnull() & df['City'].isnull() & df['Zip Code'].isnull() & df['County Number'].isnull(), 'County'] = df[(df['Store Number'] == store_num) & (df['Date'] == store[store_num]) & df['County'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()]['County'].unique()[0]
        df.loc[(df['Store Number'] == store_num) & df['Address'].isnull() & df['City'].isnull() & df['Zip Code'].isnull() & df['County Number'].isnull(), 'City'] = df[(df['Store Number'] == store_num) & (df['Date'] == store[store_num]) & df['County'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()]['City'].unique()[0]
        df.loc[(df['Store Number'] == store_num) & df['Address'].isnull() & df['Zip Code'].isnull() & df['County Number'].isnull(), 'Zip Code'] = df[(df['Store Number'] == store_num) & (df['Date'] == store[store_num]) & df['County'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()]['Zip Code'].unique()[0]
        df.loc[(df['Store Number'] == store_num) & df['Address'].isnull() & df['County Number'].isnull(), 'Address'] = df[(df['Store Number'] == store_num) & (df['Date'] == store[store_num]) & df['County'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()]['Address'].unique()[0]
        df.loc[(df['Store Number'] == store_num) & df['County Number'].isnull(), 'County Number'] = df[(df['Store Number'] == store_num) & (df['Date'] == store[store_num]) & df['County'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()]['County Number'].unique()[0]
    except:
        pass

'''
******* RESOLVED MISSING CITIES *******
'''
df = df.drop(list(df[df['County'].isnull() & df['Store Location'].isnull() & df['Address'].isnull() & df['City'].isnull() & df['Zip Code'].isnull() & df['County Number'].isnull()].index))
countyCityImputation = CountyCityMissingValue(df, api_key)

df.loc[df['Store Number'] == 5142, 'Address'] = '2315 Mt Vernon Rd Se'



'''
*   FIXING 70447 MISSING COUNTIES HAVING NOT NULL STORE LOCATION 
'''


longitudes_latitudes = df[(df['County'].isnull()) & df['Store Location'].notnull()]['Store Location'].unique()



for longitude_latitude in longitudes_latitudes:
    longitude, latitude = re.sub(r' ',",",re.sub(r'\)',"",re.sub(r'POINT \(',"", longitude_latitude))).split(',')
    df.loc[df['Store Location'] == longitude_latitude, 'County'] = re.sub(r' County',"",rg.search((latitude, longitude))[0]['admin2'])



df['Address'] = df['Address'].str.replace("#","")
  
df['Store Name'] = df['Store Name'].str.replace("#", "")  
'''
******* RESOLVED 70447 MISSING CITIES *******
'''

#** Total Missing County = 6356 **# 


df_locations = df[(df['County'].isnull()) & df['Address'].notnull()][['Store Name','Store Number','Address','City']].drop_duplicates()


'''
******* RESOLVED 6356 MISSING CITIES *******
'''
#countyCityImputation.extract_lat_long_via_address("STATION MART LIQUOR AND TOBACCO, 3594 LAFAYETTE ST, EVANSDALE, IOWA")


for index, row in df_locations.iterrows():
    store_location = countyCityImputation.extract_lat_long_via_address(row['Store Name'] + "," + row['Address'] + "," + row['City'] + ", IOWA")
    df.loc[df['County'].isnull() & (df['Store Number'] == row['Store Number']), "Store Location"] = store_location
    longitude, latitude = re.sub(r' ',",",re.sub(r'\)',"",re.sub(r'POINT \(',"", store_location))).split(',')
    df.loc[df['County'].isnull() & (df['Store Number'] == row['Store Number']) & (df['Address'] == row['Address']) & (df['City'] == row['City']), "County"] = re.sub(r' County',"",rg.search((latitude, longitude))[0]['admin2'])


#** Total Missing Store Location = 1645567 **#     


    
df_locations = df[(df['Store Location'].isnull()) & df['Address'].notnull()][['Store Name','Store Number','Address','City']].drop_duplicates()

'''
******* RESOLVED 1645567 MISSING STORE LOCATION *******
'''

df.loc[df['Store Number'] == 5142, 'Store Name'] = "Smokin' Joe's 13 Tobacco and Liquor Outlet"

for index, row in df_locations.iterrows():
    store_location = countyCityImputation.extract_lat_long_via_address(row['Store Name'] + "," + row['Address'] + "," + row['City'] + ", IOWA")
    df.loc[df['Store Location'].isnull() & (df['Store Number'] == row['Store Number']) & (df['Address'] == row['Address']) & (df['City'] == row['City']), "Store Location"] = store_location
    
'''
****** STORE LOCATION IS WRONG IN THE MAIN DATASET, SO CORRECTED AND FIXED 
5284  POINT (-73.9881152 40.7024718)  122, FRONT ST. BROOKLYN 
********
'''
df_locations = df[(df['County'] == "") & df['Store Location'].notnull()][['Store Name', 'Store Number', 'Store Location', 'Address', 'City']].drop_duplicates()

for index, row in df_locations.iterrows():
    store_location = countyCityImputation.extract_lat_long_via_address(row['Store Name'] + "," +row['Address'] + ", " + row['City'] + ", IOWA")
    df.loc[(df['County'] == "") & df['Store Location'].notnull() & (df['Store Number'] == row['Store Number']) & (df['Address'] == row['Address']) & (df['City'] == row['City']), "Store Location"] = store_location
    longitude, latitude = re.sub(r' ',",",re.sub(r'\)',"",re.sub(r'POINT \(',"", store_location))).split(',')
    df.loc[(df['County'] == "") & (df['Store Number'] == row['Store Number']) & (df['Address'] == row['Address']) & (df['City'] == row['City']), "County"] = re.sub(r' County',"",rg.search((latitude, longitude))[0]['admin2'])


df['County'] = df['County'].str.upper()

df_county_number = df[df['County Number'].notnull()][['County','County Number']].drop_duplicates()

for index, row in df_county_number.iterrows():
    df.loc[(df['County'] == row['County']) & df['County Number'].isnull(), 'County Number'] = row['County Number']

'''
******  MISSING COUNTY NUMBER LOGIC OF EL PASO *****
'''

df.loc[df['County'] == "EL PASO", "County Number"] = df["County Number"].max() + 1

#** Standardised Cases of Address, city and county **#

df['County'] = df['County'].str.title()
df['Address'] = df['Address'].str.title()
df['City'] = df['City'].str.title()

longitudes_latitudes = df['Store Location'].unique()

#store_dt_series = df[df['County'].notnull() & df['Store Location'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()].groupby(['Store Number'])['Date'].max()


#df[df['Store Number'] == 5140 & df['County'].notnull() & df['Store Location'].notnull() & df['Address'].notnull() & df['City'].notnull() & df['Zip Code'].notnull() & df['County Number'].notnull()]

cooridnates = None

for longitude_latitude in longitudes_latitudes:
    lng_lat = re.match(r'POINT \(\-?\d+\.\d+\s*\-?\d+\.\d+\)',longitude_latitude)
    if lng_lat is None:
        cooridnates = longitude_latitude
        
'''
**** FIXING INVALID CO-ORDINATES ****
'''

df_locations = df[df['Store Location'] == cooridnates][['Store Name', 'Address','City','Store Location']].drop_duplicates()

#JW Liquor, 4518 Mortonsen Street Suite 109, Ames



for index, row in df_locations.iterrows():
    store_location = countyCityImputation.extract_lat_long_via_address(row['Store Name'] + "," + row['Address'] + ", " + row['City'] + ", IOWA")
    if store_location != 'POINT (None None)':
        df_locations.loc[(df["Address"] == row['Address']) & (df['City'] == row['City']), "Store Location"] = store_location
    else:
        store_location = countyCityImputation.extract_lat_long_via_address(row['Address'] + ", " + row['City'] + ", IOWA")
        df_locations.loc[(df["Address"] == row['Address']) & (df['City'] == row['City']), "Store Location"] = store_location
    
    
for index, row in df_locations.iterrows():
    df.loc[(df['Store Name'] == row['Store Name'] ) & (df["Address"] == row['Address']) & (df['City'] == row['City']), "Store Location"] = row['Store Location']



## Testing ##

longitudes_latitudes = df['Store Location'].unique()

cooridnates = None

for longitude_latitude in longitudes_latitudes:
    lng_lat = re.match(r'POINT \(\-?\d+\.\d+\s*\-?\d+\.\d+\)',longitude_latitude)
    if lng_lat is None:
        cooridnates = longitude_latitude
        
print(cooridnates) #Expected None


df.to_csv(r'/Users/saptarshimaiti/Desktop/Data Preparation And Analysis/Project/Iowa_Liquor_Sales_Cleaned_v4.csv', index = False, header=True)


#df[df['Store Number'] == 5142][['Store Name', 'Store Number', 'Address', 'City', 'County', 'Store Location']].drop_duplicates()


gc.collect()
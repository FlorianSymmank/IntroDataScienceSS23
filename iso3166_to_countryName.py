"""
Simple conversion form iso3166 to country name
"""

import pandas as pd
import pycountry

def iso_to_country_name(iso_code):
    try:
        country = pycountry.countries.get(alpha_2=iso_code)
        return country.name
    except KeyError:
        return None
    
df = pd.read_csv(r'data\o_salaries.csv')
df['employee_residence_country_name'] = df['employee_residence'].apply(iso_to_country_name)
df['company_location_country_name'] = df['company_location'].apply(iso_to_country_name)

df.to_csv(r'data\salaries.csv', index=False)
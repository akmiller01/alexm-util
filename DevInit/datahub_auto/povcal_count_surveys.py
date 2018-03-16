import pandas as pd
import pdb

smy = pd.read_csv("C:/Users/Alex/Documents/Data/pcn_auto2/smy_time.csv")

smy_filtered = smy[(smy["displayMode"].isin([0,2,4])) & (smy["CoverageType"].isin([3,5]))]

smy_surveys = pd.concat([smy["CountryCode"],smy["DataYear"]],axis=1)
unique_surveys = smy_surveys.drop_duplicates().dropna()

latest_surveys = unique_surveys[(unique_surveys["DataYear"]>=1993) & (unique_surveys["DataYear"]<=2013)]
survey_counts = latest_surveys["CountryCode"].value_counts()

latest_surveys.to_csv("C:/Users/Alex/Documents/Data/pcn_auto2/latest_surveys.csv",index=False)
survey_counts.to_csv("C:/Users/Alex/Documents/Data/pcn_auto2/survey_counts.csv")
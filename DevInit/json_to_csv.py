import requests
import json
import pandas as pd

url = "https://dacmzozdwasla.cloudfront.net//1.0/poverty/country/all/2030-12-31/86400/?format=json"

raw_data = requests.get(url).content
data = json.loads(raw_data)

df = pd.DataFrame(data)
df.to_csv("C:/Users/Alex/Documents/Data/P20/Meta/pov_clock_12_31_2030.csv",index=False,encoding="utf-8")

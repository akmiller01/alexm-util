import requests
import json
import time
import pandas as pd
import re
import pdb

def imfDB():
    user_agent = "di-imf-pysdmx/0.0.1"
    data_flow_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/Dataflow"
    headers = {
        'User-Agent':user_agent
    }
    
    content = requests.get(data_flow_url,headers=headers).content
    time.sleep(1)
    
    rawJson = json.loads(content)
    structure = rawJson["Structure"]
    dataflows = structure["Dataflows"]["Dataflow"]
    output = []
    for dataflow in dataflows:
        db_key = dataflow["KeyFamilyRef"]["KeyFamilyID"]
        db_name = dataflow["Name"]["#text"]
        obj = {"db_key":db_key,"db_name":db_name}
        output.append(obj)
    return(output)
        
def imfDS(db_key):    
    user_agent = "di-imf-pysdmx/0.0.1"
    headers = {
        'User-Agent':user_agent
    }
    ds_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/DataStructure/{}".format(db_key)
    content = requests.get(ds_url,headers=headers).content
    cleanr = re.compile('<.*?>')
    cleaned_content = re.sub(cleanr,"",content)
    time.sleep(1)
    ds = json.loads(content)["Structure"]
    return(ds)

def imfCL(db_key):
    ds = imfDS(db_key)
    codelists = ds["CodeLists"]["CodeList"]
    output = []
    for codelist in codelists:
        dim_name = codelist["Name"]["#text"]
        dim_code = codelist["@id"]
        for code in codelist["Code"]:
            code_val = code["@value"]
            code_desc = code["Description"]["#text"]
            obj = {"dim_name":dim_name,"dim_code":dim_code,"code_val":code_val,"code_desc":code_desc}
            output.append(obj)
    return(output)

def imfCD(db_key,params="",startPeriod=None,endPeriod=None):
    user_agent = "di-imf-pysdmx/0.0.1"
    headers = {
        'User-Agent':user_agent
    }
    
    cd_url = "http://dataservices.imf.org/REST/SDMX_JSON.svc/CompactData/{}/{}".format(db_key,params)
    
    if startPeriod is not None:
        if endPeriod is not None:
            cd_url += "?startPeriod={}&endPeriod={}".format(startPeriod,endPeriod)
        else:
            cd_url += "?startPeriod={}".format(startPeriod)
    else:
        if endPeriod is not None:
            cd_url += "?endPeriod={}".format(endPeriod)
        
    cdContent = requests.get(cd_url,headers=headers).content
    time.sleep(1)
    cd = json.loads(cdContent)
    return(cd)

def imfDF(db_key,params="",startPeriod=None,endPeriod=None):
    output = []
    
    cd = imfCD(db_key,params,startPeriod,endPeriod)
    series = cd["CompactData"]["DataSet"]["Series"]
    if type(series) is dict:
        data = {"db_key":db_key}
        for key in series:
            if key != "Obs":
                data[key] = series[key]
            else:
                obs = series[key]
                single_ob = False
                for ob in obs:
                    if type(ob) is dict:
                        multivar = "value_{}".format(ob["@TIME_PERIOD"])
                        if "@OBS_VALUE" in ob.keys():
                            data[multivar] = ob["@OBS_VALUE"]
                        else:
                            data[multivar] = None
                    else:
                        single_ob = True
                if single_ob:
                    multivar = "value_{}".format(obs["@TIME_PERIOD"])
                    if "@OBS_VALUE" in obs.keys():
                        data[multivar] = obs["@OBS_VALUE"]
                    else:
                        data[multivar] = None
        output.append(data)
    else:
        for element in series:
            data = {"db_key":db_key}
            for key in element:
                if key!= "Obs":
                    data[key] = element[key]
                else:
                    obs = element[key]
                    single_ob = False
                    for ob in obs:
                        if type(ob) is dict:
                            multivar = "value_{}".format(ob["@TIME_PERIOD"])
                            if "@OBS_VALUE" in ob.keys():
                                data[multivar] = ob["@OBS_VALUE"]
                            else:
                                data[multivar] = None
                        else:
                            single_ob = True
                    if single_ob:
                        multivar = "value_{}".format(obs["@TIME_PERIOD"])
                        if "@OBS_VALUE" in obs.keys():
                            data[multivar] = obs["@OBS_VALUE"]
                        else:
                            data[multivar] = None
                        
            output.append(data)
    full_df = pd.DataFrame(output)
    return(full_df)
                        
    
    
# dbs = imfDB()
# ds = imfDS("MCDREO")
codelist = imfCL("GFSR")
pd.DataFrame(codelist).to_csv("GFSR_codelist.csv",index=False,encoding="utf8")
# cd = imfCD("GFSR","A..S1311B.XDC.W0_S1_G1","2014","2016")
df = imfDF("GFSR","A..S1311B.XDC.W0_S1_G1","2014","2016")
df.to_csv("GFSR.csv",index=False)

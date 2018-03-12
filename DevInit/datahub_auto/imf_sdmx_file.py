import io
import zipfile
from contextlib import closing
import requests
from os.path import splitext
import pdb
import xmltodict
import pandas as pd
from progressbar import ProgressBar
from collections import OrderedDict
pbar = ProgressBar()

def parseSDMX(xml):
    output = []
    doc = xmltodict.parse(xml)
    series = doc[u'message:StructureSpecificData']["message:DataSet"]["Series"]
    if type(series) is dict:
        data = {}
        for key in pbar(series.keys()):
            if key != "Obs":
                data[key] = series[key]
            else:
                obs = series[key]
                single_ob = False
                for ob in obs:
                    if type(ob) is dict or type(ob) is OrderedDict:
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
        for element in pbar(series):
            data = {}
            for key in element:
                if key!= "Obs":
                    data[key] = element[key]
                else:
                    obs = element[key]
                    single_ob = False
                    for ob in obs:
                        if type(ob) is dict or type(ob) is OrderedDict:
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
    pdb.set_trace()
    return(full_df)

def parseSDMXd(xsd):
    pdb.set_trace()
    
if __name__=="__main__":

    r = requests.get("https://www.imf.org/external/pubs/ft/weo/2017/02/weodata/WEOOct2017_SDMXData.zip")
    with closing(r), zipfile.ZipFile(io.BytesIO(r.content)) as archive:
        for member in archive.infolist():
            filename, file_extension = splitext(member.filename)
            if file_extension==".xml":
                parseSDMX(archive.read(member))
            elif file_extension==".xsd":
                parseSDMXd(archive.read(member))
            

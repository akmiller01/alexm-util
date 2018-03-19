import requests
import xmltodict
import pandas as pd
from collections import OrderedDict
import pdb

dataUrl = "http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/TABLE2B/10100+10010+71+86+64+62+30+66+35+57+45+93+65+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+225+236+227+287+228+230+229+231+232+233+234+247+235+274+237+245+271+238+239+240+241+243+244+248+249+251+252+253+255+256+257+258+259+275+260+261+266+276+268+269+270+272+273+218+279+278+280+282+283+285+288+265+289+298+10004+10005+376+377+373+328+329+352+331+388+386+336+338+378+340+342+381+347+349+351+354+358+385+361+364+366+382+383+384+375+387+380+389+10006+425+428+431+434+437+440+446+451+454+457+460+463+489+498+10007+10008+725+728+730+740+735+738+742+745+748+751+752+753+755+761+732+764+765+769+789+10009+625+610+611+666+630+612+645+650+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+831+832+840+836+859+860+845+850+856+858+861+862+880+866+868+870+872+854+876+889+9998+9998.20001+801+1+2+301+68+3+18+4+5+40+6+701+742+22+7+820+8+9+50+10+11+12+302.1.201+204+205+292+293+295+296+217+297.A+D/all?startTime=2007&endTime=2016"

structureUrl = "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE2B"

dataContent = requests.get(dataUrl).content
dataDict = xmltodict.parse(dataContent)

ds = dataDict["message:MessageGroup"]["DataSet"]
series = ds["Series"]
outputList = []
for element in series:
    outputDictCommon = {}
    metas = element["SeriesKey"]["Value"]
    for meta in metas:
        metaKey = meta["@concept"]
        metaValue = meta["@value"]
        outputDictCommon[metaKey] = metaValue
    attributes = element["Attributes"]["Value"]
    for attribute in attributes:
        attributeKey = attribute["@concept"]
        attributeValue = attribute["@value"]
        outputDictCommon[attributeKey] = attributeValue
    obs = element["Obs"]
    if type(obs) is OrderedDict:
        outputDict = outputDictCommon.copy()
        outputDict["Time"] = obs["Time"]
        outputDict["Value"] = obs["ObsValue"]["@value"]
        outputList.append(outputDict)
    else:
        for ob in obs:
            outputDict = outputDictCommon.copy()
            outputDict["Time"] = ob["Time"]
            outputDict["Value"] = ob["ObsValue"]["@value"]
            outputList.append(outputDict)
        
df = pd.DataFrame(outputList)
df.to_csv("table2b.csv",index=False)
    

# https://www.reddit.com/r/Python/comments/690j1q/faster_loading_of_dataframes_from_pandas_to/
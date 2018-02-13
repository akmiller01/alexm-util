import requests
import pandas as pd
import xml.etree.ElementTree as ET
import pdb

class XML2DataFrame:

    def __init__(self, xml_data):
        self.root = ET.fromstring(xml_data)

    def parse_root(self, root):
        return [self.parse_element(child) for child in iter(root)]

    def parse_element(self, element, parsed=None):
        if parsed is None:
            parsed = dict()
        for key in element.keys():
            parsed[key] = element.attrib.get(key)
        if element.text:
            parsed[element.tag] = element.text
        for child in list(element):
            self.parse_element(child, parsed)
        return parsed

    def process_data(self):
        structure_data = self.parse_root(self.root)
        return pd.DataFrame(structure_data)

base_url = "http://www.glidenumber.net/glide/xmlglideset.jsp?"
params = {
    "sortby" : "0"
    ,"fromyear" : "1998"
    ,"toyear" : "2017"
    ,"tomonth" : "12"
    ,"today" : "31"
    ,"maxhits" : "100"
}
param_string = ""
for key in params.keys():
    value = params[key]
    if param_string!="":
        param_string += "&"
    param_string += "{}={}".format(key,value)
    
url = base_url+param_string
url_valid = True
nStart = 0
dataframes = []
while url_valid:
    print nStart
    try:
        paginated_url = url + "&nStart={}".format(nStart)
        response = requests.get(paginated_url)
        xml_data = response.content
        xml2df = XML2DataFrame(xml_data)
        xml_dataframe = xml2df.process_data()
        dataframes.append(xml_dataframe)
        nStart += int(params["maxhits"])
    except requests.exceptions.RequestException as e:
        print e
        url_valid = False
        
try:
    dataset = pd.concat(dataframes)
    dataset.to_csv("C:/Users/Alex/Documents/Data/glide_1998_2017.csv")
except ValueError as e: 
    print e
    

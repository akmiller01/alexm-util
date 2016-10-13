from selenium import webdriver
import time
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
import pandas as pd

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\PovCal_Increment\\regional\\",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()


def input_text(browser, inputs):
    # Fills a list of text boxes
    #
    # inputs: [{"input_id": "someId", "input_str": "some string"}, ... ]

    # This will cause Selenium to wait until the element is found.
    browser.find_element_by_xpath('//*[@id="{}"]'.format(inputs[0]["input_id"]))
    # browser.find_element_by_id(inputs[0]["input_id"]) 

    # Selenium is very slow at traversing the DOM. 
    # To quickly input text in many boxes, we inject a 
    # javacript function into the iframe. The collection
    # of textbox ids and strings is serialized 
    # as a Javascript object literal using the json module.
    inputs= json.dumps(inputs)
    js = "var inputs = {};".format(inputs)
    js += """
    console.log(inputs)
    for (var k = 0; k < inputs.length; k++) {
        var inputStr = inputs[k]["input_str"];
        var input = document.getElementById(inputs[k]["input_id"]);
        input.value = inputStr;
    }
    return true;"""
    browser.execute_script(js)

browser = webdriver.Chrome("C://chromedriver//chromedriver") # Create a session of Firefox
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

for i in range(1947,2001):
    povline = str(i/100.00)
    browser.get("http://iresearch.worldbank.org/PovcalNet/povDuplicateWB.aspx") # Load page
    queries = []
    plInput = {}
    plInput["input_id"] = "txtPovertyLine"
    plInput["input_str"] = povline
    queries.append(plInput)
    input_text(browser, queries)
    yearSelect = browser.find_element_by_xpath('//*[@name="Years"]')
    yearOptions = yearSelect.find_elements_by_tag_name('option')
    for yearOption in yearOptions:
        yearOption.click()
    browser.find_element_by_xpath('//*[@id="SubmitValue"]').click()
    browser.find_element_by_xpath('//*[@id="btnShowAllCountries"]').click()
    tableElements = browser.find_elements_by_css_selector("#Aggre table table.oTbl")
    years = [2013,2012,2011,2010,2008,2005,2002,1999,1996,1993,1990,1987,1984,1981]
    output = []
    for j in range(0,len(tableElements)):
        tableElement = tableElements[j]
        year = years[j]
        tableSource = tableElement.get_attribute('outerHTML')
        df = pd.read_html(tableSource)[0]
        df.columns = ["region","pl","hc","pg","pg2","poor","pop","coverage"]
        df['year'] = year
        output.append(df)
    frame = pd.concat(output)
    frame.loc[frame.pl=="Survey data coverage is too low, the result is suppressed",["coverage"]] = frame["hc"].values
    frame.loc[frame.pl=="Survey data coverage is too low, the result is suppressed",["hc"]] = None
    frame.loc[frame.pl=="Survey data coverage is too low, the result is suppressed",["pl"]] = povline
    frame = frame.replace("n/a","")
    frame.to_csv(options.output+povline+".csv",index=False,header=False)

for i in range(3,151):
    povline = str(i*10.00)
    browser.get("http://iresearch.worldbank.org/PovcalNet/povDuplicateWB.aspx") # Load page
    queries = []
    plInput = {}
    plInput["input_id"] = "txtPovertyLine"
    plInput["input_str"] = povline
    queries.append(plInput)
    input_text(browser, queries)
    yearSelect = browser.find_element_by_xpath('//*[@name="Years"]')
    yearOptions = yearSelect.find_elements_by_tag_name('option')
    for yearOption in yearOptions:
        yearOption.click()
    browser.find_element_by_xpath('//*[@id="SubmitValue"]').click()
    browser.find_element_by_xpath('//*[@id="btnShowAllCountries"]').click()
    tableElements = browser.find_elements_by_css_selector("#Aggre table table.oTbl")
    years = [2013,2012,2011,2010,2008,2005,2002,1999,1996,1993,1990,1987,1984,1981]
    output = []
    for j in range(0,len(tableElements)):
        tableElement = tableElements[j]
        year = years[j]
        tableSource = tableElement.get_attribute('outerHTML')
        df = pd.read_html(tableSource)[0]
        df.columns = ["region","pl","hc","pg","pg2","poor","pop","coverage"]
        df['year'] = year
        output.append(df)
    frame = pd.concat(output)
    frame.loc[frame.pl=="Survey data coverage is too low, the result is suppressed",["coverage"]] = frame["hc"].values
    frame.loc[frame.pl=="Survey data coverage is too low, the result is suppressed",["hc"]] = None
    frame.loc[frame.pl=="Survey data coverage is too low, the result is suppressed",["pl"]] = povline
    frame = frame.replace("n/a","")
    frame.to_csv(options.output+povline+".csv",index=False,header=False)
    
browser.close()
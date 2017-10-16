from selenium import webdriver
import time
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
import pandas as pd
from time import sleep

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="C:\\Users\\Alex\\Documents\\Data\\PovCal_Increment\\individual\\",
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

i = 1
povline = str(i/100.00)
browser.get("http://iresearch.worldbank.org/PovcalNet/povOnDemand.aspx") # Load page
browser.find_element_by_xpath('//*[@title="Add All"]').click()
alert = browser.switch_to_alert()
alert.accept()
browser.find_element_by_xpath('//*[@id="btnSubmitCountries"]').click()
queries = []
plInput = {}
plInput["input_id"] = "PL0"
plInput["input_str"] = povline
queries.append(plInput)
input_text(browser, queries)
browser.find_element_by_xpath('//*[@id="btnSetToAll"]').click()
# browser.find_element_by_xpath('//*[@id="btnSelectAllYears"]').click()
browser.find_element_by_xpath('//*[@id="selAll2014"]').click()
browser.find_element_by_xpath('//*[@id="selAll2015"]').click()
browser.find_element_by_xpath('//*[@id="selAll2016"]').click()
browser.find_element_by_xpath('//*[@id="chkOutputFT"]').click()
browser.find_element_by_xpath('//*[@title="Get result"]').click()
sleep(1)
tableElement = browser.find_element_by_xpath('//*[@class="oTbl"]')
tableSource = tableElement.get_attribute('outerHTML')
df = pd.read_html(tableSource)[0]
df.to_csv(options.output+povline+".csv",index=False,header=False)
browser.find_element_by_xpath('//*[@title="Back to country selection pages"]').click()

for i in range(2,2001):
    sleep(0.5)
    povline = str(i/100.00)
    queries = []
    plInput = {}
    plInput["input_id"] = "PL0"
    plInput["input_str"] = povline
    queries.append(plInput)
    input_text(browser, queries)
    browser.find_element_by_xpath('//*[@id="btnSetToAll"]').click()
    browser.find_element_by_xpath('//*[@title="Get result"]').click()
    sleep(1)
    tableSource = browser.find_element_by_xpath('//*[@class="oTbl"]').get_attribute('outerHTML')
    df = pd.read_html(tableSource)[0]
    df.to_csv(options.output+povline+".csv",index=False,header=False)
    browser.find_element_by_xpath('//*[@title="Back to country selection pages"]').click()
    
for i in range(3,151):
    sleep(0.5)
    povline = str(i*10.00)
    queries = []
    plInput = {}
    plInput["input_id"] = "PL0"
    plInput["input_str"] = povline
    queries.append(plInput)
    input_text(browser, queries)
    browser.find_element_by_xpath('//*[@id="btnSetToAll"]').click()
    browser.find_element_by_xpath('//*[@title="Get result"]').click()
    sleep(1)
    tableSource = browser.find_element_by_xpath('//*[@class="oTbl"]').get_attribute('outerHTML')
    df = pd.read_html(tableSource)[0]
    df.to_csv(options.output+povline+".csv",index=False,header=False)
    browser.find_element_by_xpath('//*[@title="Back to country selection pages"]').click()

browser.close()
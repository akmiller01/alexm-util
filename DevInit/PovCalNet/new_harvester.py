from selenium import webdriver
import time
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
import pandas as pd

parser = OptionParser()
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\PovCal_Increment\\aggregates\\",
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

# for i in range(1,2):
for i in range(820,2001):
    povline = str(i/100.00)
    browser.get("http://iresearch.worldbank.org/PovcalNet/povOnDemand.aspx") # Load page
    browser.find_element_by_xpath('//*[@title="Add All"]').click()
    alert = browser.switch_to_alert()
    alert.accept()
    alert = browser.switch_to_alert()
    alert.accept()
    browser.find_element_by_xpath('//*[@id="chk_GroupUp"]').click()
    browser.find_element_by_xpath('//*[@id="btnSubmitCountries"]').click()
    queries = []
    plInput = {}
    plInput["input_id"] = "NewPovertyLine"
    plInput["input_str"] = povline
    queries.append(plInput)
    input_text(browser, queries)
    yearSelect = browser.find_element_by_xpath('//*[@name="Years"]')
    yearOptions = yearSelect.find_elements_by_tag_name('option')
    for yearOption in yearOptions:
        yearOption.click()
    browser.find_element_by_xpath('//*[@title="Get result"]').click()
    browser.find_element_by_xpath('//*[@id="btnShowAllCountries"]').click()
    tableElement = browser.find_element_by_xpath('//*[@id="Smry"]')
    tableSource = tableElement.get_attribute('outerHTML')
    df = pd.read_html(tableSource)[0]
    df.to_csv(options.output+povline+".csv",index=False,header=False)
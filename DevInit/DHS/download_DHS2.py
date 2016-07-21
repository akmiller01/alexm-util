from selenium import webdriver
import time
from time import sleep
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os

class Error(Exception):
    """Base class for exceptions in this module."""
    pass

class InputError(Error):
    """Exception raised for errors in the input.

    Attributes:
        msg  -- explanation of the error
    """

    def __init__(self, msg):
        self.msg = msg

parser = OptionParser()
parser.add_option("-u", "--username", dest="user", default="alex.miller@devinit.org",
                help="DHS username", metavar="STRING")
parser.add_option("-p", "--password", dest="password", default=False,
                        help="DHS password",metavar="STRING")
parser.add_option("-r", "--project", dest="proj", default=1,
                        help="Project index",metavar="INTEGER")
parser.add_option("-d", "--download", dest="download",
                        help="Bulk download URL",metavar="STRING")
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\Data\\DHS gps data",
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
    
if not options.password:
    raise InputError("A valid password was not supplied.")

profile = webdriver.FirefoxProfile() #Change default download location for Firefox
profile.set_preference("browser.download.folderList", 2)
profile.set_preference('browser.download.manager.showWhenStarting', False)
profile.set_preference("browser.download.dir", options.output)
profile.set_preference("browser.helperApps.alwaysAsk.force", False)
profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/x-zip-compressed,application/zip,application/octet-stream")

browser = webdriver.Firefox(firefox_profile=profile) # Create a session of Firefox
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

browser.get(options.download)

links = browser.page_source.split("\n")

logged_in = False
for link in links:
    if link != "" and logged_in == False:
        browser.get(link)
        queries = []
        userInput = {}
        userInput["input_id"] = "UserName"
        userInput["input_str"] = options.user
        queries.append(userInput)
        passInput = {}
        passInput["input_id"] = "Password"
        passInput["input_str"] = options.password
        queries.append(passInput)
        input_text(browser, queries)
        browser.find_element_by_xpath('//*[@name="submit"]').click() #Click the submit button
        browser.find_element_by_xpath("//*[@name='proj_id']/option[{}]".format(options.proj+1)).click() #Click on the project option in the drop down
        browser.find_element_by_xpath('//*[@type="submit"]').click() #Click the submit button
        logged_in = True
        browser.get(link)
        sleep(1)
    elif link!="":
        browser.get(link)
        sleep(1)
from selenium import webdriver
import time
from time import sleep
import json
import pdb
from selenium.webdriver.remote.command import Command
from optparse import OptionParser
import os
import glob

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
parser.add_option("-o", "--output", dest="output", default="D:\\Documents\\DollarStreet\\Homes\\Unsorted\\",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

chromeOptions = webdriver.ChromeOptions()
prefs = {"download.default_directory" : options.output
         ,"directory_upgrade":True
         ,"extensions_to_open":""
         ,"profile.default_content_settings.popups": 0}
chromeOptions.add_experimental_option("prefs",prefs)

# profile = webdriver.FirefoxProfile() #Change default download location for Firefox
# profile.set_preference("browser.download.folderList", 2)
# profile.set_preference('browser.download.manager.showWhenStarting', False)
# profile.set_preference("browser.download.dir", options.output)
# profile.set_preference("browser.helperApps.alwaysAsk.force", False)
# profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "image/jpeg,image/pjpeg")

browser = webdriver.Chrome("C://chromedriver//chromedriver",chrome_options=chromeOptions)
browser.maximize_window()
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

for i in range(1,146):
    browser.get("https://www.gapminder.org/dollar-street/matrix?thing=Homes&countries=World&regions=World&zoom=10&row=1&lowIncome=26&highIncome=15000&activeHouse="+str(i)) # Load page
    downloadLink = browser.find_element_by_css_selector("a.download")
    incomes = browser.find_elements_by_css_selector("span.place-image-box-income")
    income = incomes[i-1].text[1:].replace(" ","")
    browser.get(downloadLink.get_attribute("href"))
    sleep(5)
    newest = max(glob.iglob(options.output+'*.[Jj][Pp][Gg]'), key=os.path.getctime)
    errorCount = 0
    success = False
    while success == False:
        try:
            os.rename(newest,options.output+income+"_"+str(errorCount)+".jpg")
            success = True
        except WindowsError:
            errorCount = errorCount + 1
            success = False

browser.close()
        
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
parser.add_option("-d", "--download", dest="download", default="C:\\Users\\Alex\\Documents\\Data\\P20\\MICS_urls.txt",
                        help="Bulk download PATH",metavar="STRING")
parser.add_option("-o", "--output", dest="output", default="C:\\Users\\Alex\\Documents\\Data\\P20\\MICS",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

chromeOptions = webdriver.ChromeOptions()
prefs = {"download.default_directory" : options.output
         ,"directory_upgrade":True
         ,"extensions_to_open":""
         ,"profile.default_content_settings.popups": 0}
chromeOptions.add_experimental_option("prefs",prefs)

browser = webdriver.Chrome("C://chromedriver//chromedriver",chrome_options=chromeOptions)
browser.implicitly_wait(30) # Configure the WebDriver to wait up to 30 seconds for each page to load

links = open(options.download).read().splitlines()

for link in links:
    if link != "":
        browser.get(link)
        sleep(1)

browser.close()
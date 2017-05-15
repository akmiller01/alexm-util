from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from glob import glob
import os
from optparse import OptionParser
from os.path import basename, splitext
from PIL import Image
import pdb

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="C:\\Users\\alexm\\Documents\\charts\\Gates\\Global trends",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

opts = webdriver.ChromeOptions()
opts.add_argument("start-maximized")


browser = webdriver.Chrome(executable_path="C://chromedriver//2.29//chromedriver",chrome_options=opts)

svgs = glob(options.input+"\\*.svg")

for svg in svgs:
    filename = options.input+"\\"+basename(splitext(svg)[0])+".png"
    browser.get(svg)
    browser.save_screenshot(filename)
    
    element = browser.find_element_by_css_selector('svg')
    location = element.location
    size = element.size
    
    im = Image.open(filename) # uses PIL library to open image in memory
    width, height = im.size

    left = int(location['x'])
    top = int(location['y'])
    right = int(location['x']) + min(int(size['width']),width)
    bottom = int(location['y']) + min(int(size['height']),height)


    im = im.crop((left, top, right, bottom)) # defines crop points
    im.save(filename) # saves new cropped image

browser.close()
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from glob import glob
import os
from optparse import OptionParser
from os.path import basename, splitext
from PIL import Image
import pdb
from pyvirtualdisplay import Display

display = Display(visible=0, size=(1000, 1000))
display.start()

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="/media/alex/HD/charts/Gates/Donor trends",
                        help="Output path. Default is wd",metavar="FOLDER")
(options, args) = parser.parse_args()

# opts = webdriver.ChromeOptions()
# opts.add_argument("start-maximized")
# 
# 
# browser = webdriver.Chrome(chrome_options=opts)
browser = webdriver.Chrome()


svgs = glob(options.input+"/*.svg")

for svg in svgs:
    filename = options.input+"/"+basename(splitext(svg)[0])+".png"
    browser.get("file://"+svg)
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
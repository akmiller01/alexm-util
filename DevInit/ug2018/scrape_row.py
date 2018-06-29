#!/usr/bin/env python

import sys, os
import re
import lxml.etree
from optparse import OptionParser
import pdb
import itertools
import operator
import pandas as pd
from operator import itemgetter
import glob
from itertools import groupby
import numpy as np
from PIL import Image

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="/home/alex/Documents/budget_scrape_2018/",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="/home/alex/Documents/budget_scrape_2018/output/",
                        help="Output path. Default is './'",metavar="FOLDER")
parser.add_option("-d", "--debug", dest="debug", default=False,
                        help="Debug",metavar="BOOLEAN")
(options, args) = parser.parse_args()

def trytext(el):
    textList = []
    text = el.text
    childText = None
    grandchildText = None
    children = el.getchildren()
    childLen = len(children)
    if childLen>0:
        child = children[0]
        childText = child.text
        grandchildren = child.getchildren()
        grandchildLen = len(grandchildren)
        if grandchildLen>0:
            grandchild = grandchildren[0]
            grandchildText = grandchild.text
    result = ""
    textList.append(text)
    textList.append(childText)
    textList.append(grandchildText)
    finalList = filter(None,textList)
    result = " ".join(finalList)
    if result=="":
        return None
    else:
        return result

def pdftoxml(pdfdata, options):
    """converts pdf file to xml file"""
    # lots of hacky Windows fixes c.f. original
    basename = os.path.basename(pdfdata)
    inputname, inputextension = os.path.splitext(basename)
    absDir = os.path.dirname(pdfdata)+"/"
    result = absDir+inputname+'.xml'
    if not os.path.isfile(result):
        cmd = 'pdftohtml -xml -nodrm -zoom 1.5 -enc UTF-8 -noframes "'
        if options:
            cmd += options
        cmd += pdfdata
        cmd +=  '" "'
        cmd += absDir
        cmd += inputname+'.xml"'
        cmd = cmd + " > NUL 2>&1" # can't turn off output, so throw away even stderr yeuch
        os.system(cmd)
    return result

def main():
    #Before writing, try pdftohtml NAME.pdf -xml NAME.xml
    #Requires Poppler for windows in your path
    #http://blog.alivate.com.au/poppler-windows/
    paths = glob.glob(options.input+"/*.pdf")
    for path in paths:
        basename = os.path.basename(path)
        inputname, inputextension = os.path.splitext(basename)
        print("Reading "+basename+"...")
        xmlpath = pdftoxml(path, False)
        root = lxml.etree.parse(xmlpath).getroot()
        pages = list(root)
        pageLen = len(pages)
        for i in range(0, pageLen):
            pageData = []
            page = pages[i]
            elLen = len(page)
            textLen = 0
            pageTexts = []
            try:
                pageWidth = int(page.attrib['width'])
                pageHeight = int(page.attrib['height'])
            except:
                continue
            for j in range(0,elLen):
                el = page[j]
                if el.tag == "text":
                    textLen += 1
                    left = int(el.attrib['left'])
                    width = int(el.attrib['width'])
                    right = left + width
                    top = int(el.attrib['top'])
                    height = int(el.attrib['height'])
                    bottom = top + height
                    textObj = {"left": left, "right": right, "top": top, "bottom": bottom, "width": width, "text": trytext(el)}
                    pageTexts.append(textObj)
            if textLen > 0:
                rowgetter = itemgetter('top')
                sorted_text = sorted(pageTexts, key=rowgetter)
                rows = groupby(sorted_text, key=rowgetter)
                for k, row in rows:
                    matching_row = list(row)
                    row_text = " ".join([el['text'] for el in matching_row]).replace(",", "").replace(".", "")
                    wordRegex = re.compile(r'\D+\s\D+')
                    numRegex = re.compile(r'\s\d+')
                    codeRegex = re.compile(r'\D+_\d+')
                    data_row = codeRegex.findall(row_text) + wordRegex.findall(row_text) + numRegex.findall(row_text)
                    pageData.append(data_row)
                pd.DataFrame(pageData).to_csv(options.output+inputname+"-"+str(i+1)+".csv", index=False, header=False)
    sys.stdout.write("\n")
    print("Done.")

main()

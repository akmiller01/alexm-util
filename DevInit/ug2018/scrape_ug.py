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
parser.add_option("-i", "--input", dest="input", default="C:/Users/Alex/Documents/Data/Uganda2018",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="C:/Users/Alex/Documents/Data/Uganda2018/pages/",
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
    
def roundx(num,x=5):
    """Round to the nearest x"""
    return round(float(num)/float(x))*x

def main():
    #Before writing, try pdftohtml NAME.pdf -xml NAME.xml
    #Requires Poppler for windows in your path
    #http://blog.alivate.com.au/poppler-windows/
    paths = glob.glob(options.input+"/*.pdf")
    for path in paths:
        basename = os.path.basename(path)
        inputname, inputextension = os.path.splitext(basename)
        print("Reading "+basename+"...")
        xmlpath = pdftoxml(path,False)
        root = lxml.etree.parse(xmlpath).getroot()
        pages = list(root)
        pageLen = len(pages)
        for i in range(0,pageLen):
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
            overlap_columns = pageWidth*[0]
            overlap_rows = pageHeight*[0]
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
                    #Measure overlap
                    for k in range(left,right+1):
                        overlap_columns[k] += 1
                    for k in range(top,bottom+1):
                        overlap_rows[k] += 1
                    textObj = {"left":left,"right":right,"top":top,"bottom":bottom,"width":width,"text":trytext(el)}
                    pageTexts.append(textObj)
            if textLen > 0:
                col_array = np.array(overlap_columns)
                row_array = np.array(overlap_rows)
                rescaled_col = (100.0 / col_array.max() * (col_array - col_array.min()))
                rescaled_row = (100.0 / row_array.max() * (row_array - row_array.min()))
                col_ws = np.where(rescaled_col<10)[0]
                row_ws = np.where(rescaled_row<10)[0]
                column_starts = col_ws[np.where(np.ediff1d(col_ws)>10)[0]]
                column_ends = col_ws[np.where(np.ediff1d(col_ws)>10)[0]+1]
                row_starts = row_ws[np.where(np.ediff1d(row_ws)>1)[0]]
                row_ends = row_ws[np.where(np.ediff1d(row_ws)>1)[0]+1]
                if len(column_starts) >= 3:
                    for r in range(0,len(row_starts)):
                        r_start = row_starts[r]
                        r_end = row_ends[r]
                        matching_row = [el for el in pageTexts if (roundx(el['top'])>=roundx(r_start) and roundx(el['bottom'])<=roundx(r_end))]
                        data_row = []
                        for c in range(0,len(column_starts)):
                            if c-1>=0:
                                c_start = column_ends[c-1]
                                real_c_start = column_starts[c]
                            else:
                                c_start = column_starts[c]
                                real_c_start = c_start
                            try:
                                c_end = column_starts[c+1]
                                real_c_end = column_ends[c]
                            except IndexError:
                                c_end = column_ends[c]
                                real_c_end = c_end
                            matching_text = [el['text'] for el in matching_row if (roundx(el['left'])>=roundx(c_start) and roundx(el['right'])<=roundx(c_end) and el['text']!="-")]
                            overlapping_text = [el['text'] for el in matching_row if (el['left']<real_c_start and el['right']>real_c_end and el['text']!="-" and el['width']>50)]
                            dedup = list(set(overlapping_text+matching_text))
                            cell_text = " ".join(dedup)
                            data_row.append(cell_text)
                        pageData.append(data_row)
                    pd.DataFrame(pageData).to_csv(options.output+inputname+"-"+str(i+1)+".csv",index=False,header=False,sep="\t",encoding="utf-16")
    sys.stdout.write("\n")
    print("Done.")

main()
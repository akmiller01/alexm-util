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
from collections import defaultdict

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
    datasets = []
    #for i in range(1):
    #    path = paths[i]
    for path in paths:
        basename = os.path.basename(path)
        inputname, inputextension = os.path.splitext(basename)
        print("Reading "+basename+"...")
        xmlpath = pdftoxml(path,False)
        root = lxml.etree.parse(xmlpath).getroot()
        pages = list(root)
        output = []
        pageLen = len(pages)
        #Cascade these down...
        vote = ""
        for i in range(0,pageLen):
            page = pages[i]
            elLen = len(page)
            for j in range(0,elLen):
                el = page[j]
                if el.tag == "text":
                    left = int(el.attrib['left'])
                    right = int(el.attrib['left'])+int(el.attrib['width'])
                    top = int(el.attrib['top'])
                    #Scrape all page text by going backwards and forwards...
                    pageTexts = []
                    unique_bounds_dict = defaultdict(lambda: 0)
                    textObj = {"left":roundx(left),"right":roundx(right),"top":roundx(top),"text":trytext(el)}
                    pageTexts.append(textObj)
                    unique_bounds_dict[",".join([str(textObj['left']),str(textObj['right'])])] += 1
                    #Backwards
                    prev = el.getprevious()
                    while prev is not None and prev.tag == "text":
                        left = int(prev.attrib['left'])
                        right = int(prev.attrib['left'])+int(prev.attrib['width'])
                        top = int(prev.attrib['top'])
                        textObj = {"left":roundx(left),"right":roundx(right),"top":roundx(top),"text":trytext(prev)}
                        pageTexts.append(textObj)
                        unique_bounds_dict[",".join([str(textObj['left']),str(textObj['right'])])] += 1
                        prev = prev.getprevious()
                    #Forwards
                    nxt = el.getnext()
                    while nxt is not None and nxt.tag == "text":
                        left = int(nxt.attrib['left'])
                        right = int(nxt.attrib['left'])+int(nxt.attrib['width'])
                        top = int(nxt.attrib['top'])
                        textObj = {"left":roundx(left),"right":roundx(right),"top":roundx(top),"text":trytext(nxt)}
                        pageTexts.append(textObj)
                        unique_bounds_dict[",".join([str(textObj['left']),str(textObj['right'])])] += 1
                        nxt = nxt.getnext()
                    unique_bounds = [[int(bound) for bound in bounds_str.split(",")] for bounds_str in unique_bounds_dict.keys()]
                    if i==5:
                        pdb.set_trace()
                    # max_columns = max(unique_lefts.values())
                    # if max_columns < 3:
                    #     #Not a table
                    #     continue
                    # #Might be a table
                    # cellvals = operator.itemgetter('top','left')
                    # pageTexts.sort(key=cellvals)
                    # rowvals = operator.itemgetter('top')
                    # colvals = operator.itemgetter('left')
                    # table_matrix = []
                    # #Appears at least twice
                    # orphan_lefts = { k:v for k, v in unique_lefts.items() if v<2 }
                    # unique_lefts = { k:v for k, v in unique_lefts.items() if v>=2 }
                    # for k,v in groupby(pageTexts,key=rowvals):
                    #     row = list(v)
                    #     row_lefts = [str(item['left']) for item in row]
                    #     left_diffs = list(set(unique_lefts) - set(row_lefts))
                    #     for left_diff in left_diffs:
                    #         missing_left = int(left_diff)
                    #         obj = {"left":missing_left,"text":""}
                    #         row.append(obj)
                    #     for orphan_left in orphan_lefts:
                    #         o_left = int(orphan_left)
                    #         row[:] = [d for d in row if d.get('left')!=o_left]
                    #     row.sort(key=colvals)
                    #     row_texts = [item['text'] for item in row]
                    #     table_matrix.append(row_texts)
                    # df = pd.DataFrame(table_matrix)
                    # df.to_csv(options.output+str(i)+".csv", sep='\t',encoding="utf-16")
    sys.stdout.write("\n")
    print("Done.")

main()
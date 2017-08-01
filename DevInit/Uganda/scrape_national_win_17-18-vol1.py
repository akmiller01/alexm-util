#!/usr/bin/env python

import sys, os
import re
import urllib2, lxml.etree
from optparse import OptionParser
import pdb
import itertools
import operator
import csv
from operator import itemgetter
# import numpy as np

parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="./Central govt/Draft Estimates 040417_0203_0.pdf",
                help="Input pdf name", metavar="FILE")
parser.add_option("-o", "--output", dest="output", default="./tmp/",
                        help="Output path. Default is './tmp/'",metavar="FOLDER")
parser.add_option("-d", "--debug", dest="debug", default=False,
                        help="Debug",metavar="BOOLEAN")
(options, args) = parser.parse_args()

def remdash(string):
    return unicode(string.replace(u'\u2013',"-")).encode('utf-8')
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
    output = remdash(result)
    if output=="":
        return None
    else:
        return output
    
def pdftoxml(pdfdata, options):
    """converts pdf file to xml file"""
    # lots of hacky Windows fixes c.f. original
    absDir = os.path.dirname(pdfdata)+"/"
    cmd = 'pdftohtml -xml -nodrm -zoom 1.5 -enc UTF-8 -noframes "'
    if options:
        cmd += options
    cmd += pdfdata
    cmd +=  '" "'
    cmd += absDir
    cmd +='output.xml"'
    cmd = cmd + " > NUL 2>&1" # can't turn off output, so throw away even stderr yeuch
    os.system(cmd)
    with open(absDir+'output.xml', 'r') as f:
        return f.read()

def main():
    #Before writing, try pdftohtml NAME.pdf -xml NAME.xml
    #Requires Poppler for windows in your path
    #http://blog.alivate.com.au/poppler-windows/
    basename = os.path.basename(options.input)
    inputname, inputextension = os.path.splitext(basename)
    sys.stdout.write("Reading "+basename+"... This may take a while....")
    xmldata = pdftoxml(options.input,False)
    escapes = ''.join([chr(char) for char in range(1, 32)])
    root = lxml.etree.fromstring(xmldata.translate(None, escapes))
    pages = list(root)
    output = []
    pageLen = len(pages)
    #Cascade these down...
    vote = ""
    programme = ""
    budgetType = ""
    sp_pj = ""
    isTableV3 = False
    sp_column_headers = ["Wage","Non Wage","AIA","Total","Wage","Non Wage","AIA","Total"]
    pj_column_headers = ["GoU Dev't","External Fin","AIA","Total","GoU Dev't","External Fin","AIA","Total"]
    year_headers = ["2016/17 Approved Budget","2016/17 Approved Budget","2016/17 Approved Budget","2016/17 Approved Budget","2017/18 Draft Estimates","2017/18 Draft Estimates","2017/18 Draft Estimates","2017/18 Draft Estimates",]
    #Turned on for multiple pages, remember to turn of off when you spot Table V2
    for i in range(0,pageLen):
        page = pages[i]
        elLen = len(page)
        for j in range(0,elLen):
            el = page[j]
            if el.tag == "text":
                left = int(el.attrib['left'])
                right = int(el.attrib['left'])+int(el.attrib['width'])
                top = int(el.attrib['top'])
                font = int(el.attrib['font'])
                if not isTableV3:
                    if trytext(el)=="Table V3: Detailed Estimates by Programme, Sub Programme, Output and Item":
                        isTableV3 = True
                        programme = trytext(el.getnext())
                        budgetType = trytext(el.getnext().getnext())
                        break
        if isTableV3:
            y_positioned_elements = []
            #Put all elements into this array, and then sort it by top position
            obj = {}
            obj['text'] = trytext(el)
            obj['top'] = int(el.attrib['top'])
            obj['left'] = int(el.attrib['left'])
            obj['right'] = int(el.attrib['left'])+int(el.attrib['width'])
            obj['font'] = int(el.attrib['font'])
            y_positioned_elements.append(obj)
            #Backwards
            prev = el.getprevious()
            while prev is not None:
                if prev.tag=="text":
                    obj = {}
                    obj['text'] = trytext(prev)
                    obj['top'] = int(prev.attrib['top'])
                    obj['left'] = int(prev.attrib['left'])
                    obj['right'] = int(prev.attrib['left'])+int(prev.attrib['width'])
                    obj['font'] = int(prev.attrib['font'])
                    y_positioned_elements.append(obj)
                prev = prev.getprevious()
            #Forwards
            nxt = el.getnext()
            while nxt is not None:
                if nxt.tag=="text":
                    obj = {}
                    obj['text'] = trytext(nxt)
                    obj['top'] = int(nxt.attrib['top'])
                    obj['left'] = int(nxt.attrib['left'])
                    obj['right'] = int(nxt.attrib['left'])+int(nxt.attrib['width'])
                    obj['font'] = int(nxt.attrib['font'])
                    y_positioned_elements.append(obj)
                nxt = nxt.getnext()
            rowvals = operator.itemgetter('top','left')
            y_positioned_elements.sort(key=rowvals)
            ordered_texts = [item['text'] for item in y_positioned_elements]
            if 'Table V1: Summary Of Vote Estimates by Programme  and Sub-Programme' in ordered_texts:
                isTableV3 = False
                next
            vote = ordered_texts[0]+ordered_texts[1]+" "+ordered_texts[2]
            
            for k in range(0,len(y_positioned_elements)):
                el = y_positioned_elements[k]
                if el['text'].startswith("Programme"):
                    programme = el['text']
                elif el['text'] in ["Recurrent Budget Estimates","Development Budget Estimates"]:
                    budgetType = el['text']
                elif el['text'].startswith("SubProgramme"):
                    sp_pj = el['text']
                elif el['text'].startswith("Total Cost for SubProgramme"):
                    totalcostsrow = ordered_texts[k+1:k+9]
                    split_tcr = [text.split() for text in totalcostsrow]
                    split_flat = [item for sublist in split_tcr for item in sublist][0:7]
                    for m in range(0,len(split_flat)):
                        obj = {}
                        obj["vote"] = vote
                        obj["programme"] = programme
                        obj["budgetType"] = budgetType
                        obj["sp_pj"] = sp_pj
                        obj["year"] = year_headers[m]
                        obj["columnType"] = sp_column_headers[m]
                        obj["value"] = split_flat[m]
                        obj["page"] = i+1
                        output.append(obj)
                elif el['text'].startswith("Total Cost for Project"):
                    split_tcr = [text.split() for text in totalcostsrow]
                    split_flat = [item for sublist in split_tcr for item in sublist][0:7]
                    for m in range(0,len(split_flat)):
                        obj = {}
                        obj["vote"] = vote
                        obj["programme"] = programme
                        obj["budgetType"] = budgetType
                        obj["sp_pj"] = sp_pj
                        obj["year"] = year_headers[m]
                        obj["columnType"] = pj_column_headers[m]
                        obj["value"] = split_flat[m]
                        obj["page"] = i+1
                        output.append(obj)
            
            # name_locations = np.sort(np.append(np.where([el.startswith("SubProgramme") for el in ordered_texts])[0],np.where([el.startswith("Project") for el in ordered_texts])[0]))
            # totalcost_locations = np.sort(np.append(np.where([el.startswith("Total Cost for SubProgramme") for el in ordered_texts])[0],np.where([el.startswith("Total Cost for Project") for el in ordered_texts])[0]))

    if options.debug:
        pdb.set_trace()
    keys = output[0].keys()
    with open(options.output+inputname+".csv", 'wb') as output_file:
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(output)
    sys.stdout.write("\n")
    print("Done.")

main()
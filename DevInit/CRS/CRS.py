#!/usr/bin/env python

#Import system
import glob
import sys, os
import zipfile,os.path
import io
from optparse import OptionParser
import codecs
import subprocess
import csv
import pdb
from progressbar import ProgressBar


#Parse Options
parser = OptionParser()
parser.add_option("-i", "--input", dest="input", default="C:\\Users\\Alex\\Documents\\Data\\CRS\\",
                help="Input folder.", metavar="FOLDER")
parser.add_option("-o", "--output", dest="output", default="C:\\Users\\Alex\\Documents\\Data\\CRS\\",
                help="Output folder.", metavar="FILE")
parser.add_option("-u", "--unzip", dest="unzip", default=False,
                help="Re-unzip?", metavar="BOOLEAN")
(options, args) = parser.parse_args()

reload(sys)
sys.setdefaultencoding('utf-8')

def uni(input):
    try:
        output = unicode(input).encode('latin1', 'replace')
    except:
        output = unicode(input).encode('utf-8', 'replace')
    return output

class Recoder(object):
    def __init__(self, stream, decoder, encoder, eol='\r\n'):
        self._stream = stream
        self._decoder = decoder if isinstance(decoder, codecs.IncrementalDecoder) else codecs.getincrementaldecoder(decoder)()
        self._encoder = encoder if isinstance(encoder, codecs.IncrementalEncoder) else codecs.getincrementalencoder(encoder)()
        self._buf = ''
        self._eol = eol
        self._reachedEof = False

    def read(self, size=None):
        r = self._stream.read(size)
        raw = self._decoder.decode(r, size is None)
        return self._encoder.encode(raw)

    def __iter__(self):
        return self

    def __next__(self):
        if self._reachedEof:
            raise StopIteration()
        while True:
            line,eol,rest = self._buf.partition(self._eol)
            if eol == self._eol:
                self._buf = rest
                return self._encoder.encode(line + eol)
            raw = self._stream.read(1024)
            if raw == '':
                self._decoder.decode(b'', True)
                self._reachedEof = True
                return self._encoder.encode(self._buf)
            self._buf += self._decoder.decode(raw)
    next = __next__

    def close(self):
        return self._stream.close()

def unzip(source_filename, dest_dir):
    zip_ref = zipfile.ZipFile(source_filename,'r')
    zip_ref.extractall(dest_dir)
    zip_ref.close()
            
#Find .zip in folder
paths = glob.glob(options.input+"*.zip")

#Iterate through paths and unzip
if options.unzip==True:
    for inPath in paths:
        filename = os.path.basename(inPath)
        print "Extracting "+filename
        unzip(inPath,options.input)

#Find .txt in folder
txtpaths = glob.glob(options.input+"/*.txt")

#Iterate through paths and re-encode and replace nul
for inPath in txtpaths:
    pbar = ProgressBar()
    filename = os.path.basename(inPath)
    name, extension = os.path.splitext(filename)
    print "Reading "+filename
    utf16s = []
    if filename in utf16s:
        CRS_encoding = "utf-16"
    else:
        CRS_encoding = "latin1"
    with open(inPath,'rb') as fr:
        sr = Recoder(fr, CRS_encoding, 'utf-8')
        outPath = options.output+name+".csv"
        with open(outPath, 'wb') as fw:
            writer = csv.writer(fw,delimiter=",",quotechar="\"")
            for line in pbar(sr):
                row = line.replace("\x00","").replace("\x1a","'").split("|")[0:-1]
                writer.writerow(map(uni,row))
    os.remove(inPath)
    sys.stdout.write("\n")
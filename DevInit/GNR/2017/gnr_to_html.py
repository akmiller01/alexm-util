import os

rootdir = "C:\\Users\\Alex\\Documents\\Data\\GNR\\Aggregate profile PDFs\\"
outputdir = "C:\\Users\\Alex\\Documents\\Data\\GNR\\HTML\\"

for subdir, dirs, files in os.walk(rootdir):
    for filename in files:
        filepath = os.path.join(subdir,filename)
        pathsplit = os.path.splitext(filename)
        basename = pathsplit[0]
        print(basename)
        extension = pathsplit[1]
        htmlfilename = basename+".html"
        htmloutputpath = os.path.join(outputdir,basename)
        os.mkdir(htmloutputpath)
        if extension==".pdf":
            cmd = 'pdftohtml -s -c "'
            cmd += filepath
            cmd +=  '" "'
            cmd += os.path.join(htmloutputpath,htmlfilename)
            cmd += '"'
            os.system(cmd)
            
rootdir = "C:\\Users\\Alex\\Documents\\Data\\GNR\\Country profile PDFs\\"
outputdir = "C:\\Users\\Alex\\Documents\\Data\\GNR\\HTML\\"

for subdir, dirs, files in os.walk(rootdir):
    for filename in files:
        filepath = os.path.join(subdir,filename)
        pathsplit = os.path.splitext(filename)
        basename = pathsplit[0]
        print(basename)
        extension = pathsplit[1]
        htmlfilename = basename+".html"
        htmloutputpath = os.path.join(outputdir,basename)
        os.mkdir(htmloutputpath)
        if extension==".pdf":
            cmd = 'pdftohtml -s -c "'
            cmd += filepath
            cmd +=  '" "'
            cmd += os.path.join(htmloutputpath,htmlfilename)
            cmd += '"'
            os.system(cmd)

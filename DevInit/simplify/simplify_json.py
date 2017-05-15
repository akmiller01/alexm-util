from osgeo import ogr
from optparse import OptionParser
import json
import os
import pdb

parser = OptionParser()
parser.add_option("-i", "--input", dest="input",
                        help="Input geoJson",metavar="GEOJSON")
parser.add_option("-o", "--output", dest="output", default="./",
                        help="Output path. Default is wd",metavar="FOLDER")
parser.add_option("-t", "--tolerance", dest="tolerance", default="0.02",
                        help="Simplification tolerance",metavar="FLOAT")
(options, args) = parser.parse_args()

with open(options.input) as f:
    data = json.load(f)

# Create a multipolygon
multipolygon = ogr.Geometry(ogr.wkbMultiPolygon)
feature_gaps = []

for feature in data['features']:
    geojson = json.dumps(feature['geometry'])
    feature = ogr.CreateGeometryFromJson(geojson)
    polyCount = feature.GetGeometryCount()
    print "Feature has %i polygons" % (polyCount)
    feature_gaps.append(polyCount)
    for i in range(0,polyCount):
        poly = feature.GetGeometryRef(i)
        multipolygon.AddGeometry(poly)

simplified = multipolygon.SimplifyPreserveTopology(float(options.tolerance))
polyStart = 0
polyEnd = 0
for j in range(0,len(feature_gaps)):
    gap = feature_gaps[j]
    multi = ogr.Geometry(ogr.wkbMultiPolygon)
    polyEnd = polyEnd + gap
    print(polyStart,polyEnd)
    for i in range(polyStart,polyEnd):
        poly = simplified.GetGeometryRef(i)
        multi.AddGeometry(poly)
    simp_json = json.loads(multi.ExportToJson())
    data["features"][j]["geometry"] = simp_json
    polyStart = polyEnd
    
filename = os.path.basename(options.input)
with open(options.output+"/simplified_"+filename,"w") as f:
    json.dump(data,f)
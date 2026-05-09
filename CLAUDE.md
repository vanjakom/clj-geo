guidelines:

Reading of files inside current directory and it's subdirectories is allowed.
This is clojure project. All my dependencies are linked inside checkouts
subdirectory, feel free to read them if needed. All other dependencies are
open source, use internet to understand them if needed. For reading of all other
files ask for permission.

issues:

CLAUDE-1 [DONE]
Support different marker types ( :icon :div ) in gejson-style-extended
( clj-common.visualization.map ) :marker-type should be used to distinguish
type of marker to display.

CLAUDE-2
Reading of KMZ [DONE]
Support reading of KMZ inside clj-geo.import.kmz. Add required util functions 
for entity creation into clj-geo.import.geojson. Initial file that should be
read is  Beogradski maraton  Belgrade marathon.kml under 
/Users/vanja/dataset-cloud/beogradski maraton 2026/

CLAUDE-3 [DONE]
Debug GeoJSON file
Write clj-geo.import.geojson/print-debug function. Which will fo over GeoJson
map and write tree like output, printing type of object ( Feature, 
FeatureCollection ) and it's properties ( if has ( indented 3 spaces ) Each
child is indended 3 spaces.

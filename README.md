# clj-geo

collections of fns to help working with geo data

## Usage

clj-geo.dateset namespace should contain fns for load / transform of open source datasets. All fns that load external data are doing that from root directory specified in clj-geo.env/*dataset-path*

clj-geo/env is extracting some values from environment variables.


examine clj-geo.example namespace for examples


### Datasets prepare


#### opencache.uk

register for OKAPI customer API ( https://opencache.uk/okapi/signup.html )

download full dump
wget -o full-dump 'https://opencache.uk/okapi/services/replicate/fulldump?consumer_key=<CUSTOMER_KEY>'
tar -xf full-dump
rm full-dump

this will generate index.json and partXXXXX.json

#### opencaching.de

register for OKAPI customer API ( https://www.opencaching.de/okapi/signup.html )

download full dump
wget -o full-dump 'https://opencaching.de/okapi/services/replicate/fulldump?consumer_key=<CUSTOMER_KEY>'


## links

http://www.naturalearthdata.com
http://thematicmapping.org/downloads/
https://eros.usgs.gov

### collection of geographical names
https://geographic.org/

http://live.farearth.com/


## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

(defproject com.mungolab/clj-geo "0.1.0-SNAPSHOT"
  :description "collections of fns to help working with geo data"
  :url "https://github.com/vanjakom/clj-geo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [
                 ["java.net repo" "https://download.java.net/maven/2"]
                 ["geotools repo" "https://download.osgeo.org/webdav/geotools"]]
  :dependencies [
                 [com.mungolab/clj-common "0.3.0-SNAPSHOT"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [clj-http "2.2.0"]

                 [com.mungolab/clj-scraper "0.1.0-SNAPSHOT"]

                 [org.geotools/gt-main "21.1"]
                 [org.geotools/gt-shapefile "21.1"]
                 ; https://stackoverflow.com/questions/48268257/leiningen-raises-tried-to-use-insecure-http-repository-without-tls-but-for-wh
                 ; retrieves dependency from http
                 ; it.geosolutions.imageio-ext.imageio-ext-tiff
                 #_[org.geotools/gt-swing "21.1"]])

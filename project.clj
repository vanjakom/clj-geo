(defproject com.mungolab/clj-geo "0.2.0-SNAPSHOT"
  :description "collections of fns to help working with geo data"
  :url "https://github.com/vanjakom/clj-geo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories [
                 ["java.net repo" "https://download.java.net/maven/2"]
                 ["osgeo repo" "https://repo.osgeo.org/repository/release/"]
                 ["slimjars" "https://mvn.slimjars.com"]
                 ["topobyte.de" "https://mvn.topobyte.de/"]]
  :source-paths ["src/clj" "src/cljc"]
  :dependencies [
                 [com.mungolab/clj-common "0.3.0-SNAPSHOT"]
                 [com.mungolab/clj-scraper "0.1.0-SNAPSHOT"]
                 
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [clj-http "2.2.0"]
                 [org.geotools/gt-main "25.2"]
                 [org.geotools/gt-shapefile "25.2"]
                 [de.topobyte/osm4j-pbf "1.2.0"]])

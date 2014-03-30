(defproject mdgen "0.1.0-SNAPSHOT"
  :description "XML Metadata manipulation tool"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.xml "0.0.7"]
                 [lein-light-nrepl "0.0.17"]
                 [instaparse "1.3.0"]]
  :repl-options {:nrepl-middleware [lighttable.nrepl.handler/lighttable-ops]})

(ns mdgen.core
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [mdgen.xml :refer :all]
            [clojure.zip :as zip]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]
            [clojure.data.json :as json]
            [clojure.pprint :refer [pprint]]))

(-> "wcmp13-template.xml" io/resource io/file parse-file)

(defn gen-wcmp13-from-draft
  "Create the WCMP 1.3 compatible metadata record using the given
  fileIdentifier and dateStamp from a draft metadata record."
  [urn date])


(defn info-ahl
  "Get the decoded message for the given abbreviated heading line (AHL),
  i.e. TTAAiiCCCC code."
  [ahl])

(def info (with-open [rdr (io/reader "http://wisadmn-d.bom.gov.au/mdmon/mdcheck/decoded?code=SSVX13LFVW&json")]
  (let [msg (apply str (line-seq rdr))]
    (json/read-str msg))))



(ns mdgen.core
  (:require [clojure.java.io :as io]
            [clojure.xml :as xml]
            [mdgen.xml :refer :all]
            [clojure.zip :as zip]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]
            [clojure.data.json :as json]
            [clojurewerkz.propertied.properties :as props]
            [clojure.pprint :refer [pprint]]))


(def config
  (let [m (-> "mdgen.properties" io/resource props/load-from props/properties->map)]
    (into {} (mapcat #(vector [(keyword (% 0)) (% 1)]) m))))


(def template-wcmp13-gx-au
  (-> "md_templates/wcmp13_gx_au.xml" io/resource io/file parse-file))


(defn gen-wcmp13-from-draft
  "Create the WCMP 1.3 compatible metadata record using the given
  fileIdentifier and dateStamp from a draft metadata record."
  [urn date])


(defn- ahl-decoder-url
  "Populate the decoder url with the given AHL code."
  [base-url code]
  (format (str base-url "?code=%s&json") code))

(defn info-ahl
  "Get the decoded message as json for the given abbreviated heading line (AHL),
  i.e. TTAAiiCCCC code."
  [code]
  (with-open [rdr (io/reader (ahl-decoder-url (:ahlDecoderUrl config) code))]
  (let [msg (apply str (line-seq rdr))]
    (json/read-str msg))))


(def q (not-filled? [:gco:nilReason "missing"] [:indeterminatePosition "now"]))


(q (x1-> template-wcmp13-gx-au :gmd:fileIdentifier :gco:CharacterString))

(x-> template-wcmp13-gx-au zf/descendants q (comp vector rpath))

(x-> template-wcmp13-gx-au zf/descendants (complement (zfx/attr= :gco:nilReason "missing"))
     empty-node? (comp vector rpath))



(comment

(let [msg (info-ahl "SSVX13LFVW")
      ks (keys msg)]
  (loop [k (first ks) r (rest ks)]
    (if (boolean k)
      (do
        (println k (get msg k))
        (recur (first r) (rest r))))))


)

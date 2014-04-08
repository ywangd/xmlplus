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


(defn- ahl-decoder-url
  "Populate the decoder url with the given AHL code."
  [base-url code]
  (format (str base-url "?code=%s&json") code))

; The function is set to be dynamic so test codes can bind it with other
; function with no network code.
(defn ^:dynamic info-ahl
  "Query the mdmon web service and get the decoded message as json for the
  given abbreviated heading line (AHL), i.e. TTAAiiCCCC code."
  [code]
  (with-open [rdr (io/reader (ahl-decoder-url (:ahlDecoderUrl config) code))]
    (let [msg (apply str (line-seq rdr))]
      (merge {:ahl code} (json/read-str msg)))))


(comment

  (let [msg (info-ahl "SSVX13LFVW")
        ks (keys msg)]
    (loop [k (first ks) r (rest ks)]
      (if (boolean k)
        (do
          (println k (get msg k))
          (recur (first r) (rest r))))))


  )

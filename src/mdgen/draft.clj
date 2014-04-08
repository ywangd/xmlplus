; This module takes a draft record (essentially just its urn and datestamp)
; and populate a WCMP 1.3 compatible metadata record with the decoded message
; of the AHL code.

(ns mdgen.draft
  (:use [mdgen.xml]
        [mdgen.core])
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]
            [clj-time.core :as ctc]
            [clj-time.format :as ctf]
            [clojure.string :as string]
            [clojure.set :as cs]))


; The metadata template
(def ^:private template
  (-> "md_templates/wcmp13/gx.xml" io/resource io/file parse-file))


(def ^:private paths
  "Paths to the XML elements to be filled"
  {:urn [:gmd:fileIdentifier z>]
   :datestamp [:gmd:dateStamp :gco:DateTime]
   :title [:gmd:identificationInfo z> :gmd:citation z> :gmd:title z>]
   :date-creation [:gmd:identificationInfo z> :gmd:citation z>> :gmd:date :gco:Date 0]
   :date-publication [:gmd:identificationInfo z> :gmd:citation z>> :gmd:date :gco:Date 1]
   :date-revision [:gmd:identificationInfo z> :gmd:citation z>> :gmd:date :gco:Date 2]
   :temporal-begin [:gmd:identificationInfo z>> :gml:beginPosition]
   :temporal-end [:gmd:identificationInfo z>> :gml:endPosition]
   :abstract [:gmd:identificationInfo z>> :gmd:abstract z>]})


(defn- fill
  "Fill the string content into the element specificed by path pointed by
  the key in the given loc."
  [loc pk s]
  (let [lc (apply x1-> loc (pk paths))]
    (println (zip/node lc))
    (root-loc (edit-text lc s))))


(defn- get-recent-volc1
  [v1 v2]
  (let [parse-date #(ctf/parse (ctf/formatters :date) %)
        d1 (parse-date (get-in v1 [4 1]))
        d2 (parse-date (get-in v2 [4 1]))]
    (if (ctc/after? d1 d2) v1 v2)))

; Some product has multiple VolC1 entries (e.g. ISNK22AMMC) and have the same
; registration date. Hence their contents shall be merged.
(defn- merge-volc1
  "Merge two VolC1 records, Time Group, Content"
  [v1 v2]
  (let [tg1 (-> v1 (get-in [8 1]) (string/split #",") set)
        tg2 (-> v2 (get-in [8 1]) (string/split #",") set)
        tg (if (= tg1 tg2) nil (string/join "," (sort (cs/union tg1 tg2))))
        c1 (-> v1 (get-in [9 1]) (string/split #" ") set)
        c2 (-> v2 (get-in [9 1]) (string/split #" ") set)
        c (if (= c1 c2) nil (string/join "," (sort (cs/union c1 c2))))
        v1 (if (nil? tg) v1 (assoc-in v1 [8 1] tg))
        v1 (if (nil? c) v1 (assoc-in v1 [9 1] c))]
    v1))

(defn- cmp-volc1
  "Compare the date of VolC1 entries and return either the latest one
  or merge the two entries if they have the same dates."
  ([v] v)
  ([v1 v2]
   (let [d1 (get-in v1 [4 1])
         d2 (get-in v2 [4 1])]
     (if (= d1 d2)
       (merge-volc1 v1 v2)
       (get-recent-volc1 v1 v2)))))


(defn- tidy-volc1
  "Tidy up the VolC1 entries in the message to only have a single most recent VolC1 entry."
  [msg]
  (let [vs (:VolC1 msg)
        v (reduce cmp-volc1 vs)]
    (assoc (dissoc msg :VolC1) :VolC1 v)))


(def ^:private urn-pattern
  "The WMO standard urn pattern for GTS data"
  #"(?i)^urn:x-wmo:md:int.wmo.wis::([A-Z]{4}[0-9]{2}[A-Z]{4})$")

(defn- urn->ahl
  "Get the AHL code part out of the urn and validates the urn
  to ensure it is for GTS data."
  [urn]
  (second (re-find urn-pattern urn)))


(defn- ahl->parts
  "Get the different parts out of a AHL code"
  [ahl]
  {:T1 (.substring ahl 0 1)
   :T2 (.substring ahl 1 2)
   :A1A2 (.substring ahl 2 4)
   :ii (.substring ahl 4 6)
   :CCCC (.substring ahl 6)
   :TTAA (.substring ahl 0 4)})


(defn- get-data-type
  [msg]
  (let [msg-str (str msg)]
    (cond
     (re-find #"(?i)SYNOP" msg-str) "SYNOP reports"
     (re-find #"(?i)SPECI" msg-str) (get-in msg [:T2 0 1])
     :else (throw (RuntimeException. "Unknown data type (NYI?)")))))

(defn- get-data-format
  [msg]
  (let [msg-str (str msg)]
    (cond
     (re-find #"(?i)BUFR" msg-str) "BUFR"
     (re-find #"(?i)SPECI" msg-str) "SPECI"
     :else (throw (RuntimeException. "Unknown data format (NYI?)"))))
  )

(defn- msg->title
  [msg]
  (let [{:keys [TTAA ii CCCC]} (ahl->parts (:ahl msg))
        data-type (get-data-type msg)
        data-format (get-data-format msg)
        time-group (get-in msg [:VolC1 8 1])]
    (format "%s%s collection of %s available from %s as %s at Time %s"
            TTAA ii data-type CCCC data-format time-group)))


(defn- msg->date
  "Get the VolC1 date property"
  [msg]
  (get-in msg [:VolC1 4 1]))

(defn- msg->abstract
  [msg]
  (let [{:keys [T1 T2 A1A2 ii CCCC]} (-> msg :ahl ahl->parts)]
    ()


    ))

  (msg->abstract {:ahl "SPAU32AMMC" :T1 "something"})

  (defn draft->wcmp13
    "Create the WCMP 1.3 compatible metadata record using the given urn and
    dateStamp with the help of a template, AHL decoded message:

    1. Create the record using a WCMP13 template, elements are mostly empty.
    2. Fill the template with AHL decoded message.
    "
    [urn datestamp]
    (if-let [ahl (urn->ahl urn)]
      (let [msg (-> ahl info-ahl tidy-volc1)
            md template
            md (fill md :urn urn)
            md (fill md :datestamp datestamp)
            ; VolC1 Info
            md (fill md :title (msg->title msg))
            date-volc1 (msg->date msg)
            md (fill md :date-creation date-volc1)
            md (fill md :date-publication date-volc1)
            md (fill md :date-revision date-volc1)
            md (fill md :temporal-begin date-volc1)
            ]
        md)
      (throw (IllegalArgumentException.
              (format "%s is not compatible to WMO standard of GTS data." urn)))))



  ; This is the most critical function. Here is a list of elements that need to
  ; be filled by parsing the decoded message.
  ;
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:citation :gmd:CI_Citation :gmd:title :gco:CharacterString)
  ;
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:citation :gmd:CI_Citation :gmd:date :gmd:CI_Date :gmd:date :gco:Date)
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:citation :gmd:CI_Citation :gmd:date 1 :gmd:CI_Date :gmd:date :gco:Date)
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:citation :gmd:CI_Citation :gmd:date 2 :gmd:CI_Date :gmd:date :gco:Date)
  ;
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:abstract :gco:CharacterString)
  ;
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:resourceFormat :gmd:MD_Format :gmd:name :gco:CharacterString)
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:resourceFormat :gmd:MD_Format :gmd:version :gco:CharacterString)
  ;
  ; keyword of type "theme"
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:descriptiveKeywords :gmd:MD_Keywords :gmd:keyword :gco:CharacterString)
  ;
  ; WMOEssential and GTSPriority
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:resourceConstraints :gmd:MD_LegalConstraints :gmd:otherConstraints :gco:CharacterString)
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:resourceConstraints :gmd:MD_LegalConstraints :gmd:otherConstraints 1 :gco:CharacterString)
  ;
  ; climatologyMeteorologyAtmosphere
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:topicCategory :gmd:MD_TopicCategoryCode)
  ;
  ; west
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:extent :gmd:EX_Extent :gmd:geographicElement :gmd:EX_GeographicBoundingBox :gmd:westBoundLongitude :gco:Decimal)
  ; east
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:extent :gmd:EX_Extent :gmd:geographicElement :gmd:EX_GeographicBoundingBox :gmd:eastBoundLongitude :gco:Decimal)
  ; south
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:extent :gmd:EX_Extent :gmd:geographicElement :gmd:EX_GeographicBoundingBox :gmd:southBoundLatitude :gco:Decimal)
  ; north
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:extent :gmd:EX_Extent :gmd:geographicElement :gmd:EX_GeographicBoundingBox :gmd:northBoundLatitude :gco:Decimal)
  ;
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:extent :gmd:EX_Extent :gmd:temporalElement :gmd:EX_TemporalExtent :gmd:extent :gml:TimePeriod :gml:beginPosition)
  ; (:gmd:MD_Metadata :gmd:identificationInfo :gmd:MD_DataIdentification :gmd:extent :gmd:EX_Extent :gmd:temporalElement :gmd:EX_TemporalExtent :gmd:extent :gml:TimePeriod :gml:endPosition)
  ;
  ; (:gmd:MD_Metadata :gmd:distributionInfo :gmd:MD_Distribution :gmd:distributionFormat :gmd:MD_Format :gmd:name :gco:CharacterString)
  ; (:gmd:MD_Metadata :gmd:distributionInfo :gmd:MD_Distribution :gmd:distributionFormat :gmd:MD_Format :gmd:version :gco:CharacterString)
  ;

; This module takes a draft record (essentially just its urn and datestamp)
; and populate a WCMP 1.3 compatible metadata record with the decoded message
; of the AHL code.

(ns mdgen.draft
  (:use [mdgen.xml]
        [mdgen.core])
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]))


; The metadata template
(def template
  (-> "md_templates/wcmp13/gx.xml" io/resource io/file parse-file))


(defn fill
  [loc pth txt]
  (let [lc (apply x1-> loc pth)]
    (println (zip/node lc))
    (root-loc (edit-text lc txt))))

(def urn-pattern
  "The WMO standard urn pattern for GTS data"
  #"(?i)^urn:x-wmo:md:int.wmo.wis::([A-Z]{4}[0-9]{2}[A-Z]{4})$")

(defn urn->ahl
  "Get the AHL code part out of the urn. It also validates the urn."
  [urn]
  (second (re-find urn-pattern urn)))

;(info-ahl "SPAU32AMMC")

(def msg
  {"ii" [["Note" "See WMO-No.386 paragraph 2.3.2.2 for definition and use."]],
   "CCCC" [["Location Name" "Melbourne/World Met. Centre"] ["Country Name" "Australia"]],
   "T2" [["Data Type" "Special aviation weather reports"] ["Code Form" "FM 16 (SPECI)"]],
   "T1" [["Data Type" "Surface data"] ["Priority" "2/4"]],
   "A1" [["Country" "Australia"]],
   "VolC1" [[
             ["Region" 5]
             ["RTH" "MELBOURNE"]
             ["Country" "AUSTRALIA"]
             ["Centre" "MELBOURNE"]
             ["Date" "2013-10-21"]
             ["TTAAii" "SPAU32"]
             ["CCCC" "AMMC"]
             ["Code Form" "FM 16-X EXT."]
             ["Time Group" "AS REQUIRED"]
             ["Content" "YAMB YBCG YBHM YBMA YBRK YBRM YCIN YFRT YMAV YMHB YMLT YPEA YPGV YPKG YPPD YSCB YSDU YSNF YSRI YWLM"]
             ["Remarks" ""]]],
   "A2" [["Country" "Australia"]]
   :ahl "SPAU32AMMC"})

(defn- ahl->parts
  [ahl]
  {:T1 (.substring ahl 0 1)
   :T2 (.substring ahl 1 2)
   :A1A2 (.substring ahl 2 4)
   :ii (.substring ahl 4 6)
   :CCCC (.substring ahl 6)
   :TTAA (.substring ahl 0 4)})

(defn- msg->title
  [msg]
  (let [{:keys [TTAA CCCC]} (ahl->parts (:ahl msg))]
    (format "%s collection available from %s as %s" TTAA CCCC (get-in msg ["T1" 0 1]))))


(defn parse-decoded-msg
  "Takes a decoded message of AHL code and use it to fill various XML elements."
  [msg])


(def paths
  {:urn [:gmd:fileIdentifier z>]
   :datestamp [:gmd:dateStamp :gco:DateTime]
   :title [:gmd:identificationInfo z> :gmd:citation z> :gmd:title z>]
   :date-creation [:gmd:identificationInfo z> :gmd:citation z>> :gmd:date :gco:Date 0]
   :date-publication [:gmd:identificationInfo z> :gmd:citation z>> :gmd:date :gco:Date 1]
   :date-revision [:gmd:identificationInfo z> :gmd:citation z>> :gmd:date :gco:Date 2]})



(defn draft->wcmp13
  "Create the WCMP 1.3 compatible metadata record using the given urn and
  dateStamp with the help of a template, AHL decoded message:

  1. Create the record using a WCMP13 template, elements are mostly empty.
  2. Fill the template with AHL decoded message.
  "
  [urn datestamp]
  (if-let [ahl (urn->ahl urn)]
    (let [msg mdgen.draft/msg ;(info-ahl ahl)
          md template
          md (fill md (:urn paths) urn)
          md (fill md (:datestamp paths) datestamp)
          md (fill md (:title paths) (msg->title msg))
          ]
      md)))



(def md
  (draft->wcmp13 "urn:x-wmo:md:int.wmo.wis::SPAU32AMMC" "2013-10-21T00:00:00Z"))


(emit (zip/node md))

(:date-creation paths)

(path (apply x1-> template (:date-revision paths)))



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

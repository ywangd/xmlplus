; This module takes a draft record (essentially just its urn and datestamp)
; and convert it to WCMP 1.3 compatible metadata record with the help of an
; initial template, AHL decoded message, a reference record and a default
; reference record.

(ns mdgen.draft
  (:use [mdgen.xml]
        [mdgen.core])
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]))


; The reference metadata record
(def ref-md
  (-> "ref_md.xml" io/resource io/file parse-file))


; The default reference metadata record
(def default-ref
  (-> "md_default_refs/wcmp13/gx_au.xml" io/resource io/file parse-file))


;(info-ahl "SPAU32AMMC")

(def dmsg
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
   :code "SPAU32AMMC"})

(defn- code->parts
  [code]
  {:T1 (.substring code 0 1)
   :T2 (.substring code 1 2)
   :A1A2 (.substring code 2 4)
   :ii (.substring code 4 6)
   :CCCC (.substring code 6)
   :TTAA (.substring code 0 4)})

(defn- dmsg->title
  [dmsg]
  (let [{:keys [TTAA CCCC]} (code->parts (:code dmsg))]
    {'(:gmd:MD_Metadata
       :gmd:identificationInfo
       :gmd:MD_DataIdentification
       :gmd:citation
       :gmd:CI_Citation
       :gmd:title)
    (format "%s collection available from %s as %s" TTAA CCCC (get-in dmsg ["T1" 0 1]))}))

(dmsg->title dmsg)


(defn parse-decoded-msg
  "Takes a decoded message of AHL code and use it to fill various XML elements."
  [dmsg])


(defn draft->wcmp13
  "Create the WCMP 1.3 compatible metadata record using the given urn and
  dateStamp with the help of a template, AHL decoded message, a reference
  metadata record and a default reference metadata record. The steps of the
  processing is as follows:

  1. Create the record using a WCMP13 template, elements are mostly empty.
  2. Fill the template with AHL decoded message.
  3. Fill the template with relavant contents from the reference record.
  4. Fill the template with relavant contents from the default reference record.

  Note:
    1. The reference record should be chosen based on the type of the record, e.g.
  Surface, BUFR, Warning etc. The AHL code of the reference record should be close
  to the draft urn as much as possible.
    2. The default reference record can be a single for records from the same
  organisation or country.
  "
  [urn datestamp]
  {'(:gmd:MD_Metadata :gmd:fileIdentifier :gco:CharacterString) urn
   '(:gmd:MD_Metadata :gmd:dateStamp :gco:DateTime) datestamp}
  )

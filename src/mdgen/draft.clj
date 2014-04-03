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



(defn parse-ahl-msg
  "Takes a decoded message of AHL code and use it to fill various XML elements."
  [msg])


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
  [urn datestamp])

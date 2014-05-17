(ns mdgen.draft-test
  (:use [clojure.test]
        [mdgen xml core draft]))

; Simulate the result of (info-ahl "SPAU32AMMC")
(def msg1
  {:A2 [["Country" "Australia"]],
   :VolC1
   [[["Region" 5]
     ["RTH" "MELBOURNE"]
     ["Country" "AUSTRALIA"]
     ["Centre" "MELBOURNE"]
     ["Date" "2013-10-21"]
     ["TTAAii" "SPAU32"]
     ["CCCC" "AMMC"]
     ["Code Form" "FM 16-X EXT."]
     ["Time Group" "AS REQUIRED"]
     ["Content"
      "YAMB YBCG YBHM YBMA YBRK YBRM YCIN YFRT YMAV YMHB YMLT YPEA YPGV YPKG YPPD YSCB YSDU YSNF YSRI YWLM"]
     ["Remarks" ""]]],
   :A1 [["Country" "Australia"]],
   :T1 [["Data Type" "Surface data"] ["Priority" "2/4"]],
   :T2
   [["Data Type" "Special aviation weather reports"]
    ["Code Form" "FM 16 (SPECI)"]],
   :CCCC
   [["Location Name" "Melbourne/World Met. Centre"]
    ["Country Name" "Australia"]],
   :ii
   [["Note" "See WMO-No.386 paragraph 2.3.2.2 for definition and use."]],
   :ahl "SPAU32AMMC"})

(def msg2
  {:A2
   [["Geographical Area Designator" "180° - 90°E southern hemisphere"]],
   :VolC1
   [[["Region" 5]
     ["RTH" "MELBOURNE"]
     ["Country" "AUSTRALIA"]
     ["Centre" "MELBOURNE"]
     ["Date" "2007-02-28"]
     ["TTAAii" "ISNK22"]
     ["CCCC" "AMMC"]
     ["Code Form" "FM 94-XIII"]
     ["Time Group" "01,04,07,10,13,16,19,22"]
     ["Content"
      "94909 94915 94918 94919 94921 94922 94923 94925 94927 94928 94929 94937 94938 94939 94941 94942 95512 95520 95541 95570 95571 95692 95695 95697 95699 95705 95706 95707 95708 95709 95710 95713 95715 95716 95717 95718 95720 95721 95722 95723 95725 95726 95827"]
     ["Remarks" ""]]
    [["Region" 5]
     ["RTH" "MELBOURNE"]
     ["Country" "AUSTRALIA"]
     ["Centre" "MELBOURNE"]
     ["Date" "2007-02-28"]
     ["TTAAii" "ISNK22"]
     ["CCCC" "AMMC"]
     ["Code Form" "FM 94-XIII"]
     ["Time Group" "02,05,08,11,14,17,20,23"]
     ["Content"
      "94909 94915 94918 94919 94921 94922 94923 94925 94927 94928 94929 94937 94938 94939 94941 94942 95512 95520 95541 95570 95571 95692 95695 95697 95699 95705 95706 95707 95708 95709 95710 95713 95715 95716 95717 95718 95720 95721 95722 95723 95725 95726 95827"]
     ["Remarks" ""]]],
   :A1
   [["Data Type"
     "Synoptic observations from fixed land stations at non-standard time (i.e. 01, 02, 04, 05, ... UTC)"]
    ["TAC correspondence" "SYNOP (SNxx)"]
    ["Data Category Sub Category (Common Table C13)"
     "000/000  000/050"]],
   :T1
   [["Data Type" "Observational data (Binary coded) - BUFR"]
    ["Priority" "2"]],
   :T2 [["Data Type" "Surface/sea level"]],
   :CCCC
   [["Location Name" "Melbourne/World Met. Centre"]
    ["Country Name" "Australia"]],
   :ii
   [["Note" "See WMO-No.386 paragraph 2.3.2.2 for definition and use."]],
   :ahl "ISNK22AMMC"})


; Mock info-ahl without network usage
(defn- info-ahl*
  [x]
  msg1)

; Test with dynamic bindings
(binding [info-ahl info-ahl*]
  (def md
    (draft->wcmp13 "urn:x-wmo:md:int.wmo.wis::SPAU32AMMC" "2013-10-21T00:00:00Z")))

(deftest test-filled-elements
  (testing "fileIdentifier"
    (is (= (x1-> md z>> :gmd:fileIdentifier z> text)
           "urn:x-wmo:md:int.wmo.wis::SPAU32AMMC")))
  (testing "dateStamp"
    (is (= (x1-> md z>> :gmd:dateStamp z> text)
           "2013-10-21T00:00:00Z")))
  (testing "date-revision"
    (is (= (x1-> md z>> :gmd:citation z>> :gmd:CI_Date [(texts=* #"revision")] z>> :gco:Date text)
           "2013-10-21"))))


(run-tests *ns*)

(write-file "tmp-out.xml" md)


;(emit* md)





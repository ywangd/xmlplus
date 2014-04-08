(ns mdgen.draft-test
  (:use [mdgen xml core draft]))

; Simulate the result of (info-ahl "SPAU32AMMC")
(def msg1
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

(def msg (tidy-volc1 msg1))


; Mock info-ahl without network usage
(defn- info-ahl*
  [x]
  msg1)

; Test with dynamic bindings
(binding [info-ahl info-ahl*]
  (def md
    (draft->wcmp13 "urn:x-wmo:md:int.wmo.wis::SPAU32AMMC" "2013-10-21T00:00:00Z"))


  (emit* md)

  )

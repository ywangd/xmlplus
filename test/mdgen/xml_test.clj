(ns mdgen.xml-test
  (:use [clojure.test]
        [mdgen.xml])
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]))


(def l1 (parse-str "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<gmd:MD_Metadata xmlns:gco=\"http://www.isotc211.org/2005/gco\" xmlns:gmd=\"http://www.isotc211.org/2005/gmd\">
    <gmd:fileIdentifier>
        <gco:CharacterString>urn:x-wmo:md:int.wmo.wis::IOPL01AMMC</gco:CharacterString>
    </gmd:fileIdentifier>
    <gmd:identificationInfo>
        <gmd:MD_DataIdentification>
            <gmd:citation>
                <gmd:CI_Citation>
                    <gmd:title>
                        <gco:CharacterString>IOPL01 collection available from AMMC as BUFR</gco:CharacterString>
                    </gmd:title>
                    <gmd:date>
                        <gmd:CI_Date>
                            <gmd:date>
                                <gco:Date>2012-03-15</gco:Date>
                            </gmd:date>
                            <gmd:dateType>
                                <gmd:CI_DateTypeCode codeListValue=\"creation\">creation</gmd:CI_DateTypeCode>
                            </gmd:dateType>
                        </gmd:CI_Date>
                    </gmd:date>
                    <gmd:date>
                        <gmd:CI_Date>
                            <gmd:date>
                                <gco:Date>2012-06-15</gco:Date>
                            </gmd:date>
                            <gmd:dateType>
                                <gmd:CI_DateTypeCode codeListValue=\"publication\">publication</gmd:CI_DateTypeCode>
                            </gmd:dateType>
                        </gmd:CI_Date>
                    </gmd:date>
                </gmd:CI_Citation>
            </gmd:citation>
            <gmd:descriptiveKeywords>
                <gmd:MD_Keywords id=\"WMOCodeListKeywords\">
                    <gmd:keyword>
                        <gco:CharacterString>oceanography</gco:CharacterString>
                    </gmd:keyword>
                    <gmd:keyword>
                        <gco:CharacterString>meteorology</gco:CharacterString>
                    </gmd:keyword>
                    <gmd:type>
                        <gmd:MD_KeywordTypeCode codeListValue=\"theme\">theme</gmd:MD_KeywordTypeCode>
                    </gmd:type>
                </gmd:MD_Keywords>
            </gmd:descriptiveKeywords>
        </gmd:MD_DataIdentification>
    </gmd:identificationInfo>
</gmd:MD_Metadata>
"))

(deftest test-x->
  (testing "Return the location packed into a sequence if no filter is given"
           (is (= (first (x-> l1)) l1))))

(deftest test-x1->
  (testing "basic test for pure delegation to clojure.data.zip.xml/xml->"
           (is (= (first (x1-> l1 :gmd:fileIdentifier :gco:CharacterString))
                  {:tag :gco:CharacterString, :attrs nil, :content ["urn:x-wmo:md:int.wmo.wis::IOPL01AMMC"]} )))
  
  (testing "test for integer filter"
           (is (= (first (x1-> l1 :gmd:identificationInfo :gmd:MD_DataIdentification 
                               :gmd:citation :gmd:CI_Citation 
                               :gmd:date 1 :gmd:CI_Date :gmd:date :gco:Date))
                  {:tag :gco:Date :attrs nil, :content ["2012-06-15"]} )))
  
  (testing "Composite test for integer filter and clojure.data.zip filters"
           (is (= (x1-> l1 zf/descendants :gco:Date 1 zip/node)
                  {:tag :gco:Date, :attrs nil, :content ["2012-06-15"]}))))

(deftest test-text-node?
  (testing "True for a text node"
           (is (-> l1 zip/down zip/down text-node?)))
  (testing "False for a non-text node"
           (is (-> l1 zip/down text-node? not))))

(deftest test-edit-text
  (is (= (let [new-l1 (-> (edit-text (-> l1 zip/down zip/down) "CHANGED") zip/root zip/xml-zip)]
           (-> new-l1 zip/down zip/down first :content first))
         "CHANGED")))

(deftest test-move-node
  (testing "basic test for moving node"
           (is (let [floc (-> l1 zip/down)
                     tloc (-> l1 zip/down zip/right zip/down)
                     node (zip/node floc)]
                 (= (-> (move-node floc tloc 1) zip/root zip/xml-zip zip/down zip/down zip/down zip/right zip/node) node))))
  (testing "floc must not be ancestor of tloc"
           (is (thrown? IllegalArgumentException
                        (let [floc (-> l1 zip/down zip/right zip/down zip/down)
                              tloc (-> floc zip/down zip/down zip/right)]
                          (move-node floc tloc))))))

(deftest test-rpath
  (testing "Test for integer filter"
           (is (= (rpath (x1-> l1 zf/descendants :gco:Date 1))
                  '(:gmd:identificationInfo 
                     :gmd:MD_DataIdentification
                     :gmd:citation 
                     :gmd:CI_Citation
                     :gmd:date 
                     1 
                     :gmd:CI_Date 
                     :gmd:date 
                     :gco:Date))))

  (testing "Result from rpath can be used to get the node back via x1->"
         (is (= (zip/node (apply x1-> l1 (rpath (x1-> l1 zf/descendants :gco:Date 1))))
                {:tag :gco:Date, :attrs nil, :content ["2012-06-15"]})))

  (testing "Test the :include-root keyword"
           (is (= (cons :gmd:MD_Metadata (rpath (x1-> l1 zf/descendants :gco:Date 1)))
                  (rpath (x1-> l1 zf/descendants :gco:Date 1) :include-root true)))))
  
(deftest test-text=
  (testing "text= should only test for text directly belong to a node"
           (is (= (x1-> l1 zf/descendants (text= "2012-06-15") first)
                  {:tag :gco:Date, :attrs nil, :content ["2012-06-15"]})))
  (testing "shortcut for text="
           (is (= (x1-> l1 zf/descendants "2012-06-15" first)
                  {:tag :gco:Date, :attrs nil, :content ["2012-06-15"]}))))

(deftest test-texts=
  (is (= (x1-> l1 zf/descendants (texts= "2012-06-15") first)
         {:tag :gmd:date, 
          :attrs nil, 
          :content [{:tag :gco:Date, 
                     :attrs nil, 
                     :content ["2012-06-15"]}]})))
  
  
  

(ns xmlplus.xml-test
  (:use [clojure.test]
        [xmlplus.xml])
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
                    <gmd:keyword>
                        <gco:CharacterString></gco:CharacterString>
                    </gmd:keyword>
                    <gmd:type>
                        <gmd:MD_KeywordTypeCode codeListValue=\"theme\">theme</gmd:MD_KeywordTypeCode>
                    </gmd:type>
                </gmd:MD_Keywords>
            </gmd:descriptiveKeywords>
        </gmd:MD_DataIdentification>
    </gmd:identificationInfo>
    <gmd:metadataExtensionInfo gco:nilReason=\"missing\"/>
</gmd:MD_Metadata>
"))

(deftest test-x->
  (testing "Return the location packed into a sequence if no filter is given"
           (is (= (first (x-> l1)) l1))))

(deftest test-x1->
  (testing "basic test for pure delegation to clojure.data.zip.xml/xml->"
    (is (= (first (x1-> l1 :gmd:fileIdentifier :gco:CharacterString))
           {:tag :gco:CharacterString, :attrs nil, :content ["urn:x-wmo:md:int.wmo.wis::IOPL01AMMC"]})))

  (testing "test for integer filter"
    (is (= (first (x1-> l1 :gmd:identificationInfo :gmd:MD_DataIdentification
                        :gmd:citation :gmd:CI_Citation
                        :gmd:date 1 :gmd:CI_Date :gmd:date :gco:Date))
           {:tag :gco:Date :attrs nil, :content ["2012-06-15"]} )))

  (testing "Composite test for integer filter and clojure.data.zip filters"
    (is (= (x1-> l1 zf/descendants :gco:Date 1 zip/node)
           {:tag :gco:Date, :attrs nil, :content ["2012-06-15"]}))))

(deftest test-x1*->
  (testing "basic test to make sure it does starts from the root node"
    (is (= (first (x1->* l1 :gmd:MD_Metadata :gmd:fileIdentifier :gco:CharacterString))
           {:tag :gco:CharacterString, :attrs nil, :content ["urn:x-wmo:md:int.wmo.wis::IOPL01AMMC"]}))))

(deftest test-text-node?
  (testing "True for a text node"
           (is (-> l1 zip/down zip/down text-node?)))
  (testing "False for a non-text node"
           (is (-> l1 zip/down text-node? not)))
  (testing "Test :pure keyword"
           (is (let [loc (edit-text l1 "some text" :append true)]
                 (and (text-node? loc) (not (text-node? loc :pure true)))))))


(deftest test-not-filled?
  (testing "The gmd:metadataExtensionInfo is returned when search only for empty-node?
    The empty-node? function can be achieved by calling not-filled without arguments"
    (is (= (path (second (x-> l1 zf/descendants (not-filled?))))
           '(:gmd:metadataExtensionInfo))))
  (testing "gmd:metadataExtensionInfo is not returned if search for not-filled? node"
    (is (= (path (x1-> l1 zf/descendants (not-filled? [:gco:nilReason "missing"])))
           '(:gmd:identificationInfo
             :gmd:MD_DataIdentification
             :gmd:descriptiveKeywords
             :gmd:MD_Keywords
             :gmd:keyword
             2
             :gco:CharacterString))))
  (testing "Testing not-filled? works for both attr name and attr name/value pair"
    (is (= (path (x1-> l1 zf/descendants (not-filled? :gco:nilReason)))
           '(:gmd:identificationInfo
             :gmd:MD_DataIdentification
             :gmd:descriptiveKeywords
             :gmd:MD_Keywords
             :gmd:keyword
             2
             :gco:CharacterString)))))

(deftest test-edit-attrs
  (is (= (zfx/attr (x1-> (root-loc (edit-attrs (x1-> l1 zf/descendants :gco:Date 1) {:alias "something"})) zf/descendants :gco:Date 1) :alias)
         "something")))

(deftest test-edit-text
  (is (= (let [new-l1 (-> (edit-text (-> l1 zip/down zip/down) "CHANGED") zip/root zip/xml-zip)]
           (-> new-l1 zip/down zip/down first :content first))
         "CHANGED")))

(deftest test-move-node
  (testing "basic test for moving node"
           (is (let [floc (-> l1 zip/down)
                     tloc (-> l1 zip/down zip/right zip/down)
                     node (zip/node floc)]
                 (= (-> (move-node floc tloc 1)
                        zip/root zip/xml-zip zip/down zip/down zip/down zip/right zip/node)
                    node))))
  (testing "floc must not be ancestor of tloc"
           (is (thrown? IllegalArgumentException
                        (let [floc (-> l1 zip/down zip/right zip/down zip/down)
                              tloc (-> floc zip/down zip/down zip/right)]
                          (move-node floc tloc))))))

(deftest test-path
  (testing "Test for integer filter"
           (is (= (path (x1-> l1 zf/descendants :gco:Date 1))
                  '(:gmd:identificationInfo
                     :gmd:MD_DataIdentification
                     :gmd:citation
                     :gmd:CI_Citation
                     :gmd:date
                     1
                     :gmd:CI_Date
                     :gmd:date
                     :gco:Date))))

  (testing "Result from path can be used to get the node back via x1->"
         (is (= (zip/node (apply x1-> l1 (path (x1-> l1 zf/descendants :gco:Date 1))))
                {:tag :gco:Date, :attrs nil, :content ["2012-06-15"]})))

  (testing "Test the :include-root keyword"
           (is (= (cons :gmd:MD_Metadata (path (x1-> l1 zf/descendants :gco:Date 1)))
                  (path (x1-> l1 zf/descendants :gco:Date 1) :include-root true)))))

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

(deftest test-texts=*
  (is (= (x1-> l1 zf/descendants (texts=* #"^2012-06-[0-9]{2}$") first)
         {:tag :gmd:date,
          :attrs nil,
          :content [{:tag :gco:Date,
                     :attrs nil,
                     :content ["2012-06-15"]}]})))

(deftest test-text=*
  (is (= (x1-> l1 zf/descendants (text=* #"2012-06-[0-9]{2}") first)
         {:tag :gco:Date,
          :attrs nil,
          :content ["2012-06-15"]})))


(deftest test-convenient-func
  (testing "z>>"
    (is (= (x1-> l1 z>> :gmd:title)
           (x1-> l1 zf/descendants :gmd:title))))
  (testing "z>"
    (is (= (x1-> l1 z>)
           (x1-> l1 zf/children-auto)))))

(run-tests *ns*)





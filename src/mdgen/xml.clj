(ns mdgen.xml
 (:require [clojure.zip :as zip]
           [clojure.xml :as xml]
           [clojure.java.io :as io]
           [clojure.data.zip :as zf]
           [clojure.data.zip.xml :as zfx]))

(defn read->root
  "Read a given xml file and return the root node"
  [fname]
  (with-open [ins (io/input-stream fname)]
    (xml/parse ins)))


(defn get-node
  "Return a lazy seq of matching nodes"
  ([node tag]
	  (for [x (xml-seq node) :when (= tag (:tag x))]
	    x))
  ([node tag val]
    (for [x (xml-seq node) 
          :when (and (= tag (:tag x)) (= val (-> x :content first)))]
      x)))

(defn query
  "Similar to xml-> but takes a node instead of loc. Returns a lazy seq."
  [node & args]
  (apply zfx/xml-> (zip/xml-zip node) args))

(defn query1
  "Take the first element from the query result"
  [& args]
  (first (apply query args)))

(defn edit
  [root & args]
  (zip/root (zip/edit (apply query1 root args) #(assoc % :content ["Changed"])))
)

(defmacro x->
  [loc & args]
  `(loop [res# ~loc preds# '~args] 
     (let [pred# (first preds#)]
       (println pred# (rest preds#))
       (if (= nil pred#) res#)
       (cond
         (= nil pred#) res#
         (= (type pred#) 'java.lang.Long) (recur (first res#) (rest preds#))
         :else (recur (mapcat #(zfx/xml-> % pred#) res#) (rest preds#))
         )       
     )
   )
)
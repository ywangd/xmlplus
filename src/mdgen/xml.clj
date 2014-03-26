(ns mdgen.xml
 (:require [clojure.zip :as zip]
           [clojure.xml :as xml]
           [clojure.java.io :as io]
           [clojure.data.zip :as zf]
           [clojure.data.zip.xml :as zfx]))


(defn parse-file
  "Parse a given xml file and return location of the root"
  [fname]
  (zip/xml-zip 
    (with-open [ins (io/input-stream fname)]
      (xml/parse ins))))

(defn parse-str
  "Parse a given xml string and return location of the root"
  [s]
  (zip/xml-zip 
    (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))


(defn x->
  "Similar to clojure.data.zip/xml-> with the ability to take integer as index filter
   Note this function always returns a lazy seq even when index filter is used."
  [loc & args]
  (loop [res loc preds args]
    (let [pred (first preds)]
      (cond
        (nil? pred) res
        (number? pred) 
          (recur (->> res (drop pred) (take 1)) (rest preds))
        (vector? res)
          (recur (zfx/xml-> res pred) (rest preds))
        :else (recur (mapcat #(zfx/xml-> % pred) res) (rest preds))))))

(defn x1->
  "Similar to x-> but guarantee to return a single location."
  [loc & args]
  (first (apply x-> loc args)))

(defn rpath
  "Get the complete path to root node from given loc. 
   The path can be used as arguments to x-> and get the loc back."
  [loc]
  (flatten
    (for [ancestor (-> (zf/ancestors loc) butlast reverse)]
      (do 
        (let [tag (-> ancestor first :tag)
              left (-> ancestor second :l)
              cnt (count (filter #(= tag (:tag %)) left))
              ]
          (if (> cnt 0)
            [tag cnt]
            tag))))))
          
(defn ed-text
  "Edit text of the node at given location"
  [loc text & {:as opts}]
  (let [append? (:append opts false)]
    (zip/edit loc #(assoc % :content %2) 
              (if append? 
                (conj (-> loc first :content) text)
                (vector text)))))
  
(defn insert-child
  "Insert a child node at the given location based on the given position.
   Without moving the location."
  ([loc node pos]
    (let [children (zf/children loc)
          nchildren (count children)]
      (cond 
        (or (= pos :last) (>= pos nchildren)) (zip/append-child loc node)
        (zero? pos) (zip/insert-child loc node)
        :else (let [child (first (drop pos children))]
                (zip/up (zip/insert-left child node))))))
  ([loc node]
    (insert-child loc node 0)))

(defn insert-left
  "Insert a node to the left on the given location"
  [loc node]
  (zip/insert-left loc node))

(defn insert-right
  "Insert a node to the right on the given location"
  [loc node]
  (zip/insert-right loc node))

(defn make-node
  "Make an node based on given tag, attrs and content"
  [tag attrs content]
  (struct xml/element tag attrs
          (if (vector? content) 
            content 
            (vector content))))

(defn insert-parent
  "1. Create a node with given tag and attrs. 
   2. Insert this node as parent at the given location.
   3. Returns the location of the new parent node."
  [loc tag attrs]
  (let [p (make-node tag attrs (zip/node loc))]
    (zip/replace loc p)))

(defn move-node
  "Move the node at given location as a child node at the second given location.
   return the location moved node."
  ([floc tloc pos]
    (let [node (zip/node floc)
          tloc (insert-child tloc node pos)]
      (zip/remove floc)
      (x1-> tloc zf/children pos)))
  ([floc tloc]
    (move-node floc tloc 0)))
  

(defn emit-element 
  "Modified version of emit-element from clojure.xml. 
   1. No EOLs are added into the tags surrounding text.
   2. Indent properly."
  [e lev]
  (let [lead (apply str (repeat (* lev 4) \space))]
    (if (string? e)
      (print e)
      (do
        (let [tag (name (:tag e)) 
              nottxt (not (every? string? (:content e)))]
          (print (str lead "<" tag))
          (when (:attrs e)
            (doseq [attr (:attrs e)]
              (print (str " " (name (key attr)) "=\"" (val attr) \"))))
          (if (:content e)
            (do
              (print ">")
              (if nottxt (print "\n"))
              (doseq [c (:content e)]
                (emit-element c (+ lev 1)))
              (if nottxt 
                (print (str lead "</" tag ">\n")) 
                (print (str "</" tag ">\n"))))
            (print "/>\n")))))))

(defn emit
  "Modified version of emit from clojure.xml."
  [node]
  (println "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
  (emit-element node 0))

(defn write-file
  "Get the root node of the given loc and write out to the given file"
  [fname loc]
  (spit fname (with-out-str (emit (zip/root loc)))))
  


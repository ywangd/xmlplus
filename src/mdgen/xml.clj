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

(defn read->loc
  "Similar to read->root but return the location (zipper) of root node"
  [fname]
  (zip/xml-zip (read->root fname)))

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

(defn ed-text
  "Edit text of the node at given location"
  [loc text & {:as opts}]
  (let [append? (:append opts false)]
    (zip/edit loc #(assoc % :content %2) 
              (if append? 
                (conj (-> loc first :content) text)
                (vector text)))))
  
(defn insert-child
  "Insert a child node at the given location based on the given position."
  ([loc node pos]
    (let [children (zf/children loc)
          nchildren (count children)]
      (cond 
        (or (= pos :last) (>= pos nchildren)) (zip/append-child loc node)
        (zero? pos) (zip/insert-child loc node)
        :else (let [child (first (drop pos children))]
                (zip/insert-right child node)
                loc))))
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
  


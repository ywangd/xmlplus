(ns mdgen.xml
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.data.zip :as zf]
            [clojure.data.zip.xml :as zfx]))


(defn parse-file
  "Parse a given xml file and return location of the root"
  [file]
  (zip/xml-zip
    (with-open [ins (io/input-stream file)]
      (xml/parse ins))))

(defn parse-str
  "Parse a given xml string and return location of the root"
  [^String s]
  (zip/xml-zip
    (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))

(defn root-loc
  "Get the root location of the given loc."
  [loc]
  (-> loc zip/root zip/xml-zip))

(defn texts
  "This function get texts from all descendant nodes at given location
  and return them as a lazy seq. Note this is different from
  clojure.data.zip.xml/text which returns a single concatenated string."
  [loc]
  (zfx/xml-> loc zf/descendants zip/node string?))

(defn texts=
  "Return a function to test whether given text is equal to the concatenation
  of all the texts under the given node."
  [s]
  (fn [loc] (= (apply str (texts loc)) s)))

(defn text
  "This function get only text directly belong to the node at given location"
  [loc]
  (apply str (filter string? (-> loc first :content))))

(defn text=
  "Predicate function to test if text directly belong to the given node is
   equal to the given text."
  [s]
  (fn [loc] (= (text loc) s)))

(defn x->
  "Similar to clojure.data.zip/xml-> with the ability to take integer
  as index filter. Note this function always returns a lazy seq even
  when index filter is used."
  [loc & args]
  (loop [res loc preds args]
    (let [pred (first preds)
          pred (if (string? pred) (text= pred) pred)]
      (cond
        (nil? pred) (if (seq? res) res (lazy-seq (conj () res)))
        (number? pred)
          (recur (->> res (drop pred) (take 1)) (rest preds))
        (vector? res) ; single res of location type (vector)
          (recur (zfx/xml-> res pred) (rest preds))
        ; lazy-seq of multiple res
        :else (recur (mapcat #(zfx/xml-> % pred) res) (rest preds))))))

(defn x1->
  "Similar to x-> but guarantee to return a single location."
  [loc & args]
  (first (apply x-> loc args)))

(defn text-node?
  "A node is a text node if some of its contents is string.
  A more strict definition of text node requires all of its
  contents are strings. This behaviour can be turned on by
  setting the :pure keyword to true."
  [loc & {:as opts}]
  (let [pure (:pure opts false)
        content (get-in loc [0 :content])]
    (boolean ((if pure every? some) string? content))))

(defn rpath
  "Get the complete path to root node from given loc.
  The path can be used as arguments to x-> and get the loc back.
  By default, the path is composed by tags from root to the node at
  given location, but exclude the root node tag. This is because
  clojure.data.zip.xml/xml-> function does not require the root tag
  as its arguments.
  To change the default behaviour and include the root-tag, pass
  :include-root true to the function."
  [loc & {:as opts}]
  (flatten
    (for [ancestor (-> (zf/ancestors loc) ((if (:include-root opts) identity butlast)) reverse)]
      (do
        (let [tag (-> ancestor first :tag)
              left (-> ancestor second :l)
              cnt (count (filter #(= tag (:tag %)) left))]
          (if (> cnt 0) [tag cnt] tag))))))

(defn edit-tag
  "Edit the tag of the node at given location."
  [loc tag]
  (zip/edit loc #(assoc % :tag tag)))

(defn edit-text
  "Edit text of the node at given location"
  [loc text & {:as opts}]
  (let [append? (:append opts false)
        ct (if append? (conj (-> loc first :content) text) (vector text))]
    (zip/edit loc #(assoc % :content ct))))

(defn edit-attrs
  "Edit the attrs of the node at given location.
  :dissoc [] to remove existing attrs."
  [loc attrs & {:as opts}]
  (let [ex-attrs (-> loc first :attrs)
        loc (zip/edit loc #(assoc % :attrs (merge ex-attrs attrs)))
        attrs (-> loc first :attrs)
        dis (:dissoc opts nil)]
    (if dis
      (zip/edit loc #(assoc % :attrs (apply dissoc attrs dis)))
      loc)))

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

(defn- f-t-path
  "Make sure floc is not a anscentor of tloc. Otherwise it creates a infinite loop."
  [floc tloc]
  (let [fpath (rpath floc)
        tpath (rpath tloc)
        flen (count fpath)]
    (if (= fpath (take flen tpath)) nil [fpath tpath])))

(defn move-node
  "Move the node at given location as a child node at the second given location.
   return the location of the moved node."
  ([floc tloc pos]
    (let [node (zip/node floc)
          [fpath tpath] (f-t-path floc tloc)
          valid? (boolean fpath)]
      (if valid?
        (let [trz (-> tloc root-loc)
              trz-del (-> (apply x1-> trz fpath) zip/remove root-loc)
              tloc (insert-child (apply x1-> trz-del tpath) node pos)]
          (x1-> tloc zf/children-auto pos))
        (throw (IllegalArgumentException. ": floc cannot be ancestor of tloc.\n")) )))
  ([floc tloc]
    (move-node floc tloc 0)))

(defn copy-node
  "copy the node at given location as a child node at the second given location.
  return the location of the copied node"
  ([floc tloc pos]
    (let [node (zip/node floc)
          loc (insert-child tloc node pos)]
      (x1-> loc (:tag node) pos)))
  ([floc tloc]
    (copy-node floc tloc 0)))

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



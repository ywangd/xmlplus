# XML Plus

Add more functionalities to Clojure's builtin clojure.xml and contrib package
clojure.data.zip.xml. Most notably add the support for using integers to select
a single element in a multiple match.


## Usage
```clojure
(use 'xmlplus.xml)

(def loc (parse-str "<a><b></b><b></b></a>"))

(x1-> loc :b 1)
```

## License


(ns pdf-string-replacer.core
  (:gen-class) 
  (:import org.apache.pdfbox.pdfparser.PDFStreamParser
           org.apache.pdfbox.contentstream.operator.Operator
           org.apache.pdfbox.cos.COSString
           org.apache.pdfbox.pdmodel.common.PDStream
           org.apache.pdfbox.pdfwriter.ContentStreamWriter
           org.apache.pdfbox.cos.COSString))

(require '[pdfboxing.common :as common]
         '[clojure.string :refer [replace-first]])

(def search-string "old")
(def replace-string "new")

(defn get-token  [parser]
  do (.parse parser) (.getTokens parser))

(defn replace-string-when-lower-tj [indexed-tokens idx]
  (let [[_ previous] (nth indexed-tokens (- idx 1))
        old-string (.getString previous)
        new-string (replace-first old-string search-string replace-string)]
    (.setValue previous (.getBytes new-string))))

(defn replace-string-when-upper-tj [indexed-tokens idx]
  (let [[_ previous] (nth indexed-tokens (- idx 1))]
    (doseq [x previous]
      (if (instance? COSString x)
        (do (println x)
            (let [old-string (.getString x)
                  new-string (replace-first old-string search-string replace-string)]
              (.setValue x (.getBytes new-string))))))))

(defn set-value-if-string [indexed-tokens indexed-token]
  (let [[idx token] indexed-token]
    (if (instance? Operator token)
      (cond
        (= "Tj" (.getName token)) (replace-string-when-lower-tj indexed-tokens idx)
        (= "TJ" (.getName token)) (replace-string-when-upper-tj indexed-tokens idx)))))


(defn write-token [token-and-page pdf]
  (let [[tokens page] token-and-page
        upstream (PDStream. pdf)
        out (.createOutputStream upstream)
        writer (ContentStreamWriter. out)]
    (.writeTokens writer tokens)
    (.setContents page upstream)
    (.close out)))


(defn Replace-string-in-pdf [pdf]
  (let [pages (iterator-seq (.iterator (.. pdf (getDocumentCatalog) (getPages))))
        parsers (map #(PDFStreamParser. %) pages)
        tokens-each-parser (map get-token parsers)
        indexed-tokens (map #(map-indexed vector %) tokens-each-parser)]

    (doseq [s indexed-tokens]
      (doseq [i s]
        (set-value-if-string s i)))
    (doseq [s (zipmap tokens-each-parser pages)]
      (write-token s pdf)))
  nil)

(defn -main
  [input-pdf output-pdf]
  (let
   [pdf  (pdfboxing.common/obtain-document input-pdf)]
    (Replace-string-in-pdf pdf)
    (.save pdf output-pdf))
  nil)

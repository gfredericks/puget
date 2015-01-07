(ns puget.data
  "Code to handle structured data, usually represented as EDN."
  (:require
    [clojure.data.codec.base64 :as b64])
  (:import
    (java.net URI)
    (java.util Date TimeZone UUID)))


;; TAGGED VALUE PROTOCOL

(defmulti -tagged-value
  "Returns a pair [tag-symbol value]."
  type)
(defn tagged-value? [x]
  (boolean (get-method -tagged-value (type x))))

(defn edn-tag [x] ((-tagged-value x) 0))
(defn edn-value [x] ((-tagged-value x) 1))

(defn edn-str
  "Converts the given TaggedValue data to a tagged EDN string."
  ^String
  [v]
  (str \# (edn-tag v) \space (pr-str (edn-value v))))



;; EXTENSION FUNCTIONS

(defmacro defprint-method
  "Defines a print-method for the given class which writes out the EDN
  serialization from `edn-str`."
  [t]
  `(defmethod print-method ~t
     [v# ^java.io.Writer w#]
     (.write w# (edn-str v#))))


(defmacro extend-tagged-value
  "Extends the TaggedValue protocol with implementations which return the
  given symbol as the tag and use the given expression to calculate the value.
  The expression should resolve to a function which accepts one argument and
  returns the serialized value. This macro also defines a print-method which
  delegates to edn-str."
  [t tag expr]
  `(let [value-fn# ~expr]
     (defmethod -tagged-value ~t [this#] [~tag (value-fn# this#)])
     (defprint-method ~t)))


(defmacro extend-tagged-str
  [c tag]
  `(extend-tagged-value ~c ~tag str))


(defmacro extend-tagged-map
  [c tag]
  `(extend-tagged-value ~c ~tag
     (comp (partial into {}) seq)))



;; BUILT-IN EDN TAGS

; #inst - Date-time instant as an ISO-8601 string.
(defn- format-utc
  "Produces an ISO-8601 formatted date-time string from the given Date."
  [^Date date]
  (->
    "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00"
    java.text.SimpleDateFormat.
    (doto (.setTimeZone (TimeZone/getTimeZone "GMT")))
    (.format date)))


(extend-tagged-value Date 'inst format-utc)


; #uuid - Universally-unique identifier string.
(extend-tagged-str UUID 'uuid)


; #bin - Binary data in the form of byte arrays.
(extend-tagged-value
  (class (byte-array 0)) 'bin
  #(->> % b64/encode (map char) (apply str)))


(defn read-bin
  "Reads a base64-encoded string into a byte array."
  ^bytes
  [^String bin]
  (b64/decode (.getBytes bin)))


; #uri - Universal Resource Identifier string.
(extend-tagged-str URI 'uri)


(defn read-uri
  "Constructs a URI from a string value."
  ^URI
  [^String uri]
  (URI. uri))


; #??? - default handling function
(defrecord GenericTaggedValue
  [tag value])

(defmethod -tagged-value GenericTaggedValue
  [x]
  [(:tag x) (:value x)])


(defprint-method GenericTaggedValue)


(defn tagged-value
  "Creates a generic tagged value record to represent some EDN value. This is
  suitable for use as a default-data-reader function."
  [tag value]
  {:pre [(symbol? tag)]}
  (->GenericTaggedValue tag value))

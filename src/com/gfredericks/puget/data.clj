(ns com.gfredericks.puget.data
  "Code to handle custom data represented as tagged EDN values."
  (:import
    (java.net URI)
    (java.util Date TimeZone UUID)))


(defprotocol ExtendedNotation
  "Protocol for types which use extended notation for EDN representation."

  (->edn
    [value]
    "Converts the given value into a tagged value representation for EDN
    serialization. Should return a value with a `:tag` and a `:form`."))



;; ## Extension Functions

(defmacro extend-tagged-value
  "Defines an EDN representation for a type `t`. The tag will be the symbol
  given for `tag` and the literal form will be generated by applying `expr` to
  the original value."
  [t tag expr]
  `(let [value-fn# ~expr]
     (extend-type ~t
       ExtendedNotation
       (->edn
         [this#]
         (array-map :tag ~tag :form (value-fn# this#))))))


(defmacro extend-tagged-str
  "Defines an EDN representation for the given type by converting it to a
  string form."
  [t tag]
  `(extend-tagged-value ~t ~tag str))


(defmacro extend-tagged-map
  "Defines an EDN representation for the given type by converting it to a
  map form."
  [t tag]
  `(extend-tagged-value ~t ~tag
     (comp (partial into {}) seq)))



;; ## Standard EDN Types

(defn- format-utc
  "Produces an ISO-8601 formatted date-time string from the given Date."
  [^Date date]
  (-> "yyyy-MM-dd'T'HH:mm:ss.SSS-00:00"
      java.text.SimpleDateFormat.
      (doto (.setTimeZone (TimeZone/getTimeZone "GMT")))
      (.format date)))


;; `inst` tags a date-time instant represented as an ISO-8601 string.
(extend-tagged-value Date 'inst format-utc)


;; `uuid` tags a universally-unique identifier string.
(extend-tagged-str UUID 'uuid)

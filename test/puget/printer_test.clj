(ns puget.printer-test
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    (puget
      [data :refer [TaggedValue]]
      [printer :refer :all])))


(defn- should-fail-when-strict
  [value]
  (with-options {:strict true}
    (is (thrown? IllegalArgumentException (pprint value))
        "should not print non-EDN representation")))


(deftest color-scheme-setting
  (let [old-scheme (:color-scheme *options*)]
    (set-color-scheme! {:tag [:green]})
    (is (= [:green] (:tag (:color-scheme *options*))))
    (set-color-scheme! :nil [:black] :number [:bold :cyan])
    (is (= [:black] (:nil (:color-scheme *options*))))
    (is (= [:bold :cyan] (:number (:color-scheme *options*))))
    (set-color-scheme! old-scheme)))


(deftest canonical-primitives
  (testing "Primitive values"
    (are [v text] (= text (-> v pprint with-out-str str/trim))
         nil     "nil"
         true    "true"
         false   "false"
         0       "0"
         1234N   "1234N"
         2.718   "2.718"
         3.14M   "3.14M"
         3/10    "3/10"
         \a      "\\a"
         \space  "\\space"
         "foo"   "\"foo\""
         :key    ":key"
         :ns/key ":ns/key"
         'sym    "sym"
         'ns/sym "ns/sym")))


(deftest canonical-collections
  (testing "Collections"
    (are [v text] (= text (pprint-str v))
         '(foo :bar)            "(foo :bar)"
         '(1 2 3)               "(1 2 3)"
         [4 "five" 6.0]         "[4 \"five\" 6.0]"
         {:foo 8 :bar 'baz}     "{:bar baz, :foo 8}" ; gets sorted
         #{:omega :alpha :beta} "#{:alpha :beta :omega}")) ; also sorted
  (testing "Map collection separator"
    (is (= "{:bar\n [:a :b]}" (pprint-str {:bar [:a :b]} {:width 10, :map-coll-separator :line})))))


(deftest unsorted-keys
  (testing "Unsorted collection keys"
    (with-options {:sort-keys false}
      (is (= "#{:zeta :book}" (pprint-str (set [:zeta :book]))))
      (is (= "{:9 x, :2 y}" (pprint-str (array-map :9 'x, :2 'y)))))
    (with-options {:sort-keys 2}
      (is (= "{:a 1, :b 0}" (pprint-str (array-map :b 0 :a 1))))
      (is (= "{:z 2, :a 5, :m 8}" (pprint-str (array-map :z 2 :a 5 :m 8)))))))


(defrecord TestRecord [foo bar])

(deftest canonical-records
  (testing "Records"
    (let [r (->TestRecord \x \y)]
      (should-fail-when-strict r)
      (is (= "#puget.printer_test.TestRecord{:bar \\y, :foo \\x}\n"
             (with-out-str (pprint r)))))))


(deftype ADeref []
  clojure.lang.IDeref
  (deref [this] 123))

(deftype APending []
  clojure.lang.IPending
  (isRealized [this] false))

(deftest clojure-types
  (testing "seq"
    (is (= "()" (pprint-str (lazy-seq)))))
  (testing "regex"
    (let [v #"\d+"]
      (should-fail-when-strict v)
      (is (= "#\"\\d+\"" (pprint-str v)))))
  (testing "vars"
    (let [v #'TaggedValue]
      (should-fail-when-strict v)
      (is (= "#'puget.data/TaggedValue"
             (pprint-str v)))))
  (testing "atom"
    (let [v (atom :foo)]
      (should-fail-when-strict v)
      (is (re-seq #"#<Atom@[0-9a-f]+ :foo>" (pprint-str v)))))
  (testing "delay"
    (let [v (delay (+ 8 14))]
      (should-fail-when-strict v)
      (is (re-seq #"#<Delay@[0-9a-f]+ pending>" (pprint-str v)))
      (is (= 22 @v))
      (is (re-seq #"#<Delay@[0-9a-f]+ 22>" (pprint-str v)))))
  (testing "future"
    (let [v (future (do (Thread/sleep 100) :done))]
      (should-fail-when-strict v)
      (is (re-seq #"#<Future@[0-9a-f]+ pending>" (pprint-str v)))
      (is (= :done @v))
      (is (re-seq #"#<Future@[0-9a-f]+ :done>" (pprint-str v)))))
  (testing "custom IDeref"
    (let [v (ADeref.)]
      (should-fail-when-strict v)
      (is (re-seq #"#<puget.printer_test.ADeref@[0-9a-f]+ 123>"
                  (pprint-str v)))))
  (testing "custom IPending"
    (let [v (APending.)]
      (should-fail-when-strict v)
      (is (re-seq #"#<puget.printer_test.APending@[0-9a-f]+ pending"
                  (pprint-str v))))))


(deftest canonical-tagged-value
  (let [tv (reify TaggedValue
             (edn-tag [this] 'foo)
             (edn-value [this] :bar/baz))]
    (is (= "#foo :bar/baz" (pprint-str tv))))
  (let [tv (reify TaggedValue
             (edn-tag [this] 'frobble/biznar)
             (edn-value [this] [:foo :bar :baz]))]
    (is (= "#frobble/biznar\n[:foo :bar :baz]" (pprint-str tv)))))


(deftest default-canonize
  (testing "Unknown values"
    (let [usd (java.util.Currency/getInstance "USD")]
      (should-fail-when-strict usd)
      (is (re-seq #"#<java.util.Currency@[0-9a-f]+ USD>" (pprint-str usd))))))


(deftest metadata-printing
  (let [value ^:foo [:bar]]
    (binding [*print-meta* true]
      (is (= "^{:foo true}\n[:bar]" (pprint-str value)))
      (is (= "[:bar]" (pprint-str value {:print-meta false}))))))


(deftest colored-printing
  (let [value [nil 1.0 true "foo" :bar]
        bw-str (with-out-str (pprint value))
        colored-str (with-out-str (cprint value))
        thin-str (cprint-str value {:width 5})]
    (is (> (count colored-str) (count bw-str)))
    (is (not= colored-str thin-str))
    (is (= "123" (with-color (color-text :frobble "123"))))
    (is (= "#{:baz}" (pprint-str #{:baz})))
    (is (= (cprint-str :foo)
           (with-color (color-text :keyword ":foo"))))))

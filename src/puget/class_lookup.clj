(ns puget.class-lookup
  "Class-based lookup, algorithm based on
  https://github.com/Datomic/fressian/blob/32a833ba5adce9cd14365ff64b8b42a8569b7916/src/org/fressian/impl/InheritanceLookup.java")

(defn check-base-classes
  [m ^Class the-class]
  (loop [c (.getSuperclass the-class)]
    (when-not (= Object c)
      (or (get m c)
          (recur (.getSuperclass c))))))

(defn check-base-interfaces
  [m ^Class the-class]
  (let [possibles (java.util.HashMap.)]
    (loop [c the-class]
      (when-not (= Object c)
        (doseq [itf (.getInterfaces c)]
          (when-let [impl (get m itf)]
            (.put possibles itf impl)))
        (recur (.getSuperclass c))))
    (case (.size possibles)
      0 nil
      1 (first (vals possibles))
      (throw (ex-info "More than one interface match for class"
                      {:class the-class})))))

(defn lookup-impl
  [m the-class]
  (or (get m the-class)
      (check-base-classes m the-class)
      (check-base-interfaces m the-class)
      (get m Object)))

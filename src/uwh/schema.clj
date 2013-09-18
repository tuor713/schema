(ns uwh.schema
  "Schema validator via a templating approach. Mainly dictionaries and vectors are templates for the data structures they are meant to validate. Schemas and schema fragments are normal Clojure data structures and so lend themself to reuse in the normal Clojure fashion."
  (:refer-clojure :exclude [boolean double int keyword long vector-of])
  (:use [clojure.set :only [difference subset?]]
        [clojure.string :only [join]]))


;; High-level API

(defmulti validate 
  "Validates a given value (primitive or composed) against this validator"
  (fn [v _] (class v)))

(defrecord ValidationFailure [path reason]
  java.lang.Object
  (toString [_]
    (if (empty? path)
      (str "Failure: " reason)
      (str "Failure at " (pr-str (vec path)) ": " reason))))

(defmethod print-method ValidationFailure [o ^java.io.Writer out]
  (.write out (str "ValidationFailure{" o "}")))

(def success ::success)
(defn success? 
  "Checks whether the result of validation indicates success"
  [v] (= ::success v))
(defn failure? 
  "Checks whether the results of validation indicates failure"
  [v] (not= ::success v))

(defn fail 
  "Mark validation as failed. Supports to options:
a) giving just a reason string (assuming an zero-length path to the failing item)
b) giving a path and a reason string"
  ([reason] [(ValidationFailure. [] reason)])
  ([path reason] [(ValidationFailure. path reason)]))

(defn valid? 
  "Checks whether a data structure is valid with regard to the given schema. 
Equivalent to (comp success? validate)"
  [schema v]
  (success? (validate schema v)))


;; Implementation

(defn- equals [actual expected]
  (if (= expected actual)
    ::success
    (fail (str "Expected " (pr-str expected) " but found " (pr-str actual)))))

(defmethod validate Number
  [self v] (equals v self))

(defmethod validate String
  [self v] (equals v self))

(defmethod validate Boolean
  [self v] (equals v self))

(defmethod validate clojure.lang.Keyword
  [self v] (equals v self))

(defmethod validate nil
  [_ v]
  (if (nil? v)
      ::success
      (fail (str "Expected nil but found " (pr-str v)))))

(defmethod validate java.util.regex.Pattern
  [self v] 
  (if (and (string? v) (re-matches self v))
    ::success
    (fail (str "Expected value matching " (pr-str self) " but got " (pr-str v)))))

(defmethod validate clojure.lang.IPersistentMap
  [self v]
  (cond
   (not (map? v)) 
   (fail (str "Expected map but got " v))
   
   (not (subset? (set (keys v)) (set (keys self))))
   (fail (str "Got unsupported entries "
              (pr-str (select-keys v (difference (set (keys v)) (set (keys self)))))))

   :else
   (let [failures (mapcat
                   (fn [[key validator]]
                     (let [val (validate validator (get v key))]
                       (when-not (success? val)
                         (map #(vector key %) val))))
                   self)]
     (if (not (seq failures))
       ::success
       (mapcat (fn [[key failure]]
                 (fail (cons key (:path failure))
                       (:reason failure)))
               failures)))))

(defmethod validate clojure.lang.IPersistentVector
  [self v]
  (cond
   (not (vector? v)) 
   (fail (str "Expected vector but got " (pr-str v)))

   (not= (count self) (count v))
   (fail (str "Expected vector of length " (count self) " but got " (pr-str v)))

   :else 
   (let [failures (mapcat 
                   (fn [idx validator actual]
                     (let [val (validate validator actual)]
                       (when (failure? val)
                         (map #(vector idx %) val))))
                   (range) self v)]
     (if (seq failures)
       (mapcat 
        (fn [[idx failure]]
          (fail (cons idx (:path failure)) (:reason failure)))
        failures)
       ::success))))

(defmethod validate clojure.lang.IFn
  [self v] 
  (let [res (try (self v) (catch Exception e false))]
    (if (instance? Boolean res)
      (or (and res ::success) (fail (str "Value " (pr-str v) " does not satisfy constaint " self)))
      res)))

(prefer-method validate clojure.lang.IPersistentVector clojure.lang.IFn)
(prefer-method validate clojure.lang.IPersistentMap clojure.lang.IFn)


;; Validators

(defn optional 
  "Marks a given validator as optional, i.e. it succeeds for a nil value or a value that validates"
  [validator]
  #(or (nil? %) (validate validator %)))


(defn- type-check [p type-name]
  (fn [v] (or (and (p v) ::success)
              (fail (str "Expected type " type-name " but got " (pr-str v))))))

;; no primitive support yet
(def double (type-check #(instance? Double %) "double"))
(def int (type-check #(instance? Integer %) "int"))
(def long (type-check #(instance? Long %) "long"))
(def boolean (type-check #(instance? Boolean %) "boolean"))
(def ratio (type-check ratio? "ratio"))
(def number (type-check number? "number"))
(def string (type-check string? "string"))
(def keyword (type-check keyword? "keyword"))


(defn- primitive? [v]
  (or (string? v) (number? v) (keyword? v)
      (and (map? v)
           (every? (fn [[k v]] (and (primitive? k) (primitive? v))) v))
      (and (vector? v)
           (every? primitive? v))))

(defn choice [& validators]
  ;; for primitive choices we want to have a nicer display than lengthy validation failure messages
  (let [primitive (every? primitive? validators)]
    (fn [v] 
      (let [vs (map #(validate % v) validators)]
        (if (some success? vs)
          ::success
          (if primitive
            (fail (str "Value " (pr-str v) " did not match any allowed choices: " (pr-str (vec validators))))
            (fail (str "Value " (pr-str v) " failed all allowed choices: " (vec (apply concat vs))))))))))

(defn combine
  "Creates a combined template, that satisfies all the choices. Most useful in conjunction with open map templates"
  [& validators]
  (fn [v] 
    (let [failures (mapcat #(let [val (validate % v)] (when (failure? val) val)) validators)]
      (or (seq failures) ::success))))

(def | choice)
(def & combine)

(defn set-of 
  "Creates a schema for a set of value, all validated by the given validator"
  [validator]
  (fn [v] 
    (if (set? v)
      (let [failures (mapcat #(let [val (validate validator %)] (when (failure? val) val)) v)]
        (or (seq failures) ::success))
      (fail "Expected set but got " (pr-str v)))))

(defn map-of
  "Creates a schema for a map having an arbitrary number of key-value pairs satisfying the schema"
  [key-schema value-schema]
  (fn [v]
    (if (map? v)
      (let [failures 
            (mapcat
             (fn [[key val]]
               (let [kres (validate key-schema key)
                     vres (validate value-schema val)]
                 (cond
                  (and (success? kres) (success? vres)) nil
                  (success? kres) (map #(fail (cons key (:path %)) (:reason %)) vres)
                  (success? vres) kres
                  :else (concat kres (map #(fail (cons key (:path %)) (:reason %)) vres)))))
             v)]
        (or (seq failures) ::success))
      (fail (str "Expected map but got " (pr-str v))))))

(defn vector-of
  "Creates a schema for a repetition of elements passing the validator. Supports the following options:
- :count matches only a vector with the exact number of items
- :min matches only vectors having at least <min> items
- :max matches only vectors having at most <max> items"
  ([validator]
     (fn [v]
       (if (vector? v)
         (validate (vec (repeat (count v) validator)) v)
         (fail (str "Expected type vector but got " (pr-str v))))))
  ([validator & attrs]
     (let [h (apply hash-map attrs)]
       (fn [v]
         (cond
          (not (vector? v)) 
          (fail (str "Expected vector but got " (pr-str v)))

          (and (contains? h :count)
               (not= (:count h) (count v)))
          (fail (str "Expected vector of length " (:count h) " but got " (pr-str v)))

          (and (contains? h :min)
               (> (:min h) (count v)))
          (fail (str "Expected vector of mininum length " (:min h) " but got " (pr-str v)))

          (and (contains? h :max)
               (> (count v) (:max h)))
          (fail (str "Expected vector of maximum length " (:max h) " but got " (pr-str v)))

          :else (validate (vec (repeat (count v) validator)) v))))))

(defn open-map
  "Matches a parts of a hash map"
  [template]
  (fn [v] 
    (if (map? v) 
      (validate template (select-keys v (keys template)))
      (fail (str "Expected type map but got " (pr-str v))))))

(defn merged-map
  [& templates]
  (let [dupes (keys (remove #(= 1 (val %)) (frequencies (mapcat keys templates))))
        all-keys (set (mapcat keys templates))]
    (when (seq dupes)
      (throw (Exception. (str "Non-orthogonal templates: the keys " (vec dupes) 
                              " appear in more than one template."))))
    (fn [v]
      (cond
       (not (map? v))
       (fail (str "Expected type map but got " (pr-str v)))

       (not (subset? (set (keys v)) all-keys))
       (fail (str "Got unsupported entries: "
                  (pr-str (select-keys v (difference (set (keys v)) all-keys)))))

       :else
       (let [failures (mapcat (fn [t]
                                (let [val (validate t (select-keys v (keys t)))]
                                  (when-not (success? val) val)))
                              templates)]
         (or (seq failures) ::success))))))


;; end
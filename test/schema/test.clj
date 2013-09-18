(ns schema.test
  (:require [uwh.schema :as s])
  (:use clojure.test))

(deftest test-primitive-validation
  (testing "Booleans"
    (is (s/success? (s/validate true true)))
    (is (s/failure? (s/validate true false)))
    (is (s/success? (s/validate false false)))
    (is (s/failure? (s/validate false true))))

  (testing "Numbers"
    (is (s/success? (s/validate 3 3)))
    (is (s/success? (s/validate 3.1 3.1)))
    (is (s/failure? (s/validate 3 4))))

  (testing "Strings"
    (is (s/success? (s/validate "string" "string")))
    (is (s/failure? (s/validate "string" "stri")))
    (is (s/failure? (s/validate "string" "string and stuff"))))

  (testing "Keywords"
    (is (s/success? (s/validate :keyword :keyword)))
    (is (s/failure? (s/validate :keyword :other)))
    (is (s/success? (s/validate ::keyword :schema.test/keyword)))
    (is (s/failure? (s/validate ::keyword :uwh.schema/keyword))))

  (testing "Regexes"
    (is (s/success? (s/validate #"a*" "aaaa")))
    (is (s/failure? (s/validate #"a*" "aaab")))
    (is (s/failure? (s/validate #"a*" 1))))

  (testing "nil"
    (is (s/success? (s/validate nil nil)))
    (is (s/failure? (s/validate nil false)))))

(deftest test-primitive-functions
  (is (s/success? (s/validate s/number 3)))
  (is (s/failure? (s/validate s/number :hello)))
  
  (is (s/valid? s/ratio 2/3))
  (is (not (s/valid? s/ratio 0.5)))

  (is (s/success? (s/validate s/double 3.1)))
  ;; you have to be explicit because they are actually differenec, for example when using format
  ;; and when doing arithmetic
  (is (s/failure? (s/validate s/double 3)))

  (is (s/valid? even? 2))
  (is (not (s/valid? even? 3)))
  (is (not (s/valid? even? "string"))))

(deftest test-combinators
  (testing "choice"
    (is (s/success? (s/validate (s/choice :a :b :c) :a)))
    (is (s/success? (s/validate (s/choice :a :b :c) :c)))
    (is (s/failure? (s/validate (s/choice :a :b :c) :d)))
    (is (s/success? (s/validate (s/choice :a 1 "string") "string"))))

  (testing "optional"
    (is (s/success? (s/validate (s/optional 1) nil)))
    (is (s/success? (s/validate (s/optional 1) 1)))
    ;; if there is a value it has to validate
    (is (s/failure? (s/validate (s/optional 1) 2))))

  (testing "combine"
    (is (s/success? (s/validate (s/combine s/double 3.1) 3.1)))
    (is (s/failure? (s/validate (s/combine s/double 3.1) 3.2)))
    (is (s/success? (s/validate (s/combine (s/open-map {:a s/double})
                                           (s/open-map {:b s/double}))
                                {:a 1.0 :b 2.0})))
    (is (s/failure? (s/validate (s/combine (s/open-map {:a s/double})
                                           (s/open-map {:b s/double}))
                                {:a 1.0}))))

  (testing "combinations of combinators"
    (is (s/success? (s/validate (s/optional (s/choice :a :b :c)) :b)))))

(deftest test-map-template
  (testing "vanilla case"
    (is (s/success? (s/validate {:a 1 :b s/double :c (s/choice :a :b :c)}
                                {:a 1 :b 3.14 :c :a})))
    (is (s/failure? (s/validate {:a 1 :b s/double :c (s/choice :a :b :c)}
                         {:a 1 :b "string" :c :a}))))
  
  (testing "More values are s/failure? allowed"
    (is (s/failure? (s/validate {:a 1} {:a 1 :b 2}))))

  (testing "Optional keys can left out"
    (is (s/success? (s/validate {:a 1 :b (s/optional 1)}
                                {:a 1})))))

(deftest test-array-template
  (testing "vanilla case"
    (is (s/success? (s/validate [(s/choice :a :b) {:a s/double} s/string]
                                [:a {:a 3.1} "string"])))
    (is (s/failure? (s/validate [(s/choice :a :b) {:a s/double} s/string]
                         [:c {:a 3.1} "string"]))))
  
  (testing "exact length check"
    (is (s/failure? (s/validate [(s/choice :a :b) {:a s/double} s/string]
                         [:c {:a 3.1}])))
    (is (s/failure? (s/validate [(s/choice :a :b) {:a s/double} s/string]
                         [:c {:a 3.1} "string" "as/failure?her"])))))

(deftest test-vector-of-template
  (testing "endless repetition"
    (is (s/success? (s/validate (s/vector-of (s/choice :a :b :c))
                                [:a :c :c :b])))
    (is (s/failure? (s/validate (s/vector-of (s/choice :a :b :c))
                         "string")))
    (is (s/success? (s/validate (s/vector-of (s/choice :a :b :c)) 
                                [])))
    (is (s/failure? (s/validate (s/vector-of s/boolean)
                                ["string"]))))
  
  (testing "exact count option"
    (is (s/success? (s/validate (s/vector-of (s/choice :a :b :c)
                                          :count 3)
                                [:b :b :c])))
    
    (is (s/failure? (s/validate (s/vector-of (s/choice :a :b :c)
                                   :count 3)
                         [:b :b :c :a])))
    (is (s/failure? (s/validate (s/vector-of (s/choice :a :b :c)
                                   :count 3)
                         [:b :b])))

    (is (s/failure? (s/validate (s/vector-of s/boolean :count 3)
                                [true false "string"]))))

  (testing "min option"
    (is (s/success? (s/validate (s/vector-of :a :min 3) [:a :a :a])))
    (is (s/success? (s/validate (s/vector-of :a :min 3) [:a :a :a :a])))
    (is (s/failure? (s/validate (s/vector-of :a :min 3) [:a :a]))))
  
  (testing "max option"
    (is (s/success? (s/validate (s/vector-of :a :max 3) [:a :a :a])))
    (is (s/success? (s/validate (s/vector-of :a :max 3) [:a :a])))
    (is (s/failure? (s/validate (s/vector-of :a :max 3) [:a :a :a :a])))))

(deftest test-open-map
  (is (s/success? (s/validate (s/open-map {:a s/string :b s/keyword})
                              {:a "string" :b :c})))
  (is (s/success? (s/validate (s/open-map {:a s/string :b s/keyword})
                              {:a "string" :b :c :other :key})))
  (is (s/failure? (s/validate (s/open-map {:a s/string :b s/keyword})
                       {:a "string" :b "s/failure? keyword" :other :key})))
  (is (not (s/valid? (s/open-map {:a 1}) "string"))))


(deftest test-set-of
  (is (s/success? (s/validate (s/set-of s/keyword)
                              #{:a :b :c})))
  (is (s/failure? (s/validate (s/set-of s/keyword)
                       #{:a :b :c "d"}))))

(deftest test-map-of
  (is (s/valid? (s/map-of s/keyword s/number)
                {:a 1 :b 2 :c 3}))
  (is (not (s/valid? (s/map-of s/keyword s/number)
                     {"a" 1 :b 2 :c 3})))
  (is (not (s/valid? (s/map-of s/keyword s/number)
                     {:a 1 :b 2 :c "string"})))
  (is (not (s/valid? (s/map-of s/keyword s/number)
                     [:a 1 :b 2 :c 3])))
  )

(deftest test-merged-map
  (testing "Single map - base case"
    (is (s/valid? (s/merged-map {:a 1})
                  {:a 1}))
    (is (not (s/valid? (s/merged-map {:a 1})
                       {:a 2})))
    (is (not (s/valid? (s/merged-map {:a 1})
                       {:a 1 :b 1}))))
  
  (testing "No duplicate keys"
    (is (thrown? Exception (s/merged-map {:a 1} {:a "stringxo"}))))

  (testing "Multiple maps"
    (is (s/valid? (s/merged-map {:a 1} {:b 2} {:c 3})
                  {:a 1 :b 2 :c 3}))
    (is (not (s/valid? (s/merged-map {:a 1} {:b 2} {:c 3})
                       {:a 1 :b 2 :c 4})))
    (is (not (s/valid? (s/merged-map {:a 1} {:b 2} {:c 3})
                       {:a 1 :b 2})))
    (is (not (s/valid? (s/merged-map {:a 1} {:b 2} {:c 3})
                       {:a 1 :b 2 :c 3 :d 4})))))

(deftest test-short-forms
  (is (s/valid? (s/| 1 2 3) 2))
  (is (s/valid? (s/& s/number even?) 2))
  (is (not (s/valid? (s/& s/number even?) 3)))
  (is (not (s/valid? (s/& s/number even?) "and"))))

;; end 
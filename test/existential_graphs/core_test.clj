(ns existential-graphs.core-test
  (:require [existential-graphs.core :as sut]
            [clojure.test :refer :all]))

(count (take-while #(not (nil? %)) [nil]))

;; Depth starts at 2
;; Each cut it passes through adds one
(deftest path-depth-test
  (is (= (sut/path-depth [nil]) 2))
  (is (= (sut/path-depth [1 nil]) 3)))

(deftest is-double-cut?-test
  (is (= (sut/is-double-cut? [:SA [:cut [:cut]]] []) false))
  (is (= (sut/is-double-cut? [:SA [:cut [:cut]]] []) false))
  (is (= (sut/is-double-cut? [:SA [:cut [:cut]]] [1]) true))
  (is (= (sut/is-double-cut? [:SA [:cut [:A [:cut]]]] [1]) false))
  (is (= (sut/is-double-cut? [:SA [:A] [:cut [:cut]]] [1]) false))
  (is (= (sut/is-double-cut? [:SA [:A] [:cut [:cut]]] [2]) true))
  (is (= (sut/is-double-cut? [:SA [:cut [:cut [:A]]]] [1]) true))
  (is (= (sut/is-double-cut? [:SA [:cut [:cut [:A]]]] [1 2 3]) false))) ;; nonexistent path

(deftest exists-ancestral-copy?-test
  ;; (is false "WRITE TEST")
  )
(deftest have-common-parent?-test
  ;; (is false "WRITE TEST")
  )

(deftest insert-test
  (is (= (sut/insert [:SA] [:cut [:cut]] []) [:SA [:cut [:cut]]]))
  (is (= (sut/insert [:SA [:A]] [:B] [nil]) [:SA [:A] [:B]]))
  (is (= (sut/insert [:SA [:A]] [:B] [1 nil]) [:SA [:A [:B]]])))

(deftest replace-node-test
  (is (= (sut/replace-node [:A [:B]]  (fn [x] (vector :C x))   [1])
         [:A [:C [:B]]]))
  (is (= (sut/replace-node [:SA [:A]] (fn [x] [:cut [:cut x]]) [1])
       [:SA [:cut [:cut [:A]]]]))
  )

(deftest drop-vec-position-test
  (is (= (sut/drop-vec-position [1 2 3] 0) [2 3]))
  (is (= (sut/drop-vec-position [1 2 3] 1) [1 3])))

(deftest erase-test
  (is (= (sut/erase [:SA [:A]] [1]) [:SA])))

;; http://users.clas.ufl.edu/jzeman/graphicallogic/introduction.htm
(deftest insertion-test
  ;; Insertion "in odd"
  ;; Should work
  (is (= (sut/insertion [:SA [:cut]]
                        [:A]
                        [1 nil])
         [:SA [:cut [:A]]]) "Can Insert on even (Under an odd node)")
  ;; Should error
  (is (thrown? AssertionError (sut/insertion [:SA] [:A] [nil]))))

(deftest erasure-test
  (testing "Erasure At Even Depth"
    (is (= (sut/erasure [:SA [:A] [:cut [:B]]] [1]) [:SA [:cut [:B]]]))
    (is (= (sut/erasure [:SA [:A] [:cut [:B]]] [2]) [:SA [:A]])))
  (testing "Erasure At Odd Depth"
    (is (thrown? AssertionError
                 (sut/erasure [:SA [:A] [:cut [:B]]] [2 1])))))

(deftest double-cut-test
  ;; Examples
  (is (= (sut/double-cut [:SA] [nil]) [:SA [:cut [:cut]]]))
  (is (= (sut/double-cut [:SA [:A]] [nil])  [:SA [:A] [:cut [:cut]]]))
  (is (= (sut/double-cut [:SA [:A]] [1])  [:SA [:cut [:cut [:A]]]]))
  (is (= (sut/double-cut [:SA [:A]] [1 nil])  [:SA [:A [:cut [:cut]]]]))
  (is (= (sut/double-cut [:SA [:A [:B [:C]]]] [1 1])  [:SA [:A [:cut [:cut [:B [:C]]]]]])))

(deftest double-cut-erasure-test
  (is (= (sut/double-cut-erasure [:SA [:cut [:cut [:A] [:B]]]] [1])
         [:SA [:A] [:B]])))
(deftest iteration-test
  (is (= (sut/iteration [:SA [:A]] [1] [1 nil])
         [:SA [:A [:A]]])))

(deftest deiteration-test
  (is
   (= (sut/double-cut-erasure [:SA [:cut [:cut [:A] [:B]]]] [1])
      [:SA [:A] [:B]])))

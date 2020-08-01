(ns existential-graphs.core-test
  (:require [clojure.test :refer :all]
            [existential-graphs.core :refer :all]))

;; (deftest a-test
  ;; (testing "FIXME, I fail."
    ;; (is (= 0 1))))

(deftest add-children-test
  (is (= (add-child-l [:A [:B]] [:C]) [:A [:C] [:B]]))
  (is (= (add-child-r [:A [:B]] [:C]) [:A [:B] [:C]])))

(deftest children?-test
  (is (= (children? []) false))
  (is (= (children? [:A [:B]]) true))
  (is (= (children? [:A]) false)))





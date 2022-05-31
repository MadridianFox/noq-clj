(ns noq-clj.core-test
  (:require [clojure.test :refer :all]
            [noq-clj.core :refer :all]))

(deftest test-expr->str
  (is (= (expr->str (sym "a")) "a"))
  (is (= (expr->str (fun "f")) "f()"))
  (is (= (expr->str (fun "g" (fun "f" (sym "x")))) "g(f(x))")))


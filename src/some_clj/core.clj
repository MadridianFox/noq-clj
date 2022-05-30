(ns some-clj.core
  (:use [clojure.string :as string]))

(defn sym [name]
  {:type :symbol :name name})

(defn fun [name & args]
  {:type :functor :name name :args args})

(defn rule [pattern body]
  {:pattern pattern :body body})

(defn match [pattern expr res]
 (cond  
   (= (:type pattern) :symbol)
   (assoc res (:name pattern) expr)

   (and
     (= (:type pattern) :functor)
     (= (:type expr) :functor)
     (= (:name pattern) (:name expr))
     (= (count (:args pattern)) (count (:args expr))))
   (reduce 
     (fn [res [p e]] (match p e res)) 
     res 
     (partition 2 (interleave (:args pattern) (:args expr))))))

;(defn apply-rule [rule expr]
;  (let [bindings (match (:pattern rule) expr {})]


(defn expr->str [expr]
  (cond
    (= (:type expr) :symbol)
    (:name expr)
    (= (:type expr) :functor)
    (str (:name expr) "(" (string/join ", " (map expr->str (:args expr))) ")")))

(defn rule->str [rule]
  (str (expr->str (:pattern rule)) " = " (expr->str (:body rule))))

(comment
  (def rule-swap
    (rule
      (fun "swap" (fun "pair" (sym "a") (sym "b")))
      (fun "pair" (sym "b") (sym "a"))))
  (def expr
    (fun "pair"
      (fun "f" (sym "g"))
      (fun "k" (sym "z"))))
)

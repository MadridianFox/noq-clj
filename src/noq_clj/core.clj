(ns noq-clj.core
  (:use [clojure.string :refer [join]]
        [clojure.test :refer [deftest is testing]]))

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

(defn apply-rule-partial [bindings expr]
  (case (:type expr)
    :symbol (get bindings (:name expr))
    :functor (apply fun (cons 
                 (:name expr) 
                 (map #(apply-rule-partial bindings %) (:args expr))))))

(defn apply-rule [rule expr]
  (let [bindings (match (:pattern rule) expr {})]
    (if (nil? bindings)
      expr
      (apply-rule-partial bindings (:body rule)))))

(defn expr->str [expr]
  (case (:type expr)
    :symbol (:name expr)
    :functor (str (:name expr) 
               "(" (join ", " (map expr->str (:args expr))) ")")))

(defn rule->str [rule]
  (str (expr->str (:pattern rule)) " = " (expr->str (:body rule))))

(defn read-ident [code pos symbols]
  (loop [ident ""
         pos pos]
    (let [sym (nth code pos nil)]
      (if
        (or
          (contains? symbols sym)
          (= \space sym)
          (nil? sym))
        (list ident pos)
        (recur (str ident sym) (inc pos))))))

(defn str->tokens [code]
  (let [symbols {\( :open-paren \) :close-paren \, :comma}]
    (loop [pos 0
           tokens []]
      (cond
        (= \space (nth code pos nil))
          (recur (inc pos) tokens)
        (contains? symbols (nth code pos nil))
          (recur 
            (inc pos) 
            (conj tokens {:type (get symbols (nth code pos nil))}))
        (nil? (nth code pos nil))
          tokens
        :else
          (let [[ident pos] (read-ident code pos symbols)]
            (recur pos (conj tokens {:type :ident :name ident})))))))    


(comment
  (def rule-swap
    (rule
      (fun "swap" (fun "pair" (sym "a") (sym "b")))
      (fun "pair" (sym "b") (sym "a"))))
  (def expr
    (fun "swap"
      (fun "pair"
        (fun "f" (sym "g"))
        (fun "k" (sym "z")))))
  (def expr2
    (fun "f"
         (sym "a")))
)

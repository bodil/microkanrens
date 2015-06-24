(ns mk
  (:require [clojure.string :as string])
  (:refer-clojure :exclude [== conj disj cons list list? take]))

;;; First, we need a real cons cell implementation:

(defprotocol ICons
  (car [this])
  (cdr [this]))

(deftype Cons [a d]
  ICons
  (car [this] a)
  (cdr [this] d)

  Object
  (toString [this]
    (if (proper-list? this)
      (str "("
           (string/join " " (map #(if (nil? %) "nil" (.toString %)) this)) ")")
      (str "(" a " . " d ")")))

  clojure.lang.Seqable
  (seq [this] (seq-cons-list this)))

(defn cons [a d]
  (Cons. a d))

(defn proper-list? [cell]
  (or (nil? cell)
      (and (list? cell)
           (proper-list? (cdr cell)))))

(defn seq-cons-list [cell]
  (when-not (nil? cell)
    (clojure.core/cons (car cell) (cdr cell))))

(defn to-cons-list [l]
  (when (seq l)
    (cons (first l) (to-cons-list (rest l)))))

(defn list [& l]
  (to-cons-list l))

(defn list? [v]
  (instance? Cons v))



;;; µKanren begins here:

(defn lvar [c] #{c})
(defn lvar? [x] (set? x))

(defn walk [u s]
  (if (and (lvar? u) (contains? s u))
    (walk (s u) s)
    u))

(def empty-state [{} 0])

(def mzero nil)
(defn unit [sc] (cons sc mzero))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
      (and (lvar? u) (lvar? v) (= u v)) s

      (lvar? u) (assoc s u v)

      (lvar? v) (assoc s v u)

      (and (list? u) (list? v))
      (let [s (unify (car u) (car v) s)]
        (and s (unify (cdr u) (cdr v) s)))

      :else (and (= u v) s))))

(defn == [u v]
  (fn [[s c]]
    (let [s (unify u v s)]
      (if s (unit [s c]) mzero))))

(defn callfresh [f]
  (fn [[s c]]
    ((f (lvar c)) [s (+ c 1)])))

(defn mplus [$1 $2]
  (cond
    (fn? $1) (fn [] (mplus $2 ($1)))

    (list? $1)
    (cons (car $1) (mplus (cdr $1) $2))

    :else $2))

(defn bind [$ g]
  (cond
    (fn? $) (fn [] (bind ($) g))

    (list? $)
    (mplus (g (car $)) (bind (cdr $) g))

    :else mzero))

(defn disj [g1 g2] (fn [sc] (mplus (g1 sc) (g2 sc))))
(defn conj [g1 g2] (fn [sc] (bind (g1 sc) g2)))

(defn callgoal [g]
  (g empty-state))

(defn pull [$]
  (if (fn? $) (pull ($)) $))

(defn take [n $]
  (if (zero? n) nil
      (let [$ (pull $)]
        (if (nil? $) nil (cons (car $) (take (- n 1) (cdr $)))))))

(defn take-all [$]
  (let [$ (pull $)]
    (if (nil? $) nil (cons (car $) (take-all (cdr $))))))

(defmacro fresh [vars & body]
  `(callfresh
    (fn [~(first vars)]
      ~(if (seq (rest vars))
         `(fresh ~(rest vars) (conj+ ~@body))
         `(conj+ ~@body)))))

(defn zzz [g] (fn [sc] (fn [] (g sc))))

(defn walk* [v s]
  (let [v (walk v s)]
    (cond
      (lvar? v) v

      (list? v)
      (cons (walk* (car v) s) (walk* (cdr v) s))

      :else v)))

(defn reify-1st [[s c]]
  (walk* (lvar 0) s))

(defn run [n g]
  (map reify-1st (take n (callgoal g))))

(defn run* [g]
  (map reify-1st (take-all (callgoal g))))



(defmacro zzz [g]
  `(fn [sc#] (fn [] (~g sc#))))

(defmacro conj+ [g0 & gs]
  (if (seq gs)
    `(conj (zzz ~g0) (conj+ ~@gs))
    `(zzz ~g0)))

(defmacro disj+ [g0 & gs]
  (if (seq gs)
    `(disj (zzz ~g0) (disj+ ~@gs))
    `(zzz ~g0)))

(defmacro conde [& gs]
  `(disj+ ~@(map (fn [l] `(conj+ ~@l)) gs)))



;; Infinite fives and sixes goal:

(defn fives [x]
  (disj (== x 5) (zzz (fives x))))
(defn sixes [x]
  (disj (== x 6) (zzz (sixes x))))

(def fives-and-sixes (callfresh (fn [x] (disj (fives x) (sixes x)))))



;; And finally, appendo:

;; (defn appendo [l r out]
;;   (disj
;;    (conj (== l nil) (== r out))
;;    (fresh [a d res]
;;      (== (cons a d) l)
;;      (== (cons a res) out)
;;      (zzz (appendo d r res)))))

;; (defn appendo [l r out]
;;   (disj+
;;    (conj+ (== l nil) (== r out))
;;    (fresh [a d res]
;;      (== (cons a d) l)
;;      (== (cons a res) out)
;;      (appendo d r res))))

(defn appendo [l r out]
  (conde
   ((== nil l) (== r out))
   ((fresh [a d res]
      (== (cons a d) l)
      (== (cons a res) out)
      (appendo d r res)))))

;; (run* (fresh [q a b] (== q (list a b)) (appendo a b (list 1 2 3 4 5))))

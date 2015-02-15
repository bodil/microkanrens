(ns mk
  (:refer-clojure :exclude [== conj disj]))

(defn lvar [c] #{c})
(defn lvar? [x] (set? x))

(defn walk [u s]
  (let [pr (and (lvar? u) (s u))]
    (if pr (walk pr s) u)))

(def empty-state [{} 0])

(def mzero (list))
(defn unit [sc] (cons sc mzero))

(defn unify [u v s]
  (let [u (walk u s)
        v (walk v s)]
    (cond
      (and (lvar? u) (lvar? v) (= u v)) s

      (lvar? u) (assoc s u v)

      (lvar? v) (assoc s v u)

      (and (seq? u) (seq? v) (seq u) (seq v))
      (let [s (unify (first u) (first v) s)]
        (and s (unify (rest u) (rest v) s)))

      :else (and (= u v) s))))

(defn == [u v]
  (fn [[s c]]
    (let [s (unify u v s)]
      (if s (unit [s c]) mzero))))

(defn callfresh [f]
  (fn [[s c]]
    ((f (lvar c)) [s (+ c 1)])))

(defn mplus [$1 $2]
  (if (seq $1)
    (cons (first $1) (lazy-seq (mplus (rest $1) $2)))
    $2))

(defn bind [$ g]
  (if (seq $)
    (mplus (g (first $)) (bind (rest $) g))
    mzero))

(defn disj [g1 g2] (fn [sc] (mplus (g1 sc) (g2 sc))))
(defn conj [g1 g2] (fn [sc] (bind (g1 sc) g2)))

(defmacro fresh [vars body]
  `(callfresh
    (fn [~(first vars)]
      ~(if (seq (rest vars))
         `(fresh ~(rest vars) ~body)
         body))))

(defn run [n goal]
  (take n (goal empty-state)))

#_(run 10
  (conj
   (fresh [a] (== a 7))
   (fresh [b] (disj (== b 5) (== b 6)))))

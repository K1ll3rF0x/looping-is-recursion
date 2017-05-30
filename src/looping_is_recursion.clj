(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 (cond
                   (zero? base) 0
                   (zero? e) acc
                   :else (recur (* acc base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [fst s]
                 (if (empty? s)
                   fst
                   (recur (first s) (rest s))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [eq s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) eq
                   (or (empty? s1) (empty? s2)) false
                   :else (recur (= (first s1) (first s2))
                                (rest s1) (rest s2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [idx 0
         s a-seq]
    (cond (empty? s) nil
          (pred (first s)) idx
          :else (recur (inc idx) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         s a-seq]
    (if (empty? s)
      (/ acc n)
      (recur (+ acc (first s)) (inc n) (rest s)))))

;= TODO: Improve?
(defn parity [a-seq]
  (loop [acc #{}
         s a-seq]
    (if (empty? s)
      acc
      (recur (if (acc (first s))
               (disj acc (first s))
               (conj acc (first s)))
             (rest s)))))

;= from bottom to top
(defn fast-fibo [n]
  (loop [a 1
         b 0
         idx 0]
    (if (>= idx n)
      b
      (recur b (+ a b) (inc idx)))))

;= TODO: Get rid of mem, and use acc also for repetition checking?
(defn cut-at-repetition [a-seq]
  (loop [mem #{}
         acc []
         s a-seq]
    (let [fst (first s)]
      (if (or (mem fst) (empty? s))
        acc
        (recur (conj mem fst) (conj acc fst) (rest s))))))


(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (<= exp 0) acc
                     (recur (* acc base) (- exp 1))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [cur xs]
                 (if (empty? xs) cur
                     (recur (first xs) (rest xs))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (if (empty? seq1)
    (empty? seq2)
    (let [x1  (first seq1)
          x2  (first seq2)]
      (and (boolean  x1)
           (boolean x2)
           (== x1 x2)
           (recur (rest seq1) (rest seq2))))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq a-seq]
    (cond
      (empty? seq)       nil
      (pred (first seq)) acc
      :else
        (recur (+ acc 1) (rest seq)))))

(defn avg [a-seq]
  (loop [seq   a-seq
         sum   0
         count 0]
    (if (empty? seq)
      (/ sum count)
      (recur (rest seq)
             (+ sum (first seq))
             (+ count 1)))))

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

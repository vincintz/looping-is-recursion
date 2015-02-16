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
  (loop [sum   0
         count 0
         seq   a-seq]
    (if (empty? seq)
      (/ sum count)
      (recur (+ sum (first seq))
             (+ count 1)
             (rest seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [pset #{}
         seq  a-seq]
    (if (empty? seq)
      pset
      (recur (toggle pset (first seq))
             (rest seq)))))

(defn fast-fibo [n]
  (loop [cur n
         fn1 1
         fn0 0]
    (cond
      (zero? cur) 0
      (== cur 1)  (+ fn1 fn0)
      :else  (recur (dec cur)
                    fn0
                    (+ fn1 fn0)))))

(defn cut-at-repetition [a-seq]
  (loop [cur a-seq
         acc []]
    (if (empty? cur)
      acc
      (let [x (first cur)]
        (if (contains? (set acc) x)
          acc
          (recur (rest cur) (conj acc x)))))))

(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [ibase iexp acc]
                 (if (zero? iexp)
                   acc
                   (recur ibase (dec iexp) (* ibase acc))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [coll last-item]
                 (if (empty? coll)
                   last-item
                   (recur (rest coll) (first coll))))]
    (helper a-seq nil)))

(defn seq= [seq1 seq2]
  (let [helper (fn helper [seqa seqb]
          (cond 
            (every? empty? [seqa seqb]) true
            (or (empty? seqa) (empty? seqb)) false
            (not (= (first seqa) (first seqb))) false
            :else (recur (rest seqa) (rest seqb))
            ))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ia-seq a-seq 
         index 0]
    (cond 
      (empty? ia-seq) nil
      (pred (first ia-seq)) index
      :else (recur (rest ia-seq) (inc index)))))

(defn avg [a-seq]
  (loop [coll a-seq
         total 0
         item-count 0]
    (if (empty? coll)
      (/ total item-count)
      (recur (rest coll) (+ total (first coll)) (inc item-count)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
          (if (contains? a-set elem)
            (disj a-set elem)
            (conj a-set elem)))]
    (loop [i-seq a-seq
           result-set #{}]
      (if (empty? i-seq)
        result-set
        (recur (rest i-seq) (toggle result-set (first i-seq)))))))

(defn fast-fibo [n]
  (loop [first-n 0 
         second-n 1
         index 0]
    (if (= n index)
      first-n
      (recur second-n (+ first-n second-n) (inc index)))))

(defn cut-at-repetition [a-seq]
  (loop [coll a-seq
         acc []
         a-set #{}]
    (cond 
      (empty? coll) acc
      (contains? a-set (first coll)) acc
      :else (recur (rest coll) (conj acc (first coll)) (conj a-set (first coll))))))
